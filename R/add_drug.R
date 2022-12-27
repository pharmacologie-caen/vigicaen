#' Add DRUG column(s) to a dataset (tidyverse syntax)
#'
#' This function creates drug columns using dplyr::mutate.
#'
#' d_code is a named list containing drug codes. Either medicinalprod_ids (e.g., from `tb_custom`), or drug record numbers (e.g., from `get_drecno`). Default method is to DrecNos.
#' Drugs can be reported according to one of three reputation bases: suspect, concomitant or interacting in the occurrence of the adverse drug reaction. You may want to study only reports with a specific reputation basis.
#' Column names will be automatically tolower-ed, so you may wish to do this step explicitly in your workflow prior to using this function.
#'
#' @param .data The dataset used to identify individual reports (usually, it is `demo`)
#' @param d_code A named list of drug codes (DrecNos or MPI). See Details.
#' @param method A character string. The type of drug code (DrecNo or MedicinalProd_Id). See details.
#' @param repbasis Suspect, interacting and/or concomitant. Type initial of those you wish to select (default to all)
#' @param drug_data A data.frame containing the drug data (usually, it is `drug`)
#' @keywords drug
#' @export
#' @importFrom dplyr %>%
#' @examples
#' # create a nivolumab column in demo
#'
#' # You need to use standard names for the tables
#' demo <- demo_
#' drug <- drug_
#'
#' d_sel_names <- rlang::list2(nivolumab = "nivolumab")
#'
#' d_drecno <- get_drecno(d_sel_names,
#'                         mp_short = ex_$mp_short)
#'
#' demo <-
#'   demo %>%
#'     add_drug(
#'       exposure_df = ex_$d_drecno,
#'       drug_names = "nivolumab",
#'       # use lower case names
#'       method = "DrecNo",
#'       repbasis = "sci",
#'       drug_data = drug
#'     )

#' demo_ %>%
#'   add_drug(
#'     d_code = d_drecno,
#'     method = "DrecNo",
#'     repbasis = "sci",
#'     drug_data = drug
#'   )

add_drug <-
  function(.data,
           d_code,
           # exposure_df,
           # drug_names = unique(exposure_df[["drug"]]),
           repbasis = "sci",
           method = c("DrecNo", "MedicinalProd_Id"),
           drug_data
  )
  {
    method <- match.arg(method)
    method_col <- rlang::sym(method)
    # exposure_df <- rlang::enexpr(exposure_df)

    col_names <- names(d_code)

    # drug_names <- tolower(drug_names)

    drug_data <- enexpr(drug_data)

    basis_sel <-
      c(
        if(grepl("s", repbasis)){ 1 },
        # subsidiary_files / Repbasis_Lx
        if(grepl("c", repbasis)){ 2 },
        if(grepl("i", repbasis)){ 3 }
      )

    basis_expr <-
      if(repbasis == "sci"){
        TRUE
      } else {
        rlang::expr(Basis %in% !!basis_sel)
      }

    # core function

    add_single_drug <- function(drug_code) {

      rlang::eval_tidy(
        rlang::quo({
          # find method (drecno/mpi) values for drug_name
          # method_val <-
          #   !!exposure_df %>%
          #   dplyr::filter(
          #     drug == !!drug_name) %>%
          #   dplyr::pull(!!method_col)

          # find matching UMCReportId in drug for this method values and this repbasis
          umc_id <-
            !!drug_data %>%
            dplyr::filter(
              !!method_col %in% drug_code &
                !!basis_expr
            ) %>%
            dplyr::pull(UMCReportId)

          # create a vector length nrow(.data) based on whether UMCReportId match with above list
          ifelse(
            UMCReportId %in% umc_id,
            1, 0)
        }),
        data = .data)
    }

    # Step 2: vectorize over drug_names and prepare call

    e_l <- purrr::map(d_code,
                      function(x) {
                        rlang::call2(
                          rlang::quo(add_single_drug),
                          rlang::quo(x)
                        )
                      }
    )

    names(e_l) <- col_names

    # Step 3: apply the functions in .data

    .data %>%
      dplyr::mutate(!!!e_l)

  }
