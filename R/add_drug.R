#' Add DRUG column(s) to a dataset (tidyverse syntax)
#'
#' This function creates drug columns using `dplyr::mutate`.
#'
#'
#' d_code is a named list containing drug codes. Either medicinalprod_ids (e.g., from `tb_custom`), or drug record numbers (e.g., from `get_drecno`). Default method is to DrecNos.
#' Drugs can be reported according to one of three reputation bases: suspect, concomitant or interacting in the occurrence of the adverse drug reaction. You may want to study only reports with a specific reputation basis.
#' You can add drug identification to a `demo`, a `link`, or and `adr` dataset. Remember to set to the `data_type` argument to the appropriate value.
#'
#' @param .data The dataset used to identify individual reports (usually, it is `demo`)
#' @param d_code A named list of drug codes (DrecNos or MPI). See Details.
#' @param d_names A character vector. Names for drug columns (must be the same length as d_code), default to `names(d_code)`
#' @param method A character string. The type of drug code (DrecNo or MedicinalProd_Id). See details.
#' @param repbasis Suspect, interacting and/or concomitant. Type initial of those you wish to select (s for suspect, c for concomitant and i for interacting ; default to all)
#' @param drug_data A data.frame containing the drug data (usually, it is `drug`)
#' @param data_type A character string. The type of data to add columns to. Either `demo` or `link` (default to `demo`)
#' @keywords drug
#' @export
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @examples
#' # create a nivolumab column in demo_
#'
#' d_sel_names <- rlang::list2(nivolumab = "nivolumab")
#'
#' d_drecno <- get_drecno(d_sel_names,
#'                         mp_short = mp_short_)
#'
#' demo_ <-
#'   add_drug(
#'     .data = demo_,
#'     d_code = d_drecno,
#'     method = "DrecNo",
#'     repbasis = "sci",
#'     drug_data = drug_,
#'     data_type = c("demo")
#'   )
#'
#' # remember to assign the result to your actual demo dataset
#'
#' # do you want to work only with cases where nivolumab was a "suspected" drug?
#' # change argument repbasis to "s"
#'
#' demo_ <-
#'   add_drug(
#'     .data = demo_,
#'     d_code = d_drecno,
#'     d_names = "nivolumab_suspected",
#'     method = "DrecNo",
#'     repbasis = "s",
#'     drug_data = drug_,
#'     data_type = c("demo")
#'   )
#'
#' check_dm(demo_, cols = c("nivolumab", "nivolumab_suspected"))

add_drug <-
  function(.data,
           d_code,
           d_names = names(d_code),
           repbasis = "sci",
           method = c("DrecNo", "MedicinalProd_Id"),
           drug_data,
           data_type = c("demo", "link", "adr")
  )
  {
    method <- match.arg(method)

    data_type <- match.arg(data_type)

    # use duplicates in UMCReportId to identify a link dataset versus a demo dataset.
    # and check that data_type is set correctly
    if(data_type == "demo" &&
       any(c("Drug_Id", "Adr_Id") %in% names(.data))){
      stop("The dataset has Drug_Id or Adr_Id columns (like a `link` dataset). Yet data_type is set to `demo`. Please set data_type to `link` or use a `demo` dataset")
    } else if(data_type == "link" &&
              !all(c("Drug_Id", "Adr_Id") %in% names(.data))){
      stop("The dataset does not have Drug_Id and Adr_Id columns, (as a `link` dataset would). Yet data_type is set to `link`. Please set data_type to `demo` or use a `link` dataset")
    } else if(data_type == "adr" &&
              !all(c("Adr_Id", "MedDRA_Id", "Outcome") %in% names(.data))){
      stop("The dataset does not have Adr_Id, MedDRA_Id, and/or Outcome columns, (as an `adr` dataset would). Yet data_type is set to `adr`. Please set data_type accordingly.")
    }

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
        rlang::expr(.data$Basis %in% !!basis_sel)
      }

    # core function

    add_single_drug_demo <- function(drug_code,
                                     UMCReportId = {{ UMCReportId }}) {

      # find matching UMCReportId in drug for this method values and this repbasis

      umc_id <-
        rlang::eval_tidy(
          rlang::quo({
            drug_data %>%
              dplyr::filter(
                .data[[!!method]] %in% drug_code &
                  !!basis_expr
              ) %>%
              dplyr::pull(UMCReportId)
          })
        )

      rlang::eval_tidy(
        rlang::quo({
          # create a vector length nrow(.data) based on whether UMCReportId match with above list
          ifelse(
            UMCReportId %in% umc_id,
            1, 0)
        }),
        data = .data)
    }

    # core function link

    add_single_drug_link <- function(drug_code,
                                     Drug_Id = {{ Drug_Id }}) {

      # find matching Drug_Id in `drug_data` for this method values and this repbasis
      drug_id <-
        rlang::eval_tidy(
          rlang::quo({
            drug_data %>%
              dplyr::filter(
                .data[[!!method]] %in% drug_code &
                  !!basis_expr
              ) %>%
              dplyr::pull(Drug_Id)
          })
        )
      rlang::eval_tidy(
        rlang::quo({


          # create a vector length nrow(.data) based on whether Drug_Id match with above list
          ifelse(
            Drug_Id %in% .env$drug_id,
            1, 0)
        }),
        data = .data)
    }

    # select appropriate core function according to data type

    add_single_drug <-
      switch (data_type,
        demo = add_single_drug_demo,
        adr  = add_single_drug_demo,
        link = add_single_drug_link
      )

    # Step 2: vectorize over drug_names and prepare call

     e_l <- purrr::map(d_code,
                       function(x) {
                         rlang::call2(
                           rlang::quo(add_single_drug),
                           rlang::quo(x)
                         )
                         }
     )

    names(e_l) <- d_names

    # Step 3: apply the functions in .data

    .data %>%
      dplyr::mutate(!!!e_l)

  }
