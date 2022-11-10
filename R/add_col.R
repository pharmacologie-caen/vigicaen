
#' Add ADR column(s) to a dataset (tidyverse syntax)
#'
#' This function creates adr columns using dplyr::mutate.
#'
#' The function was initially written to fight against the horrible double free or corruption error occurring on the CHU server (with the french accent, please). See details on a word document on my CHU pc. Low-level term codes are the preferred level of requesting in the Vigibase extract case level since it captures all possible codes for a given Preferred Term. Standardized names for demo and adr cols are assumed (e.g. `UMCReportId`)

#' @param .data The dataset used to identify individual reports (usually, it is `demo`)
#' @param adr_list A named list of low level terms codes (llt_codes).
#' @param a_names Names for adr columns (must be the same length as adr_list), default to `names(adr_list)`

#' @param adr_data A data.frame containing the adr data (usually, it is `adr`)
#' @importFrom dplyr %>%
#' @keywords adr
#' @examples
#' # create adr_colitis, adr_embolism and adr_pneumonitis columns in demo
#'
#' # be careful, this example may overwrite your own demo dataset
#' demo <- demo_
#'
#' demo <-
#'   demo %>%
#'     add_adr(
#'       adr_list = ex_$adr_list,
#'       a_names = paste0("adr_", names(ex_$adr_list)),
#'       adr = adr_
#'     )


add_adr <-
  function(.data,
           adr_list,
           a_names = names(adr_list),

           adr_data){

    adr_data <- rlang::enquo(adr_data)

    # Step 1: core function, ifelse on UMCReportId

    add_single_adr <- function(adr_code) {
      rlang::eval_tidy(rlang::quo(
        ifelse(UMCReportId %in%
                 dplyr::filter(!!adr_data, MedDRA_Id %in% adr_code)[["UMCReportId"]],
               1,
               0)
        ),
        data = .data)
      # evaluated in .data
    }

    # Step 2: build calls to core function for each adr

    e_l <- purrr::map(adr_list,
                      function(x) {
                        rlang::call2(
                          rlang::quo(add_single_adr),
                          rlang::quo(x)
                          )
                        }
                      )

    names(e_l) <- a_names

    # Step 3: apply the functions in .data

    .data %>%
      dplyr::mutate(!!!e_l)
  }

#' Check binary variables
#'
#' After adding drugs and/or adrs/atc classes to a dataset, check the number of reports with the feature.
#'
#' It is a simple wrapper around `dplyr::summarise`. Be careful not to supply factors with > 2 levels or continuous outcome (the function does NOT have a checker for this, so that it is faster).
#'
#' @param .data A data.frame to be checked
#' @param cols A character vector, name of columns to look at (usually will be `d_names`, `a_names`)
#' @keywords check_dm
#' @export
#' @importFrom dplyr %>%
#' @examples
#' # first create some new variables
#'
#' demo <- demo_
#'
#' demo <-
#'   demo %>%
#'     add_adr(
#'       adr_list = ex_$adr_list,
#'       a_names = paste0("adr_", names(ex_$adr_list)),
#'       adr = adr_
#'     )
#'
#'  # then check the number of reports with each feature
#'
#' demo %>%
#'   check_dm(paste0("adr_", names(ex_$adr_list)))

check_dm <-
  function(.data,
           cols){

    .data %>%
      dplyr::summarise(
        dplyr::across(dplyr::all_of(cols),
                      ~ sum(.x)
        )
      ) %>%
      t()
  }

#' Add DRUG column(s) to a dataset (tidyverse syntax)
#'
#' This function creates drug columns using dplyr::mutate.
#'
#' Exposure can be generated from either `tb_custom`,  with 'drug' and 'MedicinalProd_Id' columns, or from `find_drecno` with 'drug' and 'DrecNo' columns. Choose the appropriate method for the function (defaults to DrecNo).
#' Drugs can be reported according to one of three reputation bases: suspect, concomitant or interacting in the occurrence of the adverse drug reaction. You may want to study only reports with a specific reputation basis.
#' Column names will be automatically tolower-ed, so you may wish to do this step explicitly in your workflow prior to using this function.
#'
#' @param .data The dataset used to identify individual reports (usually, it is `demo`)
#' @param exposure_df A data.table of exposure.
#' @param drug_names A character vector of drug names. Default to all the names found in `exposure_df[["drug"]]`
#' @param method A character string. The drug identifier to be used, depending on how you created `exposure_df`, see details.
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

add_drug <-
  function(.data,
           exposure_df,
           drug_names = unique(exposure_df[["drug"]]),
           repbasis = "sci",
           method = c("DrecNo", "MedicinalProd_Id"),
           drug_data
           )
  {
    method <- match.arg(method)
    method_col <- rlang::sym(method)
    exposure_df <- rlang::enexpr(exposure_df)

    drug_names <- tolower(drug_names)

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

    add_single_drug <- function(drug_name) {

      eval_tidy(
        rlang::quo({
          # find method (drecno/mpi) values for drug_name
          method_val <-
            !!exposure_df %>%
            dplyr::filter(
              drug == !!drug_name) %>%
            dplyr::pull(!!method_col)

          # find matching UMCReportId in drug for this method values and this repbasis
          umc_id <-
            !!drug_data %>%
            dplyr::filter(
              !!method_col %in% method_val &
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

    e_l <- purrr::map(drug_names,
                      function(x) {
                        rlang::call2(
                          rlang::quo(add_single_drug),
                          rlang::quo(x)
                        )
                      }
    )

    names(e_l) <- drug_names

    # Step 3: apply the functions in .data

    .data %>%
      dplyr::mutate(!!!e_l)

  }

#' Add ATC columns to a dataset (tidyverse syntax)
#'
#' This function creates an expression that must be evaluated in `j` in a data.table to produce new atc columns.
#'
#' IMPORTANT: At the moment, all `create_x_exp` functions assume default names for data.tables: drug, adr and thg.
#' Vigilyze style means all conditioning of drugs will be retrieved after requesting an ATC class, even if a specific conditioning is not present in the ATC class. This is the default behavior in vigilyze.
#'
#' @param .data The dataset used to identify individual reports (usually, it is `demo`)
#' @param atc_codes A character vector. ATC codes
#' @param vigilyze A logical. Should ATC classes be retrieved using the vigilyze style? See details
#' @param mp_short A modified MP data.table. See \code{\link{ex_}}
#' @param thg_data A data.frame. Correspondence between ATC codes and MedicinalProd_Id (usually, it is `thg`)
#' @keywords atc
#' @export
#' @importFrom dplyr %>%
#' @examples
#' atc_codes <- c("L03", "C")
#'
#' thg <- thg_ # table dans laquelle sont lus les codes ATC
#' demo <- demo_
#' drug <- drug_
#'
#' atc_exp <-
#'   create_atc_exp(atc_codes, mp_short = ex_$mp_short)
#'
#' demo[, c(atc_codes) := eval(atc_exp)]
#' demo[, .N, by = list(L03, C)]

add_atc <-
  function(.data,
           atc_codes,
           vigilyze = TRUE,
           mp_short,
           thg_data)
  {
    drug_expression <-
      if (vigilyze) {
        rlang::expr(
          DrecNo %in%
            charles::find_drecno(
              thg[substr(ATC.code, start = 1, stop = length_code) == atc_code,
                  as.integer(MedicinalProd_Id)],
              mp_short,
              allow_combination = FALSE,
              method = "mpi_list"
            )[["DrecNo"]])
          } else {
        rlang::expr(MedicinalProd_Id %in%
                      thg[substr(ATC.code, start = 1, stop = length_code) == atc_code,
                          as.integer(MedicinalProd_Id)])
        }

    atc_exp <- function(atc_code, env, drug_expression) {
      drug_expression <- rlang::enexpr(drug_expression)
      length_code <- nchar(atc_code)
      eval(rlang::expr(data.table::fifelse(UMCReportId %in%
                         drug[!!drug_expression, unique(UMCReportId)],
                       1, 0)), envir = env)
    }

    e_l <- lapply(atc_codes, function(x) {
      rlang::call2(rlang::expr(!!atc_exp), x, rlang::expr(.SD), drug_expression)
    })

    var_expr <- rlang::call2(rlang::expr(list),
                      !!!e_l)

    var_expr
  }
