#' Add ADR column(s) to a dataset (tidyverse syntax)
#'
#' This function creates adr columns using dplyr::mutate.
#'
#' The function was initially written to fight against the horrible double free or corruption error occurring on the CHU server (with the french accent, please). See details on a word document on my CHU pc. Low-level term codes are the preferred level of requesting in the Vigibase extract case level since it captures all possible codes for a given Preferred Term. Standardized names for demo and adr cols are assumed (e.g. `UMCReportId`)

#' @param .data The dataset used to identify individual reports (usually, it is `demo`)
#' @param a_code A named list of low level terms codes (llt_codes).
#' @param a_names Names for adr columns (must be the same length as adr_list), default to `names(adr_list)`

#' @param adr_data A data.frame containing the adr data (usually, it is `adr`)
#' @export
#' @importFrom dplyr %>%
#' @keywords adr
#' @examples
#' # create adr_colitis, adr_embolism and adr_pneumonitis columns in demo
#'
#' # be careful, this example may overwrite your own demo dataset
#' demo <- demo_
#'
#' a_pt_sel <- ex_$pt_sel
#'
#' meddra <- ex_$meddra
#'
#' adr <- adr_
#'
#' a_llt <-
#'   get_llt_soc(
#'   term_sel = a_pt_sel,
#'   term_level = "pt",
#'   meddra = meddra
#'   )
#'
#' demo <-
#'   demo %>%
#'     add_adr(
#'       a_code = a_llt,
#'       adr_data = adr
#'     )
#'
#' demo %>%
#'   check_dm(names(a_pt_sel))

add_adr <-
  function(.data,
           a_code,
           a_names = names(a_code),

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

    e_l <- purrr::map(a_code,
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
