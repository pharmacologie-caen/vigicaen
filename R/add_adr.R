#' Add ADR column(s) to a dataset (tidyverse syntax)
#'
#' This function creates adr columns using `dplyr::mutate`.
#'
#' Low-level term codes are the preferred level of requesting in the Vigibase extract case level since it captures all possible codes for a given Preferred Term. Standardized names for demo and adr cols are assumed (e.g. `UMCReportId`).
#' You can add adr identification to a `demo`, a `link`, or and `adr` dataset. Remember to set to the `data_type` argument to the appropriate value.
#'
#' @param .data The dataset used to identify individual reports (usually, it is `demo`)
#' @param a_code A named list of low level terms codes (llt_codes).
#' @param a_names A character vector. Names for adr columns (must be the same length as adr_list), default to `names(adr_list)`
#' @param adr_data A data.frame containing the adr data (usually, it is `adr`)
#' @param data_type A character string. The type of data to add columns to. Either `demo` or `link` (default to `demo`)
#' @export
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @keywords adr
#' @examples
#' # create adr_colitis, adr_embolism and adr_pneumonitis columns in demo
#'
#' # be careful, this example may overwrite your own demo dataset
#' demo <- demo_
#'
#' a_pt_sel <- ex_$pt_sel
#'
#'
#' adr <- adr_
#'
#' a_llt <-
#'   get_llt_soc(
#'   term_sel = a_pt_sel,
#'   term_level = "pt",
#'   meddra = meddra_
#'   )
#'
#' demo <-
#'   demo |>
#'     add_adr(
#'       a_code = a_llt,
#'       adr_data = adr
#'     )
#'
#' demo |>
#'   check_dm(names(a_pt_sel))

add_adr <-
  function(.data,
           a_code,
           a_names = names(a_code),

           adr_data,

           data_type = c("demo", "link", "adr")
           ){

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


    # Step 1: core function for demo data_type, ifelse on UMCReportId

    add_single_adr_demo <- function(adr_code,
                                    MedDRA_Id = {{ MedDRA_Id }},
                                    UMCReportId = {{ UMCReportId }}) {
      # promise in adr_data

      umc_ic <-
          dplyr::filter(adr_data, MedDRA_Id %in% adr_code)[["UMCReportId"]]

      rlang::eval_tidy(rlang::quo(
        ifelse(UMCReportId %in%
                 umc_ic,
               1,
               0)
        ),
        data = .data)
      # evaluated in .data
    }

    add_single_adr_link <- function(adr_code,
                                    MedDRA_Id = {{ MedDRA_Id }},
                                    Adr_Id = {{ Adr_Id }}) {

      adr_id <-
        dplyr::filter(adr_data, MedDRA_Id %in% adr_code)[["Adr_Id"]]

      rlang::eval_tidy(rlang::quo(
        ifelse(Adr_Id %in%
                 .env$adr_id,
               1,
               0)
      ),
      data = .data)
      # evaluated in .data
    }

    # select appropriate core function according to data type

    add_single_adr <-
      switch (data_type,
              demo = add_single_adr_demo,
              adr  = add_single_adr_link,
              link = add_single_adr_link
      )

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

    .data |>
      dplyr::mutate(!!!e_l)
  }
