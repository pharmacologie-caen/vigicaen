#' Internal dataset type query
#'
#' Find the matching data type, according to column names.
#'
#' @param .data The dataset to check.
#' @param calling_arg A character string.
#'   The name of the argument in the calling function (e.g. ".data" for "demo" in
#'   `add_adr()`, etc.)
#' @param arg Helper to format the error message.
#' @param call Helper to format the error message.
#'
#' @returns A character vector with corresponding data type. An error in other cases
#' @noRd
#' @examples
#'
#' drug_valid <- data.frame(
#'   DrecNo = 1, UMCReportId = 1, MedicinalProd_Id = 1, Drug_Id = 1)
#'
#'  vigicaen:::query_data_type(drug_valid, ".data")

NULL

query_data_type <-
  function(.data,
           calling_arg,
           arg = rlang::caller_arg(.data),
           call = rlang::caller_env()){

    guessed_type <-
      dplyr::case_when(
        all(c("UMCReportId", "DateDatabase", "Region", "Type") %in%
              names(.data))
        ~ "demo",
        all(c("DrecNo",
              "MedicinalProd_Id",
              "UMCReportId",
              "Drug_Id") %in% names(.data))
        ~ "drug",
        all(c("UMCReportId", "Adr_Id", "MedDRA_Id", "Outcome") %in%
              names(.data))
        ~ "adr",
        all(c("Drug_Id", "Adr_Id", "Dechallenge1", "TimeToOnsetMin") %in%
              names(.data))
        ~ "link",
        all(c("Drug_Id", "Indication") %in%
              names(.data))
        ~ "ind",
        TRUE ~ "unknown"
      )


    if (guessed_type == "unknown") {
      cli::cli_abort(
        c(
          "{.arg {arg}} must match an expected data type.",
          "!" = "Column names are not those of expected data types.",
          ">" = "Supported types are demo, drug, adr, ind, and link. See ?demo_."
        ),
        call = call
      )
    } else {
      cli::cli_inform(
        c(
          "i" = "{.arg {calling_arg}} detected as {.arg {guessed_type}} table."
        ),
        call = call
      )
    }

    return(guessed_type)
  }
