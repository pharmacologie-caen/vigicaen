#' Internal dataset checkers
#'
#' Internal helpers to check argument values.
#'
#' @param .data
#' @param calling_arg A character string.
#'   The name of the argument in the calling function (e.g. ".data" for "demo" in
#'   `add_adr()`, etc.)
#' @param arg
#' @param drug_arg
#' @param call
#'
#' @return An error if the dataset is invalid. Nothing in other cases
#'
#' @examples

check_data_drug <-
  function(.data,
           calling_arg = ".data",
           arg = rlang::caller_arg(.data),
           call = rlang::caller_env()){
    if (!all(c("DrecNo",
               "MedicinalProd_Id",
               "UMCReportId",
               "Drug_Id") %in% names(.data))) {
      cli::cli_abort(
        cli::cli_bullets(c(
          "x" = "{.arg {arg}} is not a {.arg drug} table.",
          ">" = "Supply a {.arg drug} table to {.arg {calling_arg}}. See ?drug_."
        )),
        call = call
      )
    }
  }

check_data_mp <-
  function(mp_data,
           calling_arg,
           arg = rlang::caller_arg(mp_data),
           call = rlang::caller_env()){
    if (!all(c("DrecNo", "drug_name_t") %in% names(mp_data))) {
      cli::cli_abort(
        cli::cli_bullets(c(
          "x" = "{.arg {arg}} is invalid.",
          "!" = "Either {.arg DrecNo} or {.arg drug_name_t} columns are missing.",
          ">" = "Supply an {.arg mp} table to {.arg {calling_arg}}. See ?mp_."
        )),
        call = call
      )
    }
  }
