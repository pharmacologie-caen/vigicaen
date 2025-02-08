#' Internal dataset type checkers
#'
#' Internal helpers to check argument values.
#'
#' @param .data The dataset to check.
#' @param arg Helper to format the error message.
#' @param call Helper to format the error message.
#'
#' @returns An error if the dataset is invalid. Nothing in other cases
#'
#' @examples
#'
#' drug_valid <- data.frame(
#'   DrecNo = 1, UMCReportId = 1, MedicinalProd_Id = 1, Drug_Id = 1)
#'
#'  vigicaen:::check_data_drug(drug_valid, ".data")
#'
#' @name data_checkers

NULL

#' @describeIn data_checkers adr data checker

check_data_adr <-
  function(.data,
           arg = rlang::caller_arg(.data),
           call = rlang::caller_env()){

    adr_cols <-
      c("UMCReportId",
        "Adr_Id",
        "MedDRA_Id",
        "Outcome")

    if (!all(adr_cols %in% names(.data))) {

      missing_cols <-
        adr_cols[!adr_cols %in% names(.data)]

      cli::cli_abort(
        c(
          "{.arg {arg}} is not an {.arg adr} table.",
          "x" = "Missing columns: {missing_cols}",
          ">" = "Supply an {.arg adr} table to {.arg {arg}}. See ?adr_."
        ),
        call = call
      )
    }
  }

#' @describeIn data_checkers drug data checker

check_data_drug <-
  function(.data,
           arg = rlang::caller_arg(.data),
           call = rlang::caller_env()){

    drug_cols <-
      c("DrecNo",
        "MedicinalProd_Id",
        "UMCReportId",
        "Drug_Id")

    if (!all(drug_cols %in% names(.data))) {

      missing_cols <-
        drug_cols[!drug_cols %in% names(.data)]

      cli::cli_abort(
        c(
          "{.arg {arg}} is not a {.arg drug} table.",
          "x" = "Missing columns: {missing_cols}",
            ">" = "Supply a {.arg drug} table to {.arg {arg}}. See ?drug_."
        ),
        call = call
      )
    }
  }

#' @describeIn data_checkers link checker

check_data_link <-
  function(.data,
           arg = rlang::caller_arg(.data),
           call = rlang::caller_env()) {
    link_cols <-
      c("Drug_Id", "Adr_Id", "Dechallenge1", "tto_mean", "range")

    if (!all(link_cols %in% names(.data))) {
      missing_cols <-
        link_cols[!link_cols %in% names(.data)]

      cli::cli_abort(
        c(
          "{.arg {arg}} is not a {.arg link} table.",
          "x" = "Missing columns: {missing_cols}",
          ">" = "Supply a {.arg link} table to {.arg {arg}}. See ?link_."
        ),
        call = call
      )
    }
  }

#' @describeIn data_checkers mp checker

check_data_mp <-
  function(.data,
           arg = rlang::caller_arg(.data),
           call = rlang::caller_env()){

    mp_cols <-
      c("DrecNo", "drug_name_t")

    if (!all(mp_cols %in% names(.data))) {

      missing_cols <-
        mp_cols[!mp_cols %in% names(.data)]


      cli::cli_abort(
        c(
          "{.arg {arg}} is not an {.arg mp} table.",
          "x" = "Missing columns: {missing_cols}",
          ">" = "Supply an {.arg mp} table to {.arg {arg}}. See ?mp_."
        ),
        call = call
      )
    }
  }

#' @describeIn data_checkers meddra checker

check_data_meddra <-
  function(.data,
           arg = rlang::caller_arg(.data),
           call = rlang::caller_env()){

    meddra_cols <-
      c("llt_code", "llt_name", "pt_name",
        "soc_name", "hlt_name")

    if (!all(meddra_cols %in% names(.data))) {

      missing_cols <-
        meddra_cols[!meddra_cols %in% names(.data)]


      cli::cli_abort(
        c(
          "{.arg {arg}} is not a {.arg meddra} table.",
          "x" = "Missing columns: {missing_cols}",
          ">" = "Supply a {.arg meddra} table to {.arg {arg}}. See ?meddra_."
        ),
        call = call
      )
    }
  }

#' @describeIn data_checkers smq_list data checker

check_data_smqlist <-
  function(.data,
           arg = rlang::caller_arg(.data),
           call = rlang::caller_env()){

    smqlist_cols <-
      c( # except smq_code, which is in both smq_content and list.
        "smq_name",
        "smq_level",
        "smq_description",
        "smq_source",
        "smq_note",
        "MedDRA_version",
        "status",
        "smq_algorithm"
      )

    smqcontent_cols <-
      c(
        "term_code",
        "term_level",
        "term_scope",
        "term_category",
        "term_weight",
        "term_status",
        "term_addition_version",
        "term_last_modified_version"
      )

    if (!all(smqlist_cols %in% names(.data)) |
        any(smqcontent_cols %in% names(.data))) {

      cli::cli_abort(
        c(
          "{.arg {arg}} is not an {.arg smq_list} table.",
          "x" = "Invalid/missing columns detected",
          ">" = "Did you provide an {.arg smq_list_content}, instead of an {.arg smq_list} dataset?.",
          ">" = "See ?smq_list_."
        ),
        call = call
      )
    }
  }
