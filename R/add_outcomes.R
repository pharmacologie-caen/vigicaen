#' Add outcome columns to a dataset
#'
#' @description `r lifecycle::badge('experimental')`
#' `add_death()`, `add_serious()`, and `add_fup()` create outcome columns
#' in a vigibase dataset (typically `demo`), using data from the `out`
#' and `followup` tables.
#' These functions handle both in-memory and out-of-memory (Arrow) tables.
#'
#' @details
#' \itemize{
#'   \item `add_death()` adds a column indicating whether the case
#'   resulted in death (i.e., `Seriousness == "1"` in the `out` table).
#'   Cases with no outcome data are coded `NA`.
#'   Cases with outcome data but no death are coded `0`.
#'   Cases with death are coded `1`.
#'   \item `add_serious()` adds a column indicating whether the case
#'   was serious (i.e., `Serious == "Y"` in the `out` table).
#'   Cases with no outcome data are coded `NA`.
#'   Cases with outcome data but not serious are coded `0`.
#'   Serious cases are coded `1`.
#'   \item `add_fup()` adds a column indicating whether the case has a
#'   follow-up (i.e., `UMCReportId` appears in the `followup` table).
#'   Cases with a follow-up are coded `1`. Others are coded `0`.
#' }
#'
#' @param .data The dataset to update (usually `demo`).
#' @param out_data A data.frame containing the outcome data (usually `out`).
#' @param fup_data A data.frame containing the follow-up data (usually
#' `followup`).
#' @param col_name A character string. Name of the new column.
#' Defaults to `"death"`, `"serious"`, or `"fup"` respectively.
#'
#' @returns A dataset with the new outcome column.
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @importFrom rlang :=
#' @keywords data_management outcomes
#' @seealso [add_drug()], [add_adr()]
#' @name add_outcomes
#'
#' @examples
#' demo <- demo_
#' out  <- out_
#'
#' demo <- add_death(demo, out_data = out)
#' demo <- add_serious(demo, out_data = out)
#'
#' followup <- followup_
#'
#' demo <- add_fup(demo, fup_data = followup)
#'
#' desc_facvar(demo, c("death", "serious", "fup"))
NULL

#' @rdname add_outcomes
#' @export
add_death <-
  function(.data,
           out_data,
           col_name = "death") {

    check_data_out(out_data, "out_data")
    abort_if_ind(.data, "add_death()")

    add_outcome_flag(.data, out_data, col_name,
                     positive = rlang::quo(.data$Seriousness == "1"))
  }

#' @rdname add_outcomes
#' @export
add_serious <-
  function(.data,
           out_data,
           col_name = "serious") {

    check_data_out(out_data, "out_data")
    abort_if_ind(.data, "add_serious()")

    add_outcome_flag(.data, out_data, col_name,
                     positive = rlang::quo(.data$Serious == "Y"))
  }

#' @rdname add_outcomes
#' @export
add_fup <-
  function(.data,
           fup_data,
           col_name = "fup") {

    check_data_fup(fup_data, "fup_data")
    abort_if_ind(.data, "add_fup()")

    add_outcome_flag(.data, fup_data, col_name, positive = NULL)
  }

# ---- internal helpers --------------------------------------------------------

# Abort if `.data` is an `ind` table (the add_outcome_* functions don't support
# it). `fn` is the calling-function label shown in the message.
abort_if_ind <-
  function(.data, fn, call = rlang::caller_env()) {
    if (query_data_type(.data, ".data") == "ind") {
      cli::cli_abort(
        c(
          '{.arg .data} must be one of {.code {c("demo", "drug", "adr", "link")}}.',
          "x" = "{.code ind} tables not supported in {.code {fn}}."
        ),
        call = call
      )
    }
  }

# Shared core for add_death() / add_serious() / add_fup().
#
# `positive` is a quosure giving the per-row positive condition (e.g.
# `Seriousness == "1"`). When `NULL` (followup), any case present in
# `source_data` is coded 1 and others 0. With a `positive` condition, cases
# present in `source_data` are coded 0/1 and cases absent from it are coded NA.
# Works on both in-memory and arrow `.data` (compute()d for arrow).
add_outcome_flag <-
  function(.data, source_data, col_name, positive = NULL) {

    old_opt <- getOption("arrow.pull_as_vector")
    options(arrow.pull_as_vector = TRUE)
    on.exit(options(arrow.pull_as_vector = old_opt), add = TRUE)

    present_ids <-
      source_data |>
      dplyr::pull("UMCReportId")

    result <-
      if (is.null(positive)) {
        # presence-only (followup): in source -> 1, else 0
        .data |>
          dplyr::mutate(
            "{col_name}" := ifelse(UMCReportId %in% .env$present_ids, 1L, 0L)
          )
      } else {
        positive_ids <-
          source_data |>
          dplyr::filter(!!positive) |>
          dplyr::pull("UMCReportId")

        # present in source -> 0/1 ; absent from source -> NA
        .data |>
          dplyr::mutate(
            "{col_name}" := ifelse(
              UMCReportId %in% .env$present_ids,
              as.integer(UMCReportId %in% .env$positive_ids),
              NA_integer_
            )
          )
      }

    if (any(c("Table", "Dataset") %in% class(.data))) {
      result |> dplyr::compute()
    } else {
      result
    }
  }
