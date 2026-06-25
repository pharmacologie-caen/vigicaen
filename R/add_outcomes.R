#' Add outcome columns to a dataset
#'
#' @description `r lifecycle::badge('stable')`
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
#' @export
#' @importFrom rlang .data
#' @importFrom rlang .env
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
#' check_dm(demo, c("death", "serious", "fup"))
NULL

#' @rdname add_outcomes
#' @export
add_death <-
  function(.data,
           out_data,
           col_name = "death") {

    check_data_out(out_data, "out_data")

    data_type <- query_data_type(.data, ".data")

    if (data_type == "ind") {
      cli::cli_abort(
        c(
          '{.arg .data} must be one of {.code {c("demo", "drug", "adr", "link")}}.',
          "x" = "{.code ind} tables not supported in {.code add_death()}."
        )
      )
    }

    old_opt <- getOption("arrow.pull_as_vector")
    options(arrow.pull_as_vector = TRUE)
    on.exit(options(arrow.pull_as_vector = old_opt), add = TRUE)

    all_out_ids <-
      out_data |>
      dplyr::pull("UMCReportId")

    death_ids <-
      out_data |>
      dplyr::filter(.data$Seriousness == "1") |>
      dplyr::pull("UMCReportId")

    result <-
      .data |>
      dplyr::mutate(
        "{col_name}" := ifelse(
          UMCReportId %in% .env$all_out_ids,
          as.integer(UMCReportId %in% .env$death_ids),
          NA_integer_
        )
      )

    if (any(c("Table", "Dataset") %in% class(.data))) {
      result |> dplyr::compute()
    } else {
      result
    }
  }

#' @rdname add_outcomes
#' @export
add_serious <-
  function(.data,
           out_data,
           col_name = "serious") {

    check_data_out(out_data, "out_data")

    data_type <- query_data_type(.data, ".data")

    if (data_type == "ind") {
      cli::cli_abort(
        c(
          '{.arg .data} must be one of {.code {c("demo", "drug", "adr", "link")}}.',
          "x" = "{.code ind} tables not supported in {.code add_serious()}."
        )
      )
    }

    old_opt <- getOption("arrow.pull_as_vector")
    options(arrow.pull_as_vector = TRUE)
    on.exit(options(arrow.pull_as_vector = old_opt), add = TRUE)

    all_out_ids <-
      out_data |>
      dplyr::pull("UMCReportId")

    serious_ids <-
      out_data |>
      dplyr::filter(.data$Serious == "Y") |>
      dplyr::pull("UMCReportId")

    result <-
      .data |>
      dplyr::mutate(
        "{col_name}" := ifelse(
          UMCReportId %in% .env$all_out_ids,
          as.integer(UMCReportId %in% .env$serious_ids),
          NA_integer_
        )
      )

    if (any(c("Table", "Dataset") %in% class(.data))) {
      result |> dplyr::compute()
    } else {
      result
    }
  }

#' @rdname add_outcomes
#' @export
add_fup <-
  function(.data,
           fup_data,
           col_name = "fup") {

    check_data_fup(fup_data, "fup_data")

    data_type <- query_data_type(.data, ".data")

    if (data_type == "ind") {
      cli::cli_abort(
        c(
          '{.arg .data} must be one of {.code {c("demo", "drug", "adr", "link")}}.',
          "x" = "{.code ind} tables not supported in {.code add_fup()}."
        )
      )
    }

    old_opt <- getOption("arrow.pull_as_vector")
    options(arrow.pull_as_vector = TRUE)
    on.exit(options(arrow.pull_as_vector = old_opt), add = TRUE)

    fup_ids <-
      fup_data |>
      dplyr::pull("UMCReportId")

    result <-
      .data |>
      dplyr::mutate(
        "{col_name}" := ifelse(
          UMCReportId %in% .env$fup_ids,
          1L,
          0L
        )
      )

    if (any(c("Table", "Dataset") %in% class(.data))) {
      result |> dplyr::compute()
    } else {
      result
    }
  }
