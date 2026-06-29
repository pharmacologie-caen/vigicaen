#' @name screen_adr
#'
#' @title Screening of adverse drug reactions
#'
#' @description `r lifecycle::badge('stable')`
#' Identify and rank the
#' most frequently reported adverse drug reaction (ADR) terms
#' in a dataset, based on a specified MedDRA term level.
#' It allows users to filter terms by a frequency
#' threshold or extract the top `n` most frequently occurring terms.
#'
#' @details
#' - If `freq_threshold` is set (e.g., `0.05`), the function
#' filters ADR terms appearing in at least 5% of unique reports in `.data`.
#' - If `top_n` is specified, only the most frequent `n` terms
#' are returned. If both `freq_threshold` and `top_n` are provided,
#' only `top_n` is applied (a warning is issued in such cases).
#' - Counts are computed at the *case* level, not the ADR level.
#' This means frequencies reflect the proportion of unique
#' reports (cases) where a term is mentioned,
#' rather than the total mentions across all reports.
#'
#' The function processes an ADR dataset (`adr_`) and
#' a MedDRA dataset (`meddra_`) to generate results
#' that are linked to a specific MedDRA hierarchy
#' level (`soc`, `hlgt`, `hlt`, `pt`, or `llt`).
#'
#' @param .data, An `adr` data.table. See \code{\link{adr_}}
#' @param meddra A `meddra` data.table. See \code{\link{meddra_}}
#' @param term_level A character string specifying the
#'   MedDRA hierarchy level. Must be one
#'   of `"soc"`, `"hlgt"`, `"hlt"`, `"pt"`, or `"llt"`.
#' @param freq_threshold A numeric value indicating
#'   the minimum frequency (as a proportion) of cases
#'   where a term must appear to be included in the results.
#'   For example, `0.05` means 5%. Defaults to `NULL`,
#'   meaning no threshold is applied unless `top_n`
#'   is different from `NULL`.
#' @param top_n An integer specifying the number of most
#'   frequently occurring terms to return.
#'   Defaults to `NULL`. Overrides `freq_threshold` if both are provided.
#'
#' @returns A `data.frame` with the following columns:
#' \itemize{
#'   \item **term**: The MedDRA term at the specified hierarchy level.
#'   \item **n**: The number of unique reports (cases) where
#'   the term appears.
#'   \item **percentage**: The percentage of total unique reports
#'   where the term appears.
#' }
#' The results are sorted in descending order of `percentage`.
#'
#' @export
#' @keywords descriptive adr
#'
#' @examples
#' # Example 1: Filter terms appearing in at least 5% of reports
#' screen_adr(
#'   .data = adr_,
#'   meddra = meddra_,
#'   term_level = "pt",
#'   freq_threshold = 0.05
#' )
#'
#' # Example 2: Get the top 5 most frequent terms
#' screen_adr(
#'   .data = adr_,
#'   meddra = meddra_,
#'   term_level = "hlt",
#'   top_n = 5
#' )
#'
#' @importFrom data.table .N
#' @importFrom data.table as.data.table

utils::globalVariables(c("UMCReportId", "term", "n", "percentage"))

screen_adr <-
  function (
    .data,
    meddra,
    term_level = c("soc", "hlgt", "hlt", "pt", "llt"),
    freq_threshold = NULL,
    top_n = NULL) {

  # Check if both freq_threshold and top_n are provided,
  # and issue a warning if so

  if (!is.null(freq_threshold) && !is.null(top_n)) {
    cli::cli_warn(
      c("Both {cli::col_yellow({symbol$bullet}, ' freq_threshold')} and {cli::col_yellow({symbol$bullet}, ' top_n')} are specified.",
      ">" = "Only {cli::col_green({symbol$tick})} 'top_n' will be applied.",
      "i" = "Please specify only one for precise control.")
    )
    freq_threshold <- NULL  # Ignore freq_threshold if both are provided
  }

  term_level <-
    rlang::arg_match(term_level)

  term_level_name <- paste0(term_level, "_name")

  # Validate only the columns actually used. A full `adr` / `meddra` table is
  # not required (e.g. Adr_Id / Outcome and the other hierarchy levels are
  # unused here), so we check the used columns rather than the whole schema.
  check_columns_in_data(.data, c("UMCReportId", "MedDRA_Id"))
  check_columns_in_data(meddra, c("llt_code", term_level_name))

  # `meddra` is a dictionary: collect a small `llt_code` -> `term` map. The key
  # is cast to integer so the join works on every backend (incl. arrow, where a
  # type mismatch would otherwise error).
  term_map <-
    meddra |>
    dplyr::select(dplyr::all_of(c("llt_code", term_level_name))) |>
    dplyr::collect() |>
    dplyr::rename(term = dplyr::all_of(term_level_name)) |>
    dplyr::mutate(llt_code = as.integer(.data$llt_code)) |>
    dplyr::distinct()

  # Total number of unique reports, kept lazy so an arrow Dataset streams.
  total_reports <-
    .data |>
    dplyr::distinct(UMCReportId) |>
    dplyr::count() |>
    dplyr::collect() |>
    dplyr::pull(n)

  if (total_reports == 0) {
    cli::cli_abort(
      c("{.arg .data} contains no reports.",
        "x" = "Cannot compute term frequencies on an empty {.arg .data}."),
      class = "empty_data_no_reports"
    )
  }

  # Count distinct cases per term. The aggregation streams in arrow when `.data`
  # is out-of-memory; only the small per-term result is pulled into R. A single
  # `llt_code` may map to several terms (a code used in two groups), so the join
  # is intentionally many-to-many.
  adr_keyed <-
    .data |>
    dplyr::select(dplyr::all_of(c("UMCReportId", "MedDRA_Id"))) |>
    dplyr::mutate(MedDRA_Id = as.integer(MedDRA_Id))

  joined <-
    if (inherits(.data, c("Table", "Dataset", "arrow_dplyr_query"))) {
      adr_keyed |>
        dplyr::left_join(arrow::arrow_table(term_map),
                         by = c("MedDRA_Id" = "llt_code"))
    } else {
      adr_keyed |>
        dplyr::left_join(term_map,
                         by = c("MedDRA_Id" = "llt_code"),
                         relationship = "many-to-many")
    }

  output <-
    joined |>
    dplyr::distinct(UMCReportId, term) |>
    dplyr::count(term) |>
    dplyr::collect() |>
    dplyr::mutate(percentage = (n / .env$total_reports) * 100) |>
    # ties broken alphabetically by term, matching the previous output
    dplyr::arrange(dplyr::desc(percentage), term)

  # Filter terms based on the frequency threshold if specified
  if (!is.null(freq_threshold)) {
    output <- output |>
      dplyr::filter(percentage >= .env$freq_threshold * 100)
  }

  # Keep only the top_n most frequent terms if specified
  if (!is.null(top_n)) {
    output <- output |>
      dplyr::slice_head(n = top_n)
  }

  data.table::as.data.table(output)
}
