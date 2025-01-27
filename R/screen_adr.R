#' @name screen_adr
#'
#' @title Screening of Adverse Drug Reactions
#'
#' @description `r lifecycle::badge('experimental')`
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
#' @keywords descriptive, adr
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
    warning("Both 'freq_threshold' and 'top_n' are specified. Only 'top_n' will be applied. Please specify only one for precise control.")
    freq_threshold <- NULL  # Ignore freq_threshold if both are provided
  }

  # Match the term_level argument to one of the
  # allowed values, or raise an error
  tryCatch({
    term_level <- match.arg(term_level)
  }, error = function(e) {
    stop("Invalid 'term_level' specified. Choose from 'soc', 'hlgt', 'hlt', 'pt', 'llt'.")
  })

  term_level_name <- paste0(term_level, "_name")  # Use term_level_name directly

  # Convert to data.table if necessary
  if (!inherits(.data, "data.table")) {
    .data <- data.table::as.data.table(.data)
  }

  if (!inherits(meddra, "data.table")) {
    meddra <- data.table::as.data.table(meddra)
  }

  # ---- Create the unique MedDRA ID table ----
  create_mid_unique_table <- function(.data, MedDRA_Id, N) {
    .data[, .N, by = MedDRA_Id]
    # Counts the occurrences of each MedDRA_Id
  }

  m_id_unique <-
    create_mid_unique_table(
      .data,
      MedDRA_Id = "MedDRA_Id",
      N = "N"
      )

  # ---- Create a mapping between terms and MedDRA IDs ----
  create_term_to_mid_table <-
    function(meddra,
             m_id_unique,
             llt_code) {

    meddra[llt_code %in% m_id_unique$MedDRA_Id,
           list(term = get(term_level_name), llt_code)
    ]
    }

  t_to_mid <-
    create_term_to_mid_table(
      meddra,
      m_id_unique,
      llt_code = "llt_code")

  t_to_mid <- unique(t_to_mid) # to avoid duplicates

  # ---- Count the number of distinct reports for each term ----
  n_case_counts <-
    .data |>
    dplyr::left_join(
      t_to_mid,
      by = c("MedDRA_Id" = "llt_code"),
      relationship = "many-to-many"
      ) |>
    dplyr::distinct(UMCReportId, term) |>
    # Unique UMCReportId and term combinations

    dplyr::count(term)
    # Count the number of distinct reports for each term

  # ---- Calculate the percentage per report ----
  total_reports <-
    dplyr::n_distinct(.data$UMCReportId)  # Total unique reports

  n_case_counts <- n_case_counts |>
    dplyr::mutate(percentage = (n / total_reports) * 100) |>  # Calculate percentage based on unique reports
    dplyr::arrange(dplyr::desc(percentage))  # Arrange terms by percentage

  # ---- Filter terms based on the frequency threshold if specified ----
  if (!is.null(freq_threshold)) {
    n_case_counts <- n_case_counts |>
      dplyr::filter(percentage >= freq_threshold * 100)
    # Apply the frequency threshold
  }

  # Keep only the top_n most frequent terms if specified
  if (!is.null(top_n)) {
    n_case_counts <- n_case_counts |>
      dplyr::slice_head(n = top_n)
    # Select the top_n most frequent terms
  }

  return(n_case_counts)
  # Return filtered counts with percentages
}
