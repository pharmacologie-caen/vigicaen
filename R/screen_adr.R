#' Screening of adverse drug reactions
#'
#' @description `r lifecycle::badge('experimental')` `screen_adr()` sort top seen terms,
#' according to a desired term level.
#'
#' @details If `freq_threshold` is set to 0.05, all adverse drug reactions found
#' at least in 5% of adrs in .data will be shown. That is not exactly the same as 5% of
#'  cases. Counts are provided at *case* level (not adr level).
#' The function uses an \code{\link{adr_}} data.table and a \code{\link{meddra_}} data.table.
#'
#'## Arguments of the function ##
#' @param .data A data.table, an adr data
#' @param meddra A data.table, a meddra data
#' @param term_level A character string, one of "pt", "hlt", "hlgt", "soc".
#' @param freq_threshold A numeric, lowest frequency percentage of term to be
#' screened.
#' @param top_n A numeric, representing the number of the most represented adverse effects to be screened. You can only use top_n OR freq_threshold
#'
#' @return A data.frame with counts of each term in .data. with the following variables :
#' @param term Name of the adverse effect at the selected level
#' @param n Number of reports for this adverse effect
#' @param percentage Percentage of cases with this adverse effect
#'
#' @export
#' @keywords descriptive, adr
#' @examples
#' screen_adr(
#'   .data = adr_,
#'   meddra = meddra_,
#'   term_level = "pt",
#'   top_n = 20
#' )
#'
#' screen_adr(
#'   .data = adr_,
#'   meddra = meddra_,
#'   term_level = "hlt",
#'   freq_threshold = 0.05
#' )

screen_adr <- function (.data, meddra, term_level = c("soc", "hlgt", "hlt", "pt", "llt"), freq_threshold = NULL, top_n = NULL) {
  # Check if both freq_threshold and top_n are provided, and issue a warning
  if (!is.null(freq_threshold) && !is.null(top_n)) {
    warning("Both 'freq_threshold' and 'top_n' are specified. Only 'top_n' will be applied. Please specify only one for precise control.")
    freq_threshold <- NULL  # Ignore freq_threshold if both are provided
  }

  # Match the term_level argument to one of the allowed values
  term_level <- match.arg(term_level)
  term_level_name <- paste0(term_level, "_name")  # Use term_level_name directly

  # Create the unique MedDRA ID table
  create_mid_unique_table <- function(.data, MedDRA_Id, N) {
    .data[, .N, by = MedDRA_Id]  # Counts the occurrences of each MedDRA_Id
  }
  m_id_unique <- create_mid_unique_table(.data, MedDRA_Id = "MedDRA_Id", N = "N")

  # Create a mapping between terms and MedDRA IDs
  create_term_to_mid_table <- function(meddra, m_id_unique, llt_code) {
    meddra[llt_code %in% m_id_unique$MedDRA_Id, .(term = get(term_level_name), llt_code)]
  }
  t_to_mid <- create_term_to_mid_table(meddra, m_id_unique, llt_code = "llt_code")

  # Count the number of distinct reports for each term
  n_case_counts <- .data %>%
    dplyr::left_join(t_to_mid, by = c("MedDRA_Id" = "llt_code"), relationship = "many-to-many") %>%
    dplyr::distinct(UMCReportId, term) %>%  # Unique UMCReportId and term combinations
    dplyr::count(term)  # Count the number of distinct reports for each term

  # Calculate the percentage per report
  total_reports <- dplyr::n_distinct(.data$UMCReportId)  # Total unique reports
  n_case_counts <- n_case_counts %>%
    dplyr::mutate(percentage = (n / total_reports) * 100) %>%  # Calculate percentage based on unique reports
    dplyr::arrange(dplyr::desc(percentage))  # Arrange terms by percentage

  # Filter terms based on the frequency threshold if specified
  if (!is.null(freq_threshold)) {
    n_case_counts <- n_case_counts %>%
      dplyr::filter(percentage >= freq_threshold * 100)  # Apply the frequency threshold
  }

  # Keep only the top_n most frequent terms if specified
  if (!is.null(top_n)) {
    n_case_counts <- n_case_counts %>%
      dplyr::slice_head(n = top_n)  # Select the top_n most frequent terms
  }

  return(n_case_counts)  # Return filtered counts with percentages
}
