screen_adr <- function (.data, meddra, term_level = c("soc", "hlgt", "hlt", "pt", "llt"), freq_threshold = NULL, top_n = NULL) {
  # Check if both freq_threshold and top_n are provided, and issue a warning
  if (!is.null(freq_threshold) && !is.null(top_n)) {
    warning("Both 'freq_threshold' and 'top_n' are specified. Only 'top_n' will be applied. Please specify only one for precise control.")
    freq_threshold <- NULL  # Ignore freq_threshold if both are provided
  }

  # Match the term_level argument to one of the allowed values, or raise an error
  tryCatch({
    term_level <- match.arg(term_level)
  }, error = function(e) {
    stop("Invalid 'term_level' specified. Choose from 'soc', 'hlgt', 'hlt', 'pt', 'llt'.")
  })

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
