#' @name screen_drug
#'
#' @title Screening of Drugs
#'
#' @description `r lifecycle::badge('experimental')`
#' The `screen_drug()` function identifies and ranks the most frequently
#' reported drugs (by active ingredient) in a dataset.
#'
#' @details
#' - If `freq_threshold` is set (e.g., `0.05`), the function filters
#' drugs appearing in at least 5% of unique reports in `.data`.
#' - If `top_n` is specified, only the most frequent `n` drugs are returned.
#' If both `freq_threshold` and `top_n` are provided, only `top_n` is
#' applied (a warning is raised in such cases).
#' - Counts are computed at the *case* level, not the drug mention level.
#' This means frequencies reflect the proportion of unique
#' reports (cases) where a drug is mentioned, rather than the total
#' mentions across all reports.
#'
#' @param .data, An `drug` data.table. See \code{\link{drug_}}
#' @param mp_data An `MP` data.table. See \code{\link{mp_}}
#' @param freq_threshold A numeric value indicating the minimum
#' frequency (as a proportion) of cases where a drug must appear
#' to be included in the results. Defaults to `NULL`.
#' @param top_n An integer specifying the number of most frequently occurring drugs to return. Defaults to `NULL`.
#'
#' @return A `data.frame` with the following columns:
#' \itemize{
#'    \item `Drug name`: The drug name.
#'    \item `DrecNo`: The drug record number
#'    \item `N`: The number of unique reports (cases) where the drug appears.
#'    \item `percentage`: The percentage of total unique reports
#'    where the drug appears.
#' }
#' The results are sorted in descending order of `percentage`.
#'
#' @export
#' @examples
#' # Filter drugs appearing in at least 10% of reports
#' screen_drug(
#'   .data = drug_,
#'   mp_data = mp_,
#'   freq_threshold = 0.10
#' )
#'
#' # Get the top 5 most reported drugs
#' screen_drug(
#'   .data = drug_,
#'   mp_data = mp_,
#'   top_n = 5
#' )
#'
#' # nb: in the example datasets, not all drugs are recorded in mp_,
#' # leading to NAs in screen_drug output.


screen_drug <-
  function(.data,
            mp_data,
            freq_threshold = NULL,
            top_n = NULL) {

  # Check if both freq_threshold and top_n are provided, and issue a warning
  if (!is.null(freq_threshold) && !is.null(top_n)) {
    cli::cli_warn(c(
      "Both 'freq_threshold' and 'top_n' are specified.",
      "i" = "Only 'top_n' will be applied.",
      ">" = "Specify only one for precise control."
    )
    )
    freq_threshold <- NULL  # Ignore freq_threshold if both are provided
  }

 check_data_drug(.data, ".data")

 check_data_mp(mp_data, "mp_data")

 # prepare mp

 mp_distinct <-
   mp_data |>
   dplyr::filter(.data$Sequence.number.1 == "01" &
                   .data$Sequence.number.2 == "001") |>
   dplyr::select(dplyr::all_of(c("DrecNo", "drug_name_t"))) |>
   dplyr::distinct(.data$DrecNo, .data$drug_name_t)

 # Synthetize .data

 drecno_count <-
   .data |>
   # count at case level
   dplyr::distinct(.data$DrecNo, .data$UMCReportId) |>
   dplyr::group_by(.data$DrecNo) |>
   dplyr::count() |>
   dplyr::ungroup()

 drug_count <-
   drecno_count |>
   dplyr::left_join(
     mp_distinct,
     by = c("DrecNo" = "DrecNo")
   ) |>
   dplyr::arrange(dplyr::desc(.data$n))

  # Calculate the percentage per report
  total_reports <-
    dplyr::n_distinct(.data$UMCReportId)

  # compute percentage and arrange columns
  output <-
    drug_count |>
    dplyr::mutate(percentage = (n / total_reports) * 100) |>
    dplyr::rename("Drug name" = "drug_name_t", "N" = "n") |>
    dplyr::relocate("Drug name")

  # Filter substances based on the frequency threshold if specified
  if (!is.null(freq_threshold)) {
    output <- output |>
      dplyr::filter(percentage >= freq_threshold * 100)
  }

  # Keep only the top_n most frequent substances if specified
  if (!is.null(top_n)) {
    output <- output |>
      dplyr::slice_head(n = top_n)
  }

  return(output)  # Return (filtered) counts with percentages
}
