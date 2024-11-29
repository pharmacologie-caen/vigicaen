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
#' @param .data A `data.table` containing drug data,
#' including columns `MedicinalProd_Id` for drug identifiers and `UMCReportId`
#' for unique report identifiers.
#' @param sun A `data.table` containing `Substance_Id` and `Substance.name` mappings.
#' @param ing A `data.table` containing `Substance_Id` and `MedicinalProd_Id` mappings.
#' @param mp A `data.table` containing `MedicinalProd_Id` and `drug-name-t` mappings
#' @param freq_threshold A numeric value indicating the minimum
#' frequency (as a proportion) of cases where a drug must appear
#' to be included in the results. Defaults to `NULL`.
#' @param top_n An integer specifying the number of most frequently occurring drugs to return. Defaults to `NULL`.
#'
#' @return A `data.frame` with the following columns:
#' - **substance**: The substance name.
#' - **n**: The number of unique reports (cases) where the substance appears.
#' - **percentage**: The percentage of total unique reports where the substance appears.
#' The results are sorted in descending order of `percentage`.
#'
#' @export
#' @examples
#' # Example 1: Filter substances appearing in at least 5% of reports
#' screen_drug(
#'   .data = drug_data,
#'   sun = sun_data,
#'   ing = ing_data,
#'   freq_threshold = 0.05
#' )
#'
#' # Example 2: Get the top 5 most frequent substances
#' screen_drug(
#'   .data = drug_data,
#'   sun = sun_data,
#'   ing = ing_data,
#'   top_n = 5
#' )

screen_drug <- function (.data, sun, ing, mp, freq_threshold = NULL, top_n = NULL) {
  # Check if both freq_threshold and top_n are provided, and issue a warning
  if (!is.null(freq_threshold) && !is.null(top_n)) {
    warning("Both 'freq_threshold' and 'top_n' are specified. Only 'top_n' will be applied. Please specify only one for precise control.")
    freq_threshold <- NULL  # Ignore freq_threshold if both are provided
  }

  # Ensure matching data types for Substance_Id
  ing <- ing |> dplyr::mutate(Substance_Id = as.numeric(Substance_Id))
  sun <- sun |> dplyr::mutate(Substance_Id = as.numeric(Substance_Id))

  # Merge ing and sun to create a complete mapping
  drug_mapping <- ing |>
    dplyr::left_join(sun, by = "Substance_Id") #|>
    #dplyr::select(MedicinalProd_Id, Substance.name) |>
   # dplyr::distinct()


  # Ensure matching data types for MedicinalProd_Id
  .data <- .data |> dplyr::mutate(MedicinalProd_Id = as.integer(MedicinalProd_Id))
  drug_mapping <- drug_mapping |> dplyr::mutate(MedicinalProd_Id = as.integer(MedicinalProd_Id))


  # Merge the .data with the drug mapping
  merged_data <- .data |>
    dplyr::left_join(drug_mapping, by = "MedicinalProd_Id", relationship = "many-to-many") |>

    # Merge with the mp table to get drug_name_t
    dplyr::left_join(mp, by = "MedicinalProd_Id")

  # Replace NA in Substance.name with the corresponding drug_name_t from mp
  merged_data <- merged_data |>
    dplyr::mutate(Substance.name = dplyr::coalesce(Substance.name, drug_name_t))


  # Count the number of distinct reports for each substance
  substance_counts <- merged_data |>
    dplyr::distinct(UMCReportId, Substance.name) |>  # Unique UMCReportId and Substance.name combinations
    dplyr::count(Substance.name, name = "n")  # Count the number of distinct reports for each substance

  # Calculate the percentage per report
  total_reports <- dplyr::n_distinct(.data$UMCReportId)  # Total unique reports
  substance_counts <- substance_counts |>
    dplyr::mutate(percentage = (n / total_reports) * 100) |>  # Calculate percentage based on unique reports
    dplyr::arrange(dplyr::desc(percentage))  # Arrange substances by percentage

  # Filter substances based on the frequency threshold if specified
  if (!is.null(freq_threshold)) {
    substance_counts <- substance_counts |>
      dplyr::filter(percentage >= freq_threshold * 100)  # Apply the frequency threshold
  }

  # Keep only the top_n most frequent substances if specified
  if (!is.null(top_n)) {
    substance_counts <- substance_counts |>
      dplyr::slice_head(n = top_n)  # Select the top_n most frequent substances
  }

  return(substance_counts)  # Return filtered counts with percentages
}
