library(data.table)

screen_drug <- function (.data, sun, ing, mp, freq_threshold = NULL, top_n = NULL) {

  # Check if both freq_threshold and top_n are provided, and issue a warning
  if (!is.null(freq_threshold) && !is.null(top_n)) {
    warning("Both 'freq_threshold' and 'top_n' are specified. Only 'top_n' will be applied. Please specify only one for precise control.")
    freq_threshold <- NULL  # Ignore freq_threshold if both are provided
  }

  # Convert input data frames to data.table for more efficient merging
  setDT(.data)
  setDT(sun)
  setDT(ing)
  setDT(mp)

  # Ensure matching data types for Substance_Id
  sun$Substance_Id <- as.numeric(sun$Substance_Id)
  ing$Substance_Id <- as.numeric(ing$Substance_Id)

  # Merge ing and sun to create a complete mapping
  drug_mapping <- merge(ing, sun, by = "Substance_Id", all.x = TRUE)

  # Convert MedicinalProd_Id to the same type (character) in both tables
  .data$MedicinalProd_Id <- as.integer(.data$MedicinalProd_Id)
  drug_mapping$MedicinalProd_Id <- as.integer(drug_mapping$MedicinalProd_Id)

  # Merge the .data with the drug mapping (many-to-many join)
  merged_data <- merge(.data, drug_mapping, by = "MedicinalProd_Id", all.x = TRUE, allow.cartesian = TRUE)

  # Merge with the mp table to get drug_name_t
  merged_data <- merge(merged_data, mp, by = "MedicinalProd_Id", all.x = TRUE)

  # Replace NA in Substance.name with the corresponding drug_name_t from mp
  merged_data$Substance.name <- ifelse(is.na(merged_data$Substance.name), merged_data$drug_name_t, merged_data$Substance.name)

  # Count the number of distinct reports for each substance
  substance_counts <- merged_data[, .(n = uniqueN(UMCReportId)), by = Substance.name]

  # Calculate the percentage per report
  total_reports <- uniqueN(.data$UMCReportId)  # Total unique reports
  substance_counts[, percentage := (n / total_reports) * 100]  # Calculate percentage based on unique reports

  # Sort substances by percentage
  substance_counts <- substance_counts[order(-percentage)]

  # Filter substances based on the frequency threshold if specified
  if (!is.null(freq_threshold)) {
    substance_counts <- substance_counts[percentage >= freq_threshold * 100]  # Apply the frequency threshold
  }

  # Keep only the top_n most frequent substances if specified
  if (!is.null(top_n)) {
    substance_counts <- substance_counts[1:top_n]  # Select the top_n most frequent substances
  }

  return(substance_counts)  # Return filtered counts with percentages
}
