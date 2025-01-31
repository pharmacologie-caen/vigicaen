library(testthat)
library(dplyr)

# Dummy Data Setup
demo <- demo_
drug <- drug_

# Unit Tests
test_that("add_dose correctly adds daily dose columns", {
  d_code <- list(paracetamol = c("97818920", "97409107"))

  result <- add_dose(
    .data = demo,
    d_code = d_code,
    d_names = names(d_code),
    repbasis = "sci",
    method = "DrecNo",
    drug_data = drug
  )

  expect_true("paracetamol_dose_mg_per_day" %in% colnames(result))
  expect_equal(nrow(result), 750)  # Same number of rows as input
})


test_that("add_dose handles empty dataset gracefully", {
  # Create an empty demo table with the required structure
  empty_demo <- data.frame(
    UMCReportId = integer(),      # UMCReportId column (integer type)
    AgeGroup = character(),       # AgeGroup column (character type)
    Gender = character(),         # Gender column (character type)
    DateDatabase = character(),   # DateDatabase column (character type, e.g., "YYYYMMDD")
    Type = character(),           # Type column (character type)
    Region = character(),         # Region column (character type)
    FirstDateDatabase = character() # FirstDateDatabase column (character type, e.g., "YYYYMMDD")
  )

  d_code <- list(paracetamol = c("97818920", "97409107"))

  result <- add_dose(
    .data = empty_demo,
    d_code = d_code,
    d_names = names(d_code),
    repbasis = "sci",
    method = "DrecNo",
    drug_data = drug
  )

  expect_equal(nrow(result), 0)  # Should return an empty dataset
})


test_that("add_dose correctly filters and calculates doses", {
  d_code <- list(paracetamol = c("97818920", "97409107"))

  result <- add_dose(
    .data = demo,
    d_code = d_code,
    d_names = names(d_code),
    repbasis = "sci",
    method = "DrecNo",
    drug_data = drug
  )

  # Check if the dose is calculated correctly (based on the example in the function)
  expect_equal(sum(result$paracetamol_dose_mg_per_day, na.rm = TRUE), 3048)
})

test_that("add_dose works for different `repbasis` values", {
  d_code <- list(paracetamol = c("97818920", "97409107"))

  result_suspect <- add_dose(
    .data = demo,
    d_code = d_code,
    d_names = names(d_code),
    repbasis = "s",
    method = "DrecNo",
    drug_data = drug
  )

  result_concomitant <- add_dose(
    .data = demo,
    d_code = d_code,
    d_names = names(d_code),
    repbasis = "c",
    method = "DrecNo",
    drug_data = drug
  )

  # The column names should differ
  expect_equal(sum(result_concomitant$paracetamol_dose_mg_per_day, na.rm = TRUE), 1048)
  expect_equal(sum(result_suspect$paracetamol_dose_mg_per_day, na.rm = TRUE), 2000)
})

test_that("add_dose handles invalid dose values correctly", {
  drug_invalid <- data.frame(
    UMCReportId = c(1),
    Drug_Id = c(1),
    MedicinalProd_Id = c(1),
    DrecNo = c("DRECNO_001"),
    Basis = c(1),
    Amount = c("-"),
    AmountU = c("3"), # mg
    Frequency = c("0"),
    FrequencyU = c("801"), # Invalid Frequency (0)
    stringsAsFactors = FALSE
  )

  d_code <- list(paracetamol = c("DRECNO_001"))

  result <- add_dose(
    .data = demo,
    d_code = d_code,
    d_names = names(d_code),
    repbasis = "sci",
    method = "DrecNo",
    drug_data = drug_invalid
  )

  # The dose should not be added due to invalid frequency
  expect_true(is.na(result$paracetamol_dose_mg_per_day[1]))
})

