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
    d_names = "paracetamol",
    repbasis = "sci",
    method = "DrecNo",
    drug_data = drug,
    data_type = "demo"
  )

  expect_true("daily_doses_paracetamol_in_mg" %in% colnames(result))
  expect_equal(nrow(result), 750)  # Same number of rows as input
})


test_that("add_dose handles empty dataset gracefully", {
  empty_demo <- data.frame(UMCReportId = integer(0), stringsAsFactors = FALSE)

  d_code <- list(paracetamol = c("97818920", "97409107"))

  result <- add_dose(
    .data = empty_demo,
    d_code = d_code,
    d_names = "paracetamol",
    repbasis = "sci",
    method = "DrecNo",
    drug_data = drug,
    data_type = "demo"
  )

  expect_equal(nrow(result), 0)  # Should return an empty dataset
})

test_that("add_dose throws an error when missing necessary columns in the data", {
  incomplete_demo <- data.frame(
    UMCReportId = c(1, 2, 3),
    Adr_Id = c(101, 102, 103),  # Missing Drug_Id
    stringsAsFactors = FALSE
  )

  d_code <- list(paracetamol = c("97818920", "97409107"))

  expect_error(
    add_dose(
      .data = incomplete_demo,
      d_code = d_code,
      d_names = "paracetamol",
      repbasis = "sci",
      method = "DrecNo",
      drug_data = drug,
      data_type = "link"
    ),
    "The dataset does not have Drug_Id and Adr_Id columns"
  )
})

test_that("add_dose correctly filters and calculates doses", {
  d_code <- list(paracetamol = c("97818920", "97409107"))

  result <- add_dose(
    .data = demo,
    d_code = d_code,
    d_names = "paracetamol",
    repbasis = "sci",
    method = "DrecNo",
    drug_data = drug,
    data_type = "demo"
  )

  # Check if the dose is calculated correctly (based on the example in the function)
  expect_equal(sum(result$daily_doses_paracetamol_in_mg, na.rm = TRUE), 3048)
})

test_that("add_dose works for different `repbasis` values", {
  d_code <- list(paracetamol = c("97818920", "97409107"))

  result_suspect <- add_dose(
    .data = demo,
    d_code = d_code,
    d_names = "paracetamol_suspected",
    repbasis = "s",
    method = "DrecNo",
    drug_data = drug,
    data_type = "demo"
  )

  result_concomitant <- add_dose(
    .data = demo,
    d_code = d_code,
    d_names = "paracetamol_concomitant",
    repbasis = "c",
    method = "DrecNo",
    drug_data = drug,
    data_type = "demo"
  )

  # The column names should differ
  expect_equal(sum(result_concomitant$daily_doses_paracetamol_in_mg, na.rm = TRUE), 1048)
  expect_equal(sum(result_suspect$daily_doses_paracetamol_in_mg, na.rm = TRUE), 2000)
})

test_that("add_dose handles invalid dose values correctly", {
  drug_invalid <- data.frame(
    UMCReportId = c(1),
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
    d_names = "paracetamol",
    repbasis = "sci",
    method = "DrecNo",
    drug_data = drug_invalid,
    data_type = "demo"
  )

  # The dose should not be added due to invalid frequency
  expect_true(is.na(result$daily_doses_paracetamol_in_mg[1]))
})

