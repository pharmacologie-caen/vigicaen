library(testthat)
library(dplyr)
library(purrr)

# Dummy Data Setup
demo <- demo_
drug <- drug_

# Unit Tests
test_that("add_dose correctly adds daily dose columns", {
  d_code <- list(paracetamol = c(97818920, 97409107))

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

test_that("works with irregular drug and demo names", {
  d_code <- list(paracetamol = c(97818920, 97409107))

  dema <- demo_
  druga <- drug_

  result <- add_dose(
    .data = dema,
    d_code = d_code,
    d_names = names(d_code),
    repbasis = "sci",
    method = "DrecNo",
    drug_data = druga
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

  d_code <- list(paracetamol = c(97818920, 97409107))

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
  d_code <- list(paracetamol = c(97818920, 97409107))

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
  d_code <- list(paracetamol = c(97818920, 97409107))

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
    DrecNo = c(001),
    Basis = c(1),
    Amount = c("-"),
    AmountU = c("3"), # mg
    Frequency = c("0"),
    FrequencyU = c("801"), # Invalid Frequency (0)
    stringsAsFactors = FALSE
  )

  d_code <- list(paracetamol = c(001))

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

library(testthat)
library(dplyr)
library(purrr)

# Dummy Data Setup
demo <- demo_
drug <- drug_

# Unit Tests for add_dose



test_that("you can use arrow/parquet format", {

  d_drecno_test <- rlang::list2("nivolumab" = c(001, 002))


  drug_test <-
    data.table(
      UMCReportId = c(1, 2, 3, 4, 5),
      Drug_Id = c(1, 2, 3, 4, 5),
      MedicinalProd_Id = c(1, 2, 3, 4, 5),
      DrecNo = c(001, 002, 003, 004, 005),
      Basis = c(1, 1, 0, 0, 1),
      Amount = c("-", 123, 456, 789, 758),
      AmountU = c("3", "3", "3", "3", "3"), # mg
      Frequency = c("1", "2", "3", "1", "1"),
      FrequencyU = c("804", "804", "803", "801","801"),
      stringsAsFactors = FALSE
    )

  demo_test <-
    data.table(
      UMCReportId = c(1, 2, 3, 4, 5),
      Region = NA,
      DateDatabase = NA,
      Type = NA,
      # ambiguous column name
      drug_test = c(0, 0, 0, 0, 1)
    )

  tmp_folder <- tempdir()

  # Clean up existing files if they exist
  demo_parquet_path <- paste0(tmp_folder, "/demo.parquet")
  drug_parquet_path <- paste0(tmp_folder, "/drug.parquet")

  if (file.exists(demo_parquet_path)) {
    file.remove(demo_parquet_path)
  }
  if (file.exists(drug_parquet_path)) {
    file.remove(drug_parquet_path)
  }

  # Write parquet files
  arrow::write_parquet(demo_test, sink = demo_parquet_path)
  arrow::write_parquet(drug_test, sink = drug_parquet_path)

  demo_parquet <- arrow::read_parquet(demo_parquet_path)
  drug_parquet <- arrow::read_parquet(drug_parquet_path)

  expect_snapshot({
    res <-
      demo_parquet |>
      add_dose(
        d_code = d_drecno_test,
        d_names = names(d_drecno_test),
        method = "DrecNo",
        repbasis = "sci",
        drug_data = drug_parquet
      )
  })

  expect_snapshot({
    res_a <-
      demo_test |>
      add_dose(
        d_code = d_drecno_test,
        d_names = names(d_drecno_test),
        method = "DrecNo",
        repbasis = "sci",
        drug_data = drug_test
      )
  })

  expect_equal(res, res_a)
})



test_that("add_dose validates drug_data structure", {
  invalid_drug_data <- data.frame(UMCReportId = c(1), InvalidCol = "x")

  expect_error(
    add_dose(
      .data = demo,
      d_code = list(test = 1),
      drug_data = invalid_drug_data
    ),
    regexp = "drug_data` is not a `drug` table"
  )
})


test_that("d_code must be numeric validates drug_data structure", {

  expect_error(
    add_dose(
      .data = demo,
      d_code = list(test = "x"),
    ),
    regexp = "Type of `d_code` is not numeric or integer"
  )
})


test_that("works with mpi_list", {

  mpi <- rlang::list2(
    para = mp_[DrecNo == "42225260", MedicinalProd_Id]
  )

  expect_snapshot({
    demo <-
      demo_ |>
      add_dose(
        d_code = mpi,
        d_names = names(mpi),
        method = "MedicinalProd_Id",
        repbasis = "sci",
        drug_data = drug_
      )
  })


  expect_equal(mean(demo$para_dose_mg_per_day, na.rm = TRUE), 1087.5)


})
