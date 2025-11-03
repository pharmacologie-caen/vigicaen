test_that("finding or not finding drug dose displays correctly", {
  d_code <- list(paracetamol = c(97818920, 97409107),
                 unknown_drug = c(99999999))

  expect_snapshot(
      demo <-
        add_dose(
        .data = demo_,
        d_code = d_code[1],
        drug_data = drug_
      )
  )

  # with no dose
  expect_snapshot(
    demo <-
      add_dose(
        .data = demo_,
        d_code = d_code[2],
        drug_data = drug_
      )
  )

  # both

  expect_snapshot(
    demo <-
      add_dose(
        .data = demo_,
        d_code = d_code,
        drug_data = drug_
      )
  )
})

test_that("add_dose correctly adds daily dose columns", {
  d_code <- list(paracetamol = c(97818920, 97409107))

  suppressMessages(
    result <- add_dose(
      .data = demo_,
      d_code = d_code,
      d_dose_names = names(d_code),
      repbasis = "sci",
      method = "DrecNo",
      drug_data = drug_
    )
  )

  expect_true("paracetamol_dose_mg_per_day" %in% colnames(result))
  expect_equal(nrow(result), 750)  # Same number of rows as input
})

test_that("works with irregular drug and demo names", {
  d_code <- list(paracetamol = c(97818920, 97409107))

  dema <- demo_
  druga <- drug_

  suppressMessages(
    result <- add_dose(
      .data = dema,
      d_code = d_code,
      d_dose_names = names(d_code),
      repbasis = "sci",
      method = "DrecNo",
      drug_data = druga
    )
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

  suppressMessages(
    result <- add_dose(
      .data = empty_demo,
      d_code = d_code,
      d_dose_names = names(d_code),
      repbasis = "sci",
      method = "DrecNo",
      drug_data = drug_
    )
  )

  expect_equal(nrow(result), 0)  # Should return an empty dataset
})


test_that("add_dose correctly filters and calculates doses", {
  d_code <- list(paracetamol = c(97818920, 97409107))

  result <- add_dose(
    .data = demo_,
    d_code = d_code,
    repbasis = "sci",
    method = "DrecNo",
    drug_data = drug_
  )

  # Check if the dose is calculated correctly (based on the example in the function)
  expect_equal(sum(result$paracetamol_dose_mg_per_day, na.rm = TRUE), 3048)
})

test_that("add_dose works for different `repbasis` values", {
  d_code <- list(paracetamol = c(97818920, 97409107))

  result_suspect <- add_dose(
    .data = demo_,
    d_code = d_code,
    d_dose_names = names(d_code),
    repbasis = "s",
    method = "DrecNo",
    drug_data = drug_
  )

  result_concomitant <- add_dose(
    .data = demo_,
    d_code = d_code,
    d_dose_names = names(d_code),
    repbasis = "c",
    method = "DrecNo",
    drug_data = drug_
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

  suppressMessages(
    result <- add_dose(
      .data = demo_,
      d_code = d_code,
      d_dose_names = names(d_code),
      repbasis = "sci",
      method = "DrecNo",
      drug_data = drug_invalid
    )
  )

  # The dose should not be added due to invalid frequency
  expect_true(is.na(result$paracetamol_dose_mg_per_day[1]))
})

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

  tmp_folder <- paste0(tempdir(), "/", "add_ind_t1")

  dir.create(path = tmp_folder)

  # Write parquet files
  arrow::write_parquet(demo_test,
                       sink = paste0(tmp_folder, "\\demo.parquet"))
  arrow::write_parquet(drug_test,
                       sink = paste0(tmp_folder, "\\drug.parquet"))

  demo_parquet <- arrow::read_parquet(paste0(tmp_folder, "\\demo.parquet"))
  drug_parquet <- arrow::read_parquet(paste0(tmp_folder, "\\drug.parquet"))

  suppressMessages(
    res <-
      demo_parquet |>
      add_dose(
        d_code = d_drecno_test,
        d_dose_names = names(d_drecno_test),
        method = "DrecNo",
        repbasis = "sci",
        drug_data = drug_parquet
      )
  )

  suppressMessages(
    res_a <-
      demo_test |>
      add_dose(
        d_code = d_drecno_test,
        d_dose_names = names(d_drecno_test),
        method = "DrecNo",
        repbasis = "sci",
        drug_data = drug_test
      )
  )

  expect_equal(res, res_a)

  unlink(tmp_folder, recursive = TRUE)

  if(dir.exists(tmp_folder) & Sys.info()[["sysname"]] != "Windows")
    file.remove(tmp_folder)
})



test_that("add_dose validates drug_data structure", {
  invalid_drug_data <- data.frame(UMCReportId = c(1), InvalidCol = "x")

  expect_error(
    add_dose(
      .data = demo_,
      d_code = list(test = 1),
      drug_data = invalid_drug_data
    ),
    regexp = "drug_data` must be a `drug` table"
  )
})


test_that("d_code must be numeric", {

  expect_error(
    add_dose(
      .data = demo_,
      d_code = list(test = "x"),
    ),
    regexp = "Type of `d_code` must be numeric or integer"
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
        d_dose_names = names(mpi),
        method = "MedicinalProd_Id",
        repbasis = "sci",
        drug_data = drug_
      )
  })

  expect_equal(mean(demo$para_dose_mg_per_day, na.rm = TRUE), 1087.5)

})
