library(testthat)

test_that("add_drug_adr correctly merges drug and ADR data", {
  # Create mock data
  d_names <- rlang::list2(
    nivolumab = "nivolumab",
    pembrolizumab = "pembrolizumab"
  )

  d_drecno <-
    d_names |>
    get_drecno(
      mp_short = mp_short_
    )


  demo <- demo_
  drug_data <- drug_
  adr_data <- adr_
  d_names <- d_names
  a_names <- paste0("adr_", names(ex_$a_llt))
  a_llt <- ex_$a_llt



  # Use the add_drug_adr function
  result <- add_drug_adr(
    .data = demo,
    d_code = d_drecno,
    d_names = d_names,
    drug_data = drug_data,
    a_code = a_llt,
    a_names = a_names,
    adr_data = adr_data,
    repbasis = "sci",
    method = "DrecNo",
    data_type = "demo"
  )

  result_adr <- add_adr(
    .data = demo,
    a_code = a_llt,
    a_names = a_names,
    adr_data = adr_data,
    data_type = "demo"
  )

  result_drug <- add_drug(
    .data = demo,
    d_code = d_drecno,
    d_names = d_names,
    repbasis = "sci",
    method = "DrecNo",
    drug_data = drug_data,
    data_type = "demo"
  )

  # Check that the UMCReportId column is still there and correctly merged
  expect_equal(result$UMCReportId, demo$UMCReportId)  # Ensure UMCReportId is preserved

  # Check that both drug and ADR columns are included in the result
  expect_true("nivolumab" %in% colnames(result))  # Check if drug column exists
  expect_true("adr_a_colitis" %in% colnames(result))    # Check if ADR column exists

  # Verify that the data is correctly merged (no loss of data)
  expect_equal(nrow(result), nrow(demo))  # The number of rows should be the same as the original dataset

  # Check that no adr data is lost when merging
  expect_equal(result_adr$adr_a_colitis, result$adr_a_colitis)

  # Check that no drug data is lost when merging
  expect_equal(result_drug$nivolumab, result$nivolumab)


})

