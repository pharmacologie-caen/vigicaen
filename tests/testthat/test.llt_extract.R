context("llt_extract")

test_that("there is no duplicate in extracted llts", {
  # Intended for the rechallenge data management
  llt_extraction_soc <-
    llt_extract_soc(
      "Colitis",
      term_level = "pt",
      meddra = ex_$meddra)

  llt_extraction_smq <-
    llt_extract_smq(
      "Embolic and thrombotic events, venous (SMQ)",
      smq_list_content = ex_$smq_list_content)

  expect_equal(length(llt_extraction_soc),
               length(unique(llt_extraction_soc)))
  expect_equal(length(llt_extraction_smq),
               length(unique(llt_extraction_smq)))
})
