test_that("counts are good", {

  s1 <-
    screen_adr(
    .data = adr_,
    meddra = meddra_,
    term_level = "pt"
  ) %>% dplyr::filter(term %in% c("Pneumonitis", "Diarrhoea", "Colitis"))

  s1_true <-
    data.table(
      term = c("Pneumonitis", "Diarrhoea", "Colitis"),
      n = c(92, 69, 32),
      percentage = c(92/750*100, 69/750*100, 32/750*100)
    )

  expect_equal(s1, s1_true)

  s2 <-
    screen_adr(
      .data = adr_,
      meddra = meddra_,
      term_level = "hlt"
    ) %>% dplyr::filter(term %in% c("Lower respiratory tract inflammatory and immunologic conditions", "Diarrhoea (excl infective)", "Colitis (excl infective)"))

  s2_true <-
    data.table(
      term = c("Lower respiratory tract inflammatory and immunologic conditions", "Diarrhoea (excl infective)", "Colitis (excl infective)"),
      n = c(92, 69, 40),
      percentage = c(92/750*100, 69/750*100, 40/750*100)
    )

  expect_equal(s2, s2_true)

  s3 <-
    screen_adr(
      .data = adr_,
      meddra = meddra_,
      term_level = "soc"
    ) %>% dplyr::filter(term %in% c("Gastrointestinal disorders",
                                    "Respiratory, thoracic and mediastinal disorders"))

  s3_true <-
    data.table(
      term = c("Respiratory, thoracic and mediastinal disorders", "Gastrointestinal disorders"),
      n = c(110, 104),
      percentage = c(110/750*100, 104/750*100)
    )

  expect_equal(s3, s3_true)

  s4 <-
    screen_adr(
      .data = adr_,
      meddra = meddra_,
      term_level = "hlgt"
    ) %>% dplyr::filter(term %in% c("Lower respiratory tract disorders (excl obstruction and infection)",
                                    "Gastrointestinal motility and defaecation conditions",
                                    "Gastrointestinal inflammatory conditions"))

  s4_true <-
    data.table(
      term = c("Lower respiratory tract disorders (excl obstruction and infection)",
                   "Gastrointestinal motility and defaecation conditions",
                   "Gastrointestinal inflammatory conditions"),
      n = c(103, 69, 46),
      percentage = c(103/750*100, 69/750*100, 46/750*100)
    )

  expect_equal(s4, s4_true)

})



test_that("screen_adr works with top_n", {
  result <- screen_adr(.data = adr_, meddra = meddra_, term_level = "pt", top_n = 3)
  expect_equal(nrow(result), 2)  # Ensure it returns only top 2 terms
  expect_true("Pneumonitis" %in% result$term && "Diarrhoea" %in% result$term && NA %in% result$term)  # Top 2 frequent terms are correct
})



# Test that screen_adr works with freq_threshold
test_that("screen_adr works with freq_threshold", {
  result <- screen_adr(.data = adr_, meddra = meddra_, term_level = "pt", freq_threshold = 0.05)
  expect_true(all(result$percentage >= 5))  # All terms meet the frequency threshold
  expect_true("Pneumonitis" %in% result$term || "Diarrhoea" %in% result$term)  # Ensure specific terms are present if they meet threshold
})



# Test that screen_adr handles both freq_threshold and top_n specified with a warning
test_that("screen_adr handles both freq_threshold and top_n specified with a warning", {
  expect_warning(
    result <- screen_adr(.data = adr_, meddra = meddra_, term_level = "pt", top_n = 3, freq_threshold = 0.1),
    "Both 'freq_threshold' and 'top_n' are specified. Only 'top_n' will be applied."
  )
  expect_equal(nrow(result), 3)  # Only top_n should be applied
  expect_true("Pneumonitis" %in% result$term && "Diarrhoea" %in% result$term)  # Ensure the top 3 terms are correct
})


# Test that screen_adr returns correct columns
test_that("screen_adr returns correct columns", {
  result <- screen_adr(.data = adr_, meddra = meddra_, term_level = "pt", top_n = 3)
  expect_true(all(c("term", "n", "percentage") %in% names(result)))  # Check if all expected columns are present
})

# Test that screen_adr returns empty data table for high freq_threshold
test_that("screen_adr returns empty data frame for high freq_threshold", {
  result <- screen_adr(.data = adr_, meddra = meddra_, term_level = "pt", freq_threshold = 1)
  expect_equal(nrow(result), 0)  # Should return empty data table if threshold is too high
})



# Additional edge cases
test_that("screen_adr handles term_level mismatch", {
  expect_error(screen_adr(.data = adr_, meddra = meddra_, term_level = "invalid_level"),
               "Invalid 'term_level' specified. Choose from 'soc', 'hlgt', 'hlt', 'pt', 'llt'.")
})
