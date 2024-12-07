test_that("counts are good", {

  s1 <- screen_adr(
    .data = adr_,
    meddra = meddra_,
    term_level = "pt"
  ) %>% dplyr::filter(term %in% c("Pneumonitis", "Diarrhoea", "Colitis"))

  s1_true <- data.table(
    term = c("Pneumonitis", "Diarrhoea", "Colitis"),
    n = c(92, 69, 32),
    percentage = c(92/750*100, 69/750*100, 32/750*100)
  )

  expect_equal(s1, s1_true)

  s2 <- screen_adr(
    .data = adr_,
    meddra = meddra_,
    term_level = "hlt"
  ) %>% dplyr::filter(term %in% c(
    "Lower respiratory tract inflammatory and immunologic conditions",
    "Diarrhoea (excl infective)", "Colitis (excl infective)"
  ))

  s2_true <- data.table(
    term = c("Lower respiratory tract inflammatory and immunologic conditions",
             "Diarrhoea (excl infective)", "Colitis (excl infective)"),
    n = c(92, 69, 40),
    percentage = c(92/750*100, 69/750*100, 40/750*100)
  )

  expect_equal(s2, s2_true)

  s3 <- screen_adr(
    .data = adr_,
    meddra = meddra_,
    term_level = "soc"
  ) %>% dplyr::filter(term %in% c(
    "Gastrointestinal disorders", "Respiratory, thoracic and mediastinal disorders"
  ))

  s3_true <- data.table(
    term = c("Respiratory, thoracic and mediastinal disorders", "Gastrointestinal disorders"),
    n = c(110, 104),
    percentage = c(110/750*100, 104/750*100)
  )

  expect_equal(s3, s3_true)

  s4 <- screen_adr(
    .data = adr_,
    meddra = meddra_,
    term_level = "hlgt"
  ) %>% dplyr::filter(term %in% c(
    "Lower respiratory tract disorders (excl obstruction and infection)",
    "Gastrointestinal motility and defaecation conditions",
    "Gastrointestinal inflammatory conditions"
  ))

  s4_true <- data.table(
    term = c("Lower respiratory tract disorders (excl obstruction and infection)",
             "Gastrointestinal motility and defaecation conditions",
             "Gastrointestinal inflammatory conditions"),
    n = c(103, 69, 46),
    percentage = c(103/750*100, 69/750*100, 46/750*100)
  )

  expect_equal(s4, s4_true)

})

test_that("screen_adr works with top_n", {
  result <- screen_adr(
    .data = adr_, meddra = meddra_, term_level = "pt", top_n = 3
  )
  expect_equal(nrow(result), 3)  # Ensure it returns only top 3 terms
  expect_true("Pneumonitis" %in% result$term && "Diarrhoea" %in% result$term &&
                NA %in% result$term)  # Top 2 frequent terms are correct
})

test_that("screen_adr works with freq_threshold", {
  result <- screen_adr(
    .data = adr_, meddra = meddra_, term_level = "pt", freq_threshold = 0.05
  )
  expect_true(all(result$percentage >= 5))  # All terms meet the frequency threshold
  expect_true("Pneumonitis" %in% result$term || "Diarrhoea" %in% result$term)  # Ensure
  # specific terms are present if they meet threshold
})

test_that("screen_adr handles both freq_threshold and top_n specified with a warning", {
  expect_warning(
    result <- screen_adr(
      .data = adr_, meddra = meddra_, term_level = "pt", top_n = 3, freq_threshold = 0.1
    ),
    "Both 'freq_threshold' and 'top_n' are specified. Only 'top_n' will be applied."
  )
  expect_equal(nrow(result), 3)  # Only top_n should be applied
  expect_true("Pneumonitis" %in% result$term && "Diarrhoea" %in% result$term)  # Ensure
  # the top 3 terms are correct
})

test_that("screen_adr returns correct columns", {
  result <- screen_adr(
    .data = adr_, meddra = meddra_, term_level = "pt", top_n = 3
  )
  expect_true(all(c("term", "n", "percentage") %in% names(result)))  # Check if all
  # expected columns are present
})

test_that("screen_adr returns empty data table for high freq_threshold", {
  result <- screen_adr(
    .data = adr_, meddra = meddra_, term_level = "pt", freq_threshold = 1
  )
  expect_equal(nrow(result), 0)  # Should return empty data table if threshold is too high
})

test_that("screen_adr handles term_level mismatch", {
  expect_error(
    screen_adr(
      .data = adr_, meddra = meddra_, term_level = "invalid_level"
    ),
    "Invalid 'term_level' specified. Choose from 'soc', 'hlgt', 'hlt', 'pt', 'llt'."
  )
})

test_that("result is exact with low frequency ae compounds", {
  adr_test <- data.frame(
    UMCReportId = 1:50,
    MedDRA_Id = c(rep(1,49), 2)
  )

  meddra_test <- data.frame(
    hlt_name = c("ae", "ae"),
    llt_code = c(1, 2)
  )

  s_test <- screen_adr(
    .data = adr_test,
    meddra = meddra_test,
    term_level = "hlt",
    freq_threshold = 0.05
  )

  s_true <- data.table(
    term = "ae",
    n = 50,
    percentage = 100
  )

  expect_equal(s_test, s_true)
})



test_that("result is correct with double adr entries per case", {
  adr_test <- data.frame(
    UMCReportId = c(1, 1, 2),
    MedDRA_Id = c(1, 1, 2)
  )

  meddra_test <- data.frame(
    hlt_name = c("ae", "other"),
    llt_code = c(1, 2)
  )

  s_test2 <- screen_adr(
    .data = adr_test,
    meddra = meddra_test,
    term_level = "hlt",
    freq_threshold = 0.05
  )

  s_true2 <- data.table(
    term = c("ae", "other"),
    n = c(1, 1),
    percentage = c(50, 50)
  )

  expect_equal(s_test2, s_true2)
})

test_that("multiple entries in meddra do not count several folds", {
  adr_test <- data.frame(
    UMCReportId = c(1, 2),
    MedDRA_Id = c(1, 2)
  )

  meddra_test <- data.frame(
    hlt_name = c("ae1", "ae1", "ae2"),
    llt_code = c(1, 1, 2)
  )

  s_test <-
    screen_adr(
      .data = adr_test,
      meddra = meddra_test,
      term_level = "hlt",
      freq_threshold = 0.05
    )

  s_true <- data.table(
    term = c("ae1", "ae2"),
    n = c(1, 1),
    percentage = c(50, 50)
  )

  expect_equal(s_test, s_true)

})

test_that("unused llt codes don't show up in output", {
  adr_test <- data.frame(
    UMCReportId = c(1, 2),
    MedDRA_Id = c(1, 2)
  )

  meddra_test <- data.frame(
    hlt_name = c("ae1", "ae2", "ae3"),
    llt_code = c(1, 2, 3)
  )

  s_test <-
    screen_adr(
      .data = adr_test,
      meddra = meddra_test,
      term_level = "hlt"
    )

  s_true <- data.table(
    term = c("ae1", "ae2"),
    n = c(1, 1),
    percentage = c(50, 50)
  )

  expect_equal(s_test, s_true)
})

test_that("an llt code used in two hlt codes is counted twice", {
  adr_test <- data.frame(
    UMCReportId = c(1, 2, 2),
    MedDRA_Id = c(1, 1, 2)
  )

  meddra_test <- data.frame(
    hlt_name = c("ae1", "ae2", "ae3"),
    llt_code = c(1, 1, 2)
  )

  s_test <-
    screen_adr(
      .data = adr_test,
      meddra = meddra_test,
      term_level = "hlt"
    )

  s_true <- data.table(
    term = c("ae1", "ae2", "ae3"),
    n = c(2, 2, 1),
    percentage = c(100, 100, 50)
  )

  expect_equal(s_test, s_true)

  # consequence: sum(n) is larger than number of cases

  expect_true(
    sum(s_test$n) > length(unique(adr_test$UMCReportId))
  )

  # but still, no unique n is larger than number of cases

  expect_true(
    all(s_test$n <= length(unique(adr_test$UMCReportId)))
    )
})

test_that("works with arrow Tables", {
  adr_test <- data.table(
    UMCReportId = c(1, 2, 3, 4),
    MedDRA_Id = c(1, 1, 2, 3)
  ) |>
    arrow::as_arrow_table()

  meddra_test <- data.table(
    hlt_name = c("ae1", "ae2", "ae3"),
    llt_code = c(1, 2, 3)
  ) |>
    arrow::as_arrow_table()

  s_test <-
    screen_adr(
      .data = adr_test,
      meddra = meddra_test,
      term_level = "hlt"
    )

  s_true <- data.table(
    term = c("ae1", "ae2", "ae3"),
    n = c(2, 1, 1),
    percentage = c(50, 25, 25)
  )

  expect_equal(s_test, s_true)
})
