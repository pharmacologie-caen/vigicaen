test_that("check_data_out validates outcome data structure", {

  out_valid <- data.frame(
    UMCReportId = 1L, Seriousness = "1", Serious = "Y"
  )

  expect_null(
    vigicaen:::check_data_out(out_valid, ".data")
  )

  expect_snapshot(
    error = TRUE,
    vigicaen:::check_data_out(data.frame(UMCReportId = 1L), ".data")
  )

  expect_snapshot(
    error = TRUE,
    vigicaen:::check_data_out(demo_, ".data")
  )
})

test_that("check_data_fup validates followup data structure", {

  fup_valid <- data.frame(
    UMCReportId = 1L, ReplacedUMCReportId = 2L
  )

  expect_null(
    vigicaen:::check_data_fup(fup_valid, ".data")
  )

  expect_snapshot(
    error = TRUE,
    vigicaen:::check_data_fup(data.frame(UMCReportId = 1L), ".data")
  )

  expect_snapshot(
    error = TRUE,
    vigicaen:::check_data_fup(out_, ".data")
  )
})

test_that("add_death works with in-memory tables", {

  out_test <- data.table::data.table(
    UMCReportId  = c(1L, 2L, 3L),
    Seriousness  = c("1", "6", "-"),
    Serious      = c("Y", "Y", "N")
  )

  demo_test <- data.table::data.table(
    UMCReportId  = c(1L, 2L, 3L, 4L),
    Region       = NA_character_,
    DateDatabase = NA_character_,
    Type         = NA_character_
  )

  result <- suppressMessages(add_death(demo_test, out_data = out_test))

  expect_equal(ncol(result), ncol(demo_test) + 1L)
  expect_true("death" %in% names(result))

  # UMCReportId 1: in out, Seriousness == "1" → 1
  expect_equal(result[result$UMCReportId == 1L, ]$death, 1L)
  # UMCReportId 2: in out, Seriousness != "1" → 0
  expect_equal(result[result$UMCReportId == 2L, ]$death, 0L)
  # UMCReportId 3: in out, Seriousness == "-" → 0
  expect_equal(result[result$UMCReportId == 3L, ]$death, 0L)
  # UMCReportId 4: not in out → NA
  expect_true(is.na(result[result$UMCReportId == 4L, ]$death))
})

test_that("add_death works with Arrow tables", {

  out_test <- data.table::data.table(
    UMCReportId  = c(1L, 2L, 3L),
    Seriousness  = c("1", "6", "-"),
    Serious      = c("Y", "Y", "N")
  )

  demo_test <- data.table::data.table(
    UMCReportId  = c(1L, 2L, 3L, 4L),
    Region       = NA_character_,
    DateDatabase = NA_character_,
    Type         = NA_character_
  )

  result_arrow <-
    suppressMessages(
      add_death(
        arrow::as_arrow_table(demo_test),
        out_data = arrow::as_arrow_table(out_test)
      )
    ) |>
    dplyr::collect()

  expect_equal(ncol(result_arrow), ncol(demo_test) + 1L)
  expect_true("death" %in% names(result_arrow))

  expect_equal(result_arrow[result_arrow$UMCReportId == 1L, ]$death, 1L)
  expect_equal(result_arrow[result_arrow$UMCReportId == 2L, ]$death, 0L)
  expect_equal(result_arrow[result_arrow$UMCReportId == 3L, ]$death, 0L)
  expect_true(is.na(result_arrow[result_arrow$UMCReportId == 4L, ]$death))
})

test_that("add_death works with preloaded data", {

  result <- suppressMessages(add_death(demo_, out_data = out_))

  expect_equal(ncol(result), ncol(demo_) + 1L)
  expect_true("death" %in% names(result))
  expect_true(sum(result$death, na.rm = TRUE) > 0)
  expect_true(any(is.na(result$death)))
})

test_that("add_death col_name argument works", {

  out_test <- data.table::data.table(
    UMCReportId  = c(1L, 2L),
    Seriousness  = c("1", "6"),
    Serious      = c("Y", "Y")
  )

  demo_test <- data.table::data.table(
    UMCReportId  = c(1L, 2L, 3L),
    Region       = NA_character_,
    DateDatabase = NA_character_,
    Type         = NA_character_
  )

  result <- suppressMessages(
    add_death(demo_test, out_data = out_test, col_name = "my_death")
  )

  expect_true("my_death" %in% names(result))
  expect_false("death" %in% names(result))
})

test_that("add_death raises error on wrong out_data", {

  expect_snapshot(
    error = TRUE,
    {
      add_death(demo_, out_data = adr_)
    }
  )
})

test_that("add_death raises error on ind .data", {

  expect_snapshot(
    error = TRUE,
    {
      suppressMessages(add_death(ind_, out_data = out_))
    }
  )
})

test_that("add_death works with drug, adr, and link as .data", {

  result_drug <- suppressMessages(add_death(drug_, out_data = out_))
  expect_equal(ncol(result_drug), ncol(drug_) + 1L)
  expect_true("death" %in% names(result_drug))

  result_adr <- suppressMessages(add_death(adr_, out_data = out_))
  expect_equal(ncol(result_adr), ncol(adr_) + 1L)
  expect_true("death" %in% names(result_adr))

  result_link <- suppressMessages(add_death(link_, out_data = out_))
  expect_equal(ncol(result_link), ncol(link_) + 1L)
  expect_true("death" %in% names(result_link))
})

test_that("add_serious works with in-memory tables", {

  out_test <- data.table::data.table(
    UMCReportId  = c(1L, 2L, 3L),
    Seriousness  = c("1", "6", "-"),
    Serious      = c("Y", "Y", "N")
  )

  demo_test <- data.table::data.table(
    UMCReportId  = c(1L, 2L, 3L, 4L),
    Region       = NA_character_,
    DateDatabase = NA_character_,
    Type         = NA_character_
  )

  result <- suppressMessages(add_serious(demo_test, out_data = out_test))

  expect_equal(ncol(result), ncol(demo_test) + 1L)
  expect_true("serious" %in% names(result))

  # UMCReportId 1: in out, Serious == "Y" → 1
  expect_equal(result[result$UMCReportId == 1L, ]$serious, 1L)
  # UMCReportId 2: in out, Serious == "Y" → 1
  expect_equal(result[result$UMCReportId == 2L, ]$serious, 1L)
  # UMCReportId 3: in out, Serious == "N" → 0
  expect_equal(result[result$UMCReportId == 3L, ]$serious, 0L)
  # UMCReportId 4: not in out → NA
  expect_true(is.na(result[result$UMCReportId == 4L, ]$serious))
})

test_that("add_serious works with Arrow tables", {

  out_test <- data.table::data.table(
    UMCReportId  = c(1L, 2L, 3L),
    Seriousness  = c("1", "6", "-"),
    Serious      = c("Y", "Y", "N")
  )

  demo_test <- data.table::data.table(
    UMCReportId  = c(1L, 2L, 3L, 4L),
    Region       = NA_character_,
    DateDatabase = NA_character_,
    Type         = NA_character_
  )

  result_arrow <-
    suppressMessages(
      add_serious(
        arrow::as_arrow_table(demo_test),
        out_data = arrow::as_arrow_table(out_test)
      )
    ) |>
    dplyr::collect()

  expect_equal(ncol(result_arrow), ncol(demo_test) + 1L)
  expect_true("serious" %in% names(result_arrow))

  expect_equal(result_arrow[result_arrow$UMCReportId == 1L, ]$serious, 1L)
  expect_equal(result_arrow[result_arrow$UMCReportId == 2L, ]$serious, 1L)
  expect_equal(result_arrow[result_arrow$UMCReportId == 3L, ]$serious, 0L)
  expect_true(is.na(result_arrow[result_arrow$UMCReportId == 4L, ]$serious))
})

test_that("add_serious works with preloaded data", {

  result <- suppressMessages(add_serious(demo_, out_data = out_))

  expect_equal(ncol(result), ncol(demo_) + 1L)
  expect_true("serious" %in% names(result))
  expect_true(sum(result$serious, na.rm = TRUE) > 0)
  expect_true(any(is.na(result$serious)))
})

test_that("add_serious raises error on wrong out_data", {

  expect_snapshot(
    error = TRUE,
    {
      add_serious(demo_, out_data = drug_)
    }
  )
})

test_that("add_serious raises error on ind .data", {

  expect_snapshot(
    error = TRUE,
    {
      suppressMessages(add_serious(ind_, out_data = out_))
    }
  )
})

test_that("add_serious works with drug, adr, and link as .data", {

  result_drug <- suppressMessages(add_serious(drug_, out_data = out_))
  expect_equal(ncol(result_drug), ncol(drug_) + 1L)
  expect_true("serious" %in% names(result_drug))

  result_adr <- suppressMessages(add_serious(adr_, out_data = out_))
  expect_equal(ncol(result_adr), ncol(adr_) + 1L)
  expect_true("serious" %in% names(result_adr))

  result_link <- suppressMessages(add_serious(link_, out_data = out_))
  expect_equal(ncol(result_link), ncol(link_) + 1L)
  expect_true("serious" %in% names(result_link))
})

test_that("add_fup works with in-memory tables", {

  fup_test <- data.table::data.table(
    UMCReportId         = c(1L, 2L),
    ReplacedUMCReportId = c(10L, 20L)
  )

  demo_test <- data.table::data.table(
    UMCReportId  = c(1L, 2L, 3L),
    Region       = NA_character_,
    DateDatabase = NA_character_,
    Type         = NA_character_
  )

  result <- suppressMessages(add_fup(demo_test, fup_data = fup_test))

  expect_equal(ncol(result), ncol(demo_test) + 1L)
  expect_true("fup" %in% names(result))

  # UMCReportId 1 and 2: in followup → 1
  expect_equal(result[result$UMCReportId == 1L, ]$fup, 1L)
  expect_equal(result[result$UMCReportId == 2L, ]$fup, 1L)
  # UMCReportId 3: not in followup → 0
  expect_equal(result[result$UMCReportId == 3L, ]$fup, 0L)
})

test_that("add_fup works with Arrow tables", {

  fup_test <- data.table::data.table(
    UMCReportId         = c(1L, 2L),
    ReplacedUMCReportId = c(10L, 20L)
  )

  demo_test <- data.table::data.table(
    UMCReportId  = c(1L, 2L, 3L),
    Region       = NA_character_,
    DateDatabase = NA_character_,
    Type         = NA_character_
  )

  result_arrow <-
    suppressMessages(
      add_fup(
        arrow::as_arrow_table(demo_test),
        fup_data = arrow::as_arrow_table(fup_test)
      )
    ) |>
    dplyr::collect()

  expect_equal(ncol(result_arrow), ncol(demo_test) + 1L)
  expect_true("fup" %in% names(result_arrow))

  expect_equal(result_arrow[result_arrow$UMCReportId == 1L, ]$fup, 1L)
  expect_equal(result_arrow[result_arrow$UMCReportId == 2L, ]$fup, 1L)
  expect_equal(result_arrow[result_arrow$UMCReportId == 3L, ]$fup, 0L)
})

test_that("add_fup works with preloaded data", {

  result <- suppressMessages(add_fup(demo_, fup_data = followup_))

  expect_equal(ncol(result), ncol(demo_) + 1L)
  expect_true("fup" %in% names(result))
  expect_true(sum(result$fup) > 0)
  expect_true(1L %in% result$fup)
  expect_true(0L %in% result$fup)
  expect_false(any(is.na(result$fup)))
})

test_that("add_fup col_name argument works", {

  fup_test <- data.table::data.table(
    UMCReportId         = c(1L, 2L),
    ReplacedUMCReportId = c(10L, 20L)
  )

  demo_test <- data.table::data.table(
    UMCReportId  = c(1L, 2L, 3L),
    Region       = NA_character_,
    DateDatabase = NA_character_,
    Type         = NA_character_
  )

  result <- suppressMessages(
    add_fup(demo_test, fup_data = fup_test, col_name = "follow_up")
  )

  expect_true("follow_up" %in% names(result))
  expect_false("fup" %in% names(result))
})

test_that("add_fup raises error on wrong fup_data", {

  expect_snapshot(
    error = TRUE,
    {
      add_fup(demo_, fup_data = out_)
    }
  )
})

test_that("add_fup raises error on ind .data", {

  expect_snapshot(
    error = TRUE,
    {
      suppressMessages(add_fup(ind_, fup_data = followup_))
    }
  )
})

test_that("add_fup works with drug, adr, and link as .data", {

  result_drug <- suppressMessages(add_fup(drug_, fup_data = followup_))
  expect_equal(ncol(result_drug), ncol(drug_) + 1L)
  expect_true("fup" %in% names(result_drug))

  result_adr <- suppressMessages(add_fup(adr_, fup_data = followup_))
  expect_equal(ncol(result_adr), ncol(adr_) + 1L)
  expect_true("fup" %in% names(result_adr))

  result_link <- suppressMessages(add_fup(link_, fup_data = followup_))
  expect_equal(ncol(result_link), ncol(link_) + 1L)
  expect_true("fup" %in% names(result_link))
})

