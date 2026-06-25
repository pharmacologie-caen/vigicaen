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

  result <- add_death(demo_test, out_data = out_test)

  expect_equal(ncol(result), ncol(demo_test) + 1L)
  expect_true("death" %in% names(result))

  # UMCReportId 1: in out, Seriousness == "1" → TRUE
  expect_true(result[result$UMCReportId == 1L, ]$death)
  # UMCReportId 2: in out, Seriousness != "1" → FALSE
  expect_false(result[result$UMCReportId == 2L, ]$death)
  # UMCReportId 3: in out, Seriousness == "-" → FALSE
  expect_false(result[result$UMCReportId == 3L, ]$death)
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
    add_death(
      arrow::as_arrow_table(demo_test),
      out_data = arrow::as_arrow_table(out_test)
    ) |>
    dplyr::collect()

  expect_equal(ncol(result_arrow), ncol(demo_test) + 1L)
  expect_true("death" %in% names(result_arrow))

  expect_true(result_arrow[result_arrow$UMCReportId == 1L, ]$death)
  expect_false(result_arrow[result_arrow$UMCReportId == 2L, ]$death)
  expect_false(result_arrow[result_arrow$UMCReportId == 3L, ]$death)
  expect_true(is.na(result_arrow[result_arrow$UMCReportId == 4L, ]$death))
})

test_that("add_death works with preloaded data", {

  result <- add_death(demo_, out_data = out_)

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

  result <- add_death(demo_test, out_data = out_test, col_name = "my_death")

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

  result <- add_serious(demo_test, out_data = out_test)

  expect_equal(ncol(result), ncol(demo_test) + 1L)
  expect_true("serious" %in% names(result))

  # UMCReportId 1: in out, Serious == "Y" → TRUE
  expect_true(result[result$UMCReportId == 1L, ]$serious)
  # UMCReportId 2: in out, Serious == "Y" → TRUE
  expect_true(result[result$UMCReportId == 2L, ]$serious)
  # UMCReportId 3: in out, Serious == "N" → FALSE
  expect_false(result[result$UMCReportId == 3L, ]$serious)
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
    add_serious(
      arrow::as_arrow_table(demo_test),
      out_data = arrow::as_arrow_table(out_test)
    ) |>
    dplyr::collect()

  expect_equal(ncol(result_arrow), ncol(demo_test) + 1L)
  expect_true("serious" %in% names(result_arrow))

  expect_true(result_arrow[result_arrow$UMCReportId == 1L, ]$serious)
  expect_true(result_arrow[result_arrow$UMCReportId == 2L, ]$serious)
  expect_false(result_arrow[result_arrow$UMCReportId == 3L, ]$serious)
  expect_true(is.na(result_arrow[result_arrow$UMCReportId == 4L, ]$serious))
})

test_that("add_serious works with preloaded data", {

  result <- add_serious(demo_, out_data = out_)

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

  result <- add_fup(demo_test, fup_data = fup_test)

  expect_equal(ncol(result), ncol(demo_test) + 1L)
  expect_true("fup" %in% names(result))

  # UMCReportId 1 and 2: in followup → 1
  expect_equal(result[result$UMCReportId == 1L, ]$fup, 1)
  expect_equal(result[result$UMCReportId == 2L, ]$fup, 1)
  # UMCReportId 3: not in followup → 0
  expect_equal(result[result$UMCReportId == 3L, ]$fup, 0)
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
    add_fup(
      arrow::as_arrow_table(demo_test),
      fup_data = arrow::as_arrow_table(fup_test)
    ) |>
    dplyr::collect()

  expect_equal(ncol(result_arrow), ncol(demo_test) + 1L)
  expect_true("fup" %in% names(result_arrow))

  expect_equal(result_arrow[result_arrow$UMCReportId == 1L, ]$fup, 1)
  expect_equal(result_arrow[result_arrow$UMCReportId == 2L, ]$fup, 1)
  expect_equal(result_arrow[result_arrow$UMCReportId == 3L, ]$fup, 0)
})

test_that("add_fup works with preloaded data", {

  result <- add_fup(demo_, fup_data = followup_)

  expect_equal(ncol(result), ncol(demo_) + 1L)
  expect_true("fup" %in% names(result))
  expect_true(sum(result$fup) > 0)
  expect_true(1 %in% result$fup)
  expect_true(0 %in% result$fup)
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

  result <- add_fup(demo_test, fup_data = fup_test, col_name = "follow_up")

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

  result <- add_death(demo_test, out_data = out_test)

  expect_equal(ncol(result), ncol(demo_test) + 1L)
  expect_true("death" %in% names(result))

  # UMCReportId 1: in out, Seriousness == "1" → TRUE
  expect_true(result[result$UMCReportId == 1L, ]$death)
  # UMCReportId 2: in out, Seriousness != "1" → FALSE
  expect_false(result[result$UMCReportId == 2L, ]$death)
  # UMCReportId 3: in out, Seriousness == "-" → FALSE
  expect_false(result[result$UMCReportId == 3L, ]$death)
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

  result_mem <-
    add_death(demo_test, out_data = out_test)

  result_arrow <-
    add_death(
      arrow::as_arrow_table(demo_test),
      out_data = arrow::as_arrow_table(out_test)
    ) |>
    dplyr::collect()

  expect_equal(result_mem, result_arrow)
})

test_that("add_death works with preloaded data", {

  result <- add_death(demo_, out_data = out_)

  expect_equal(ncol(result), ncol(demo_) + 1L)
  expect_true("death" %in% names(result))
  expect_true(sum(result$death, na.rm = TRUE) > 0)
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

  result <- add_death(demo_test, out_data = out_test, col_name = "my_death")

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

  result <- add_serious(demo_test, out_data = out_test)

  expect_equal(ncol(result), ncol(demo_test) + 1L)
  expect_true("serious" %in% names(result))

  # UMCReportId 1: in out, Serious == "Y" → TRUE
  expect_true(result[result$UMCReportId == 1L, ]$serious)
  # UMCReportId 2: in out, Serious == "Y" → TRUE
  expect_true(result[result$UMCReportId == 2L, ]$serious)
  # UMCReportId 3: in out, Serious == "N" → FALSE
  expect_false(result[result$UMCReportId == 3L, ]$serious)
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

  result_mem <-
    add_serious(demo_test, out_data = out_test)

  result_arrow <-
    add_serious(
      arrow::as_arrow_table(demo_test),
      out_data = arrow::as_arrow_table(out_test)
    ) |>
    dplyr::collect()

  expect_equal(result_mem, result_arrow)
})

test_that("add_serious works with preloaded data", {

  result <- add_serious(demo_, out_data = out_)

  expect_equal(ncol(result), ncol(demo_) + 1L)
  expect_true("serious" %in% names(result))
  expect_true(sum(result$serious, na.rm = TRUE) > 0)
})

test_that("add_serious raises error on wrong out_data", {

  expect_snapshot(
    error = TRUE,
    {
      add_serious(demo_, out_data = drug_)
    }
  )
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

  result <- add_fup(demo_test, fup_data = fup_test)

  expect_equal(ncol(result), ncol(demo_test) + 1L)
  expect_true("fup" %in% names(result))

  # UMCReportId 1 and 2: in followup → 1
  expect_equal(result[result$UMCReportId == 1L, ]$fup, 1)
  expect_equal(result[result$UMCReportId == 2L, ]$fup, 1)
  # UMCReportId 3: not in followup → 0
  expect_equal(result[result$UMCReportId == 3L, ]$fup, 0)
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

  result_mem <-
    add_fup(demo_test, fup_data = fup_test)

  result_arrow <-
    add_fup(
      arrow::as_arrow_table(demo_test),
      fup_data = arrow::as_arrow_table(fup_test)
    ) |>
    dplyr::collect()

  expect_equal(result_mem, result_arrow)
})

test_that("add_fup works with preloaded data", {

  result <- add_fup(demo_, fup_data = followup_)

  expect_equal(ncol(result), ncol(demo_) + 1L)
  expect_true("fup" %in% names(result))
  expect_true(sum(result$fup) > 0)
  expect_equal(unique(result$fup), c(1, 0))
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

  result <- add_fup(demo_test, fup_data = fup_test, col_name = "follow_up")

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
