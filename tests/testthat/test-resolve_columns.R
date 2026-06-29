# Unit tests for the internal resolve_columns() helper that backs tidy-select
# support in desc_facvar() and desc_cont().

# A fixture mixing character, double and integer columns, with names chosen so
# that the various name-pattern helpers resolve unambiguously.
rc_df <- function() {
  data.frame(
    sex    = c("m", "f", "m"),
    region = c("eu", "us", "eu"),
    age    = c(40, 50, 60),
    bmi    = c(20, 25, 30),
    n1     = c(1L, 2L, 3L),
    n2     = c(4L, 5L, 6L),
    stringsAsFactors = FALSE
  )
}

test_that("character input is returned untouched (legacy path)", {
  df <- rc_df()

  expect_identical(
    resolve_columns(df, rlang::quo(c("age", "bmi"))),
    c("age", "bmi")
  )

  # a single string
  expect_identical(resolve_columns(df, rlang::quo("age")), "age")

  # the helper itself does not validate: absent names are returned as-is so
  # the downstream checkers keep ownership of the error.
  expect_identical(resolve_columns(df, rlang::quo(c("nope"))), "nope")

  # column order from the character vector is preserved
  expect_identical(
    resolve_columns(df, rlang::quo(c("bmi", "age"))),
    c("bmi", "age")
  )
})

test_that("external character vectors use the legacy path without warning", {
  df <- rc_df()
  vars <- c("region", "age")

  expect_identical(
    expect_no_warning(resolve_columns(df, rlang::quo(vars))),
    c("region", "age")
  )
})

test_that("bare column names are resolved via tidy-select", {
  df <- rc_df()

  expect_identical(resolve_columns(df, rlang::quo(c(age, bmi))), c("age", "bmi"))
  # order is preserved
  expect_identical(resolve_columns(df, rlang::quo(c(bmi, age))), c("bmi", "age"))
  # a single bare name
  expect_identical(resolve_columns(df, rlang::quo(age)), "age")
})

test_that("integer positions are resolved via tidy-select", {
  df <- rc_df()

  expect_identical(resolve_columns(df, rlang::quo(3:4)), c("age", "bmi"))
  expect_identical(resolve_columns(df, rlang::quo(c(1, 3))), c("sex", "age"))
  expect_identical(
    resolve_columns(df, rlang::quo(tidyselect::last_col())),
    "n2"
  )
})

test_that("predicate helper where() respects column types", {
  df <- rc_df()

  expect_identical(
    resolve_columns(df, rlang::quo(tidyselect::where(is.character))),
    c("sex", "region")
  )
  expect_identical(
    resolve_columns(df, rlang::quo(tidyselect::where(is.double))),
    c("age", "bmi")
  )
  expect_identical(
    resolve_columns(df, rlang::quo(tidyselect::where(is.integer))),
    c("n1", "n2")
  )
  expect_identical(
    resolve_columns(df, rlang::quo(tidyselect::where(is.numeric))),
    c("age", "bmi", "n1", "n2")
  )
})

test_that("name-pattern helpers resolve correctly", {
  df <- rc_df()

  expect_identical(
    resolve_columns(df, rlang::quo(tidyselect::starts_with("n"))),
    c("n1", "n2")
  )
  expect_identical(
    resolve_columns(df, rlang::quo(tidyselect::ends_with("e"))),
    "age"
  )
  expect_identical(
    resolve_columns(df, rlang::quo(tidyselect::contains("gi"))),
    "region"
  )
  expect_identical(
    resolve_columns(df, rlang::quo(tidyselect::matches("^n[12]$"))),
    c("n1", "n2")
  )
  expect_identical(
    resolve_columns(df, rlang::quo(tidyselect::num_range("n", 1:2))),
    c("n1", "n2")
  )
})

test_that("everything(), complements and combinations work", {
  df <- rc_df()

  expect_identical(
    resolve_columns(df, rlang::quo(tidyselect::everything())),
    names(df)
  )
  # complement of a predicate
  expect_identical(
    resolve_columns(df, rlang::quo(!tidyselect::where(is.character))),
    c("age", "bmi", "n1", "n2")
  )
  # explicit de-selection
  expect_identical(
    resolve_columns(df, rlang::quo(c(-age, -bmi, -n1, -n2))),
    c("sex", "region")
  )
  # mixing a bare name with a helper
  expect_identical(
    resolve_columns(df, rlang::quo(c(age, tidyselect::starts_with("n")))),
    c("age", "n1", "n2")
  )
})

test_that("all_of() / any_of() resolve to the requested names", {
  df <- rc_df()
  vars <- c("age", "sex")

  expect_identical(
    suppressWarnings(resolve_columns(df, rlang::quo(tidyselect::all_of(vars)))),
    c("age", "sex")
  )
  # any_of() silently drops names that are absent
  expect_identical(
    suppressWarnings(
      resolve_columns(df, rlang::quo(tidyselect::any_of(c("age", "absent"))))
    ),
    "age"
  )
})

test_that("an empty selection yields character(0)", {
  df <- rc_df()

  expect_identical(
    resolve_columns(df, rlang::quo(tidyselect::starts_with("zzz"))),
    character(0)
  )
})

test_that("a bare, non-existent column raises an informative error", {
  df <- rc_df()

  expect_error(
    resolve_columns(df, rlang::quo(does_not_exist)),
    regexp = "does_not_exist"
  )
})

test_that("works on data.table input", {
  df <- data.table::as.data.table(rc_df())

  expect_identical(resolve_columns(df, rlang::quo(c("age", "bmi"))), c("age", "bmi"))
  expect_identical(
    resolve_columns(df, rlang::quo(tidyselect::where(is.double))),
    c("age", "bmi")
  )
  expect_identical(
    resolve_columns(df, rlang::quo(tidyselect::starts_with("n"))),
    c("n1", "n2")
  )
})

test_that("works on an in-memory arrow Table", {
  df <- arrow::as_arrow_table(rc_df())

  # character (legacy) path
  expect_identical(resolve_columns(df, rlang::quo(c("age", "bmi"))), c("age", "bmi"))
  # predicate helper needs the type-preserving prototype
  expect_identical(
    resolve_columns(df, rlang::quo(tidyselect::where(is.numeric))),
    c("age", "bmi", "n1", "n2")
  )
  expect_identical(
    resolve_columns(df, rlang::quo(tidyselect::starts_with("n"))),
    c("n1", "n2")
  )
})

test_that("works on an arrow_dplyr_query", {
  query <-
    arrow::as_arrow_table(rc_df()) |>
    dplyr::mutate(extra = 1)

  expect_identical(
    resolve_columns(query, rlang::quo(tidyselect::where(is.character))),
    c("sex", "region")
  )
  expect_identical(
    resolve_columns(query, rlang::quo(tidyselect::ends_with("a"))),
    "extra"
  )
})

test_that("works on an on-disk arrow Dataset", {
  skip_on_cran()

  df <- rc_df()
  tmp_folder <- file.path(tempdir(), "rc_dataset_test")
  dir.create(tmp_folder)
  on.exit(unlink(tmp_folder, recursive = TRUE), add = TRUE)

  arrow::write_parquet(df, file.path(tmp_folder, "d.parquet"))
  ds <- arrow::open_dataset(tmp_folder)

  expect_s3_class(ds, "Dataset")

  expect_identical(
    resolve_columns(ds, rlang::quo(c("age", "bmi"))),
    c("age", "bmi")
  )
  expect_identical(
    resolve_columns(ds, rlang::quo(tidyselect::where(is.numeric))),
    c("age", "bmi", "n1", "n2")
  )
})
