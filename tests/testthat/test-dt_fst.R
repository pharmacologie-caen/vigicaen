library(here)

test_that("import is successful", {
  df <- data.table(a = 1:100, b = rnorm(100))

  path <- tempdir()

  fst::write_fst(x = df,
                path = paste0(path, "/", "df.fst")
                )

  # You may import the file directly to data.table format with dt_fst
  expect_warning({
    df2 <<- dt_fst(path, "df")
  })

  expect_equal(df, df2)
  expect_equal(class(df2), c("data.table", "data.frame"))
})


test_that("works with here syntax seemlessly", {
  df <- data.table(a = 1:100, b = rnorm(100))

  path <- tempdir()

  fst::write_fst(x = df,
                 path = paste0(path, "/", "df.fst")
  )

  here_path <- here::here(path)

  # You may import the file directly to data.table format with dt_fst
  expect_warning({
    df2 <<- dt_fst(here_path, "df")
  })

  expect_equal(df, df2)
  expect_equal(class(df2), c("data.table", "data.frame"))
})

test_that("works without name arg", {
  df <- data.table(a = 1:100, b = rnorm(100))

  path <- tempdir()

  fst::write_fst(x = df,
                 path = paste0(path, "\\", "df.fst")
  )

  # You may import the file directly to data.table format with dt_fst
  expect_warning({
    df2 <<- dt_fst(path_base = paste0(path, "\\", "df.fst"))
  })

  expect_equal(df, df2)
  expect_equal(class(df2), c("data.table", "data.frame"))

  # You don't need to specify .fst
  expect_warning({
    df3 <<- dt_fst(path_base = paste0(path, "\\", "df"))
  })

  expect_equal(df, df3)
  expect_equal(class(df3), c("data.table", "data.frame"))
})

