test_that("dt_fst is deprecated", {

  # df <- data.frame(a = 1:100, b = rnorm(100))
  #
  # path <- tempdir()
  #
  # fst::write_fst(x = df,
  #               path = paste0(path, "/", "df.fst")
  #               )

  expect_snapshot(
    dt_fst(path, "df"),
    error = TRUE
  )

  # unlink( paste0(path, "/", "df.fst"))
})
