test_that("is_arrow() is TRUE only for arrow objects", {
  df <- data.frame(x = 1:3, y = c("a", "b", "c"))

  # not arrow
  expect_false(is_arrow(df))
  expect_false(is_arrow(data.table::as.data.table(df)))
  expect_false(is_arrow(1:3))
  expect_false(is_arrow(NULL))

  # in-memory arrow Table
  at <- arrow::as_arrow_table(df)
  expect_true(is_arrow(at))

  # arrow_dplyr_query (chained dplyr on arrow before collect/compute)
  expect_true(is_arrow(at |> dplyr::mutate(z = 1)))

  # on-disk arrow Dataset
  tmp <- file.path(tempdir(), "is_arrow_ds")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  arrow::write_parquet(df, file.path(tmp, "d.parquet"))
  expect_true(is_arrow(arrow::open_dataset(tmp)))
})
