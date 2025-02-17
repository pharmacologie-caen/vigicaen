test_that("basic load of dt_parquet", {
  demo <-
    data.table::data.table(
      UMCReportId = c(1, 2, 3, 4),
      AgeGroup = c(1, 7, 7, 8)
    ) |>
    arrow::as_arrow_table()

  tmp_folder <- tempdir()
  path_data <- paste0(tmp_folder, "/")

  arrow::write_parquet(demo,
                       sink = paste0(path_data, "demo.parquet")
                       )

  # in memory
  demo_res <-
    dt_parquet(path_data, "demo")

  expect_equal(
    demo |> dplyr::collect(),
    demo_res
  )

  # out of memory

  demo_out <-
    dt_parquet(path_data, "demo", in_memory = FALSE)

  expect_equal(
    class(demo),
    class(demo_out)
  )
  # cannot perform full equal, since "pointer"s aren't the same.

  unlink(paste0(path_data, "demo.parquet"), recursive = TRUE)
})


test_that("alternative path syntaxes work",{
  demo <-
    data.table::data.table(
      UMCReportId = c(1, 2, 3, 4),
      AgeGroup = c(1, 7, 7, 8)
    ) |>
    arrow::as_arrow_table()

  tmp_folder <- tempdir()
  path_data <- paste0(tmp_folder, "/")

  arrow::write_parquet(demo,
                       sink = paste0(path_data, "demo_altsynt.parquet")
  )

  # null name
  demo_res_nullname <-
    dt_parquet(paste0(path_data, "/demo_altsynt"))

  expect_equal(
    demo |> dplyr::collect(),
    demo_res_nullname
  )

  # absence of / at the end of path_data

  path_data_no_slash <-
    gsub("/$", "", path_data)

  demo_res_ns <-
    dt_parquet(path_data_no_slash, "demo_altsynt")

  expect_equal(
    demo |> dplyr::collect(),
    demo_res_ns
  )


  unlink(paste0(path_data, "demo_altsynt.parquet"), recursive = TRUE)
})
