test_that("can read text file to arrow format", {
  demo <- data.frame(f0= c("96548661   32194501051119460820")) |>
    dplyr::as_tibble()

  tmp_folder <- tempdir()

  path_base <- paste0(tmp_folder, "/")

  write.table(demo, file = paste0(path_base, "DEMO.txt"),
              row.names = FALSE,
              quote = FALSE,
              col.names = FALSE)

  demo_res <- reader("DEMO.txt", path_base) |>
    dplyr::collect()

  expect_equal(demo, demo_res)
})

