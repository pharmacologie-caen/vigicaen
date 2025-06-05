test_that("written data is unchanged when loaded back", {

  path <- paste0(tempdir(), "/test_create_example_tables/")

  dir.create(path)

  create_ex_main_txt(path)

  main_reimport <-
    names(f_sets_main()) |>
    rlang::set_names() |>
    purrr::map(function(tab_)
      reader(tab_, path) |> dplyr::collect()
    )

  # beware that SRCE is exported both from main and who

  expect_equal(
    main_reimport,
    purrr::map(f_sets_main(), dplyr::as_tibble)
  )

  # sub

  create_ex_sub_txt(path)

  sub_reimport <-
    names(f_sets_sub()) |>
    rlang::set_names() |>
    purrr::map(function(tab_)
      reader(tab_, path) |> dplyr::collect()
    )

  expect_equal(
    sub_reimport,
    purrr::map(f_sets_sub(), dplyr::as_tibble)
  )

  # who

  create_ex_who_txt(path)

  who_reimport <-
    names(f_sets_who()) |>
    rlang::set_names() |>
    purrr::map(function(tab_)
      reader(tab_, path) |> dplyr::collect()
    )

  expect_equal(
    who_reimport,
    purrr::map(f_sets_who(), dplyr::as_tibble)
  )

  # meddra

  create_ex_meddra_asc(path)

  meddra_reimport <-
    names(f_sets_meddra()) |>
    rlang::set_names() |>
    purrr::map(function(tab_)
      read.table(file = paste0(path, "/", tab_), sep = "\t") |>
        rlang::set_names("f0")
      )

  expect_equal(
    meddra_reimport,
    f_sets_meddra()
  )

  # main in parquet

  create_ex_main_pq(path)

  main_reimport_pq <-
    names(f_sets_main_pq()) |>
    rlang::set_names() |>
    purrr::map(function(tab_)
      dt_parquet(path, tab_)
    )

  expect_equal(
    main_reimport_pq,
    f_sets_main_pq()
  )

  unlink(path, recursive = TRUE)
})

test_that("create_ex_sub_pq writes all expected subsidiary parquet tables with correct structure and types", {
  path <- paste0(tempdir(), "/test_create_ex_sub_pq/")
  dir.create(path)

  create_ex_sub_pq(path)
  expected <- f_sets_sub_pq()

  for (name in names(expected)) {
    pq_file <- file.path(path, paste0(name, ".parquet"))
    expect_true(file.exists(pq_file),
      info = paste("File does not exist:", pq_file))
    tbl <- dt_parquet(path, name)
    expect_equal(names(tbl), names(expected[[name]]),
      info = paste("Column names do not match for table:", name))
    expect_equal(sapply(tbl, class), sapply(expected[[name]], class),
      info = paste("Column types do not match for table:", name))
    # Check first column is integer type
    expect_true(any(class(tbl[[1]]) %in% c("integer", "integer64")),
      info = paste("First column is not integer for table:", name))
  }

  unlink(path, recursive = TRUE)
})
