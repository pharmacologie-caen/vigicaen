test_that("written data is unchanged when loaded back", {

  path <- paste0(tempdir(), "/test_create_example_tables/")

  dir.create(path)

  # from csv ----

  create_ex_main_csv(path)

  name_main_upper <- names(f_sets_main()) |>
    stringr::str_to_upper()

  name_main_csv <- paste0(name_main_upper, ".csv")

  name_main_csv <-
    ifelse(
      name_main_csv == "SUSPDUP.csv",
      "SUSPECTEDDUPLICATES.csv",
      name_main_csv
    )

  main_reimport_csv <-
    name_main_csv |>
    rlang::set_names(names(f_sets_main())) |>
    purrr::map(function(tab_)
      reader(tab_, path) |> dplyr::collect() |>
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
    )

  # beware that SRCE is exported both from main and who

  expect_equal(
    main_reimport_csv,
    purrr::map(f_sets_main(), function(t_){
      n_col_names <- ncol(t_) - 1
      t_name <- names(t_) |>
        rlang::set_names(paste0("f", 0:n_col_names))
      t_ |> dplyr::as_tibble() |>
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
        # csv reading looses numeric/character type in case of numeric input
        dplyr::rename(dplyr::all_of(t_name))
    }
    )
  )

  # sub

  create_ex_sub_csv(path)

  name_sub_upper <- names(f_sets_sub())
  name_sub_csv <- paste0(name_sub_upper, ".csv")

  sub_reimport_csv <-
    name_sub_csv |>
    rlang::set_names(names(f_sets_sub())) |>
    purrr::map(function(tab_)
      reader(tab_, path) |> dplyr::collect() |>
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
    )

  expect_equal(
    sub_reimport_csv,
    purrr::map(f_sets_sub(), function(t_){
      n_col_names <- ncol(t_) - 1
      t_name <- names(t_) |>
        rlang::set_names(paste0("f", 0:n_col_names))
      t_ |> dplyr::as_tibble() |>
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
        # csv reading looses numeric/character type in case of numeric input
        dplyr::rename(dplyr::all_of(t_name))
    }
    )
  )

  # who

  create_ex_who_csv(path)

  name_who_upper <- stringr::str_to_upper(names(f_sets_who()))
  name_who_csv <- paste0(name_who_upper, ".csv")

  name_who_csv <-
    ifelse(
      name_who_csv == "UNITX.csv",
      "Unit-X.csv",
      ifelse(
        name_who_csv == "THG.csv",
        "ThG.csv",
        name_who_csv
      )
    )

  who_reimport_csv <-
    name_who_csv |>
    rlang::set_names(names(f_sets_who())) |>
    purrr::map(function(tab_)
      reader(tab_, path) |> dplyr::collect() |>
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
    )

  expect_equal(
    who_reimport_csv,
    purrr::map(f_sets_who(), function(t_){
      n_col_names <- ncol(t_) - 1
      t_name <- names(t_) |>
        rlang::set_names(paste0("f", 0:n_col_names))
      t_ |> dplyr::as_tibble() |>
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
        # csv reading looses numeric/character type in case of numeric input
        dplyr::rename(dplyr::all_of(t_name))
    }
    )
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
  expected <- f_sets_sub_pq() |>
    rlang::set_names(~ stringr::str_replace(.x, "_Lx$", ""))

  for (name in names(expected)) {
    pq_file <- file.path(path, paste0(name, ".parquet"))
    expect_true(file.exists(pq_file),
      info = paste("File does not exist:", pq_file))
    tbl <- dt_parquet(path, name)
    expect_equal(names(tbl), names(expected[[name]]),
      info = paste("Column names do not match for table:", name))
    expect_equal(sapply(tbl, class), sapply(expected[[name]], class),
      info = paste("Column types do not match for table:", name))
  }

  unlink(path, recursive = TRUE)
})
