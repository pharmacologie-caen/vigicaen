test_that("basic use works", {

  tmp_folder <- tempdir()

  path_who <- paste0(tmp_folder, "/", "who", "/")

  if(!dir.exists(path_who))
    dir.create(path_who)

  create_ex_who_csv(path_who)

  expect_snapshot(
    tb_who(path_who = path_who, force = TRUE),
    transform =
      function(chr_line)
        stringr::str_replace(
          chr_line,
          "(?>=\\d{1,3}\\%\\s| ).*(?= \\|)",
          " percent, seconds"
        )
  )

  mp_res <- arrow::read_parquet(paste0(path_who, "mp.parquet"))

  table_true <-
    f_sets_who_pq()

  expect_equal(mp_res, table_true$mp |> dplyr::as_tibble())

  # no end slash to path_who

  path_who_no_slash <- paste0(tmp_folder, "/", "who_no_slash")

  if(!dir.exists(paste0(path_who_no_slash, "/")))
    dir.create(paste0(path_who_no_slash, "/"))

  create_ex_who_csv(path_who_no_slash)

  expect_snapshot(
    tb_who(path_who = path_who_no_slash,
           force = TRUE),
    transform =
      function(chr_line)
        stringr::str_replace(
          chr_line,
          "(?>=\\d{1,3}\\%\\s| ).*(?= \\|)",
          " percent, seconds"
        )
  )

  mp_res_ns <- arrow::read_parquet(paste0(path_who, "mp.parquet"))

  expect_equal(mp_res_ns, table_true$mp |> dplyr::as_tibble())

  unlink(tmp_folder, recursive = TRUE)
})

test_that("path_who exists before working on tables", {
  wrong_path <- "/a/wrong/filepath/"

  expect_snapshot(error = TRUE, cnd_class = TRUE,
    tb_who(path_who  = wrong_path, force = TRUE)
  )

  cnd <- rlang::catch_cnd(tb_who(path_who  = wrong_path, force = TRUE))

  expect_s3_class(cnd, "no_dir")
  expect_equal(cnd$dir, "path_who")
  expect_equal(cnd$wrong_dir, wrong_path)
})
