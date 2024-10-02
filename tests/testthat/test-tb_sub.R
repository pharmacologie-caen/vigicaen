test_that("basic use and here package works", {
  f_sets <-
    list(
      AgeGroup_Lx.txt = data.frame(f0 = c("1An age range             ")),
      Dechallenge_Lx.txt = data.frame(f0 = c("1Some drug action                                                                                                                                                                                                                                                ")),
      Dechallenge2_Lx.txt = data.frame(f0 = c("1Some outcome occurring                                                                                                                                                                                                                                          ")),
      Frequency_Lx.txt = data.frame(f0 = c("123Some frequency of administration                                                                                                                                                                                                                                ")),
      Gender_Lx.txt = data.frame(f0 = c("1Some gender                                                                                                                                                                                                                                                     ")),
      Notifier_Lx.txt = data.frame(f0 = c("1 Some notifier                                                                                                                                                                                                                                                   ")),
      Outcome_Lx.txt = data.frame(f0 = c("1Some outcome                                                                                                                                                                                                                                                    ")),
      Rechallenge_Lx.txt = data.frame(f0 = c("1A rechallenge action                                                            ")),
      Rechallenge2_Lx.txt = data.frame(f0 = c("1A reaction recurrence status                                    ")),
      Region_Lx.txt = data.frame(f0 = c("1A world region                                    ")),
      RepBasis_Lx.txt = data.frame(f0 = c("1A reputation basis                                ")),
      ReportType_Lx.txt = data.frame(f0 = c("1A type of report                                                                                                                                                                                                                                                ")),
      RouteOfAdm_Lx.txt = data.frame(f0 = c("1 A route of admnistration                                                        ")),
      Seriousness_Lx.txt = data.frame(f0 = c("1 Some seriousness criteria                                                                                                                                                                                                                                   ")),
      SizeUnit_Lx.txt = data.frame(f0 = c("1 A dosing unit                                                                  "))
    )

  tmp_folder <- tempdir()

  path_test <- paste0(tmp_folder, "/", "test", "/")

  if(!dir.exists(path_test))
    dir.create(path_test)


  purrr::iwalk(f_sets, function(d_, name_){
    write.table(d_, file = paste0(path_test, name_), row.names = FALSE, quote = FALSE, col.names = FALSE)
    })

  expect_snapshot(tb_sub(path_test))

  age_group_res <-
    arrow::read_parquet(paste0(path_test, "AgeGroup.parquet"),
                        mmap = FALSE)

  dechallenge_res <-
    arrow::read_parquet(paste0(path_test, "Dechallenge.parquet"),
                        mmap = FALSE)

  dechallenge2_res <-
    arrow::read_parquet(paste0(path_test, "Dechallenge2.parquet"),
                        mmap = FALSE)

  frequency_res <-
    arrow::read_parquet(paste0(path_test, "Frequency.parquet"),
                        mmap = FALSE)

  gender_res <-
    arrow::read_parquet(paste0(path_test, "Gender.parquet"),
                        mmap = FALSE)

  notifier_res <-
    arrow::read_parquet(paste0(path_test, "Notifier.parquet"),
                        mmap = FALSE)

  outcome_res <-
    arrow::read_parquet(paste0(path_test, "Outcome.parquet"),
                        mmap = FALSE)

  rechallenge_res <-
    arrow::read_parquet(paste0(path_test, "Rechallenge.parquet"),
                        mmap = FALSE)

  rechallenge2_res <-
    arrow::read_parquet(paste0(path_test, "Rechallenge2.parquet"),
                        mmap = FALSE)

  region_res <-
    arrow::read_parquet(paste0(path_test, "Region.parquet"),
                        mmap = FALSE)

  rep_basis_res <-
    arrow::read_parquet(paste0(path_test, "RepBasis.parquet"),
                        mmap = FALSE)

  report_type_res <-
    arrow::read_parquet(paste0(path_test, "ReportType.parquet"),
                        mmap = FALSE)

  route_of_adm_res <-
    arrow::read_parquet(paste0(path_test, "RouteOfAdm.parquet"),
                        mmap = FALSE)

  seriousness_res <-
    arrow::read_parquet(paste0(path_test, "Seriousness.parquet"),
                        mmap = FALSE)

  size_unit_res <-
    arrow::read_parquet(paste0(path_test, "SizeUnit.parquet"),
                        mmap = FALSE)

  expect_equal(age_group_res,
             dplyr::tibble(
               AgeGroup = "1",
               Code = "An age range")
             )

  expect_equal(dechallenge_res,
               dplyr::tibble(
                 Dechallenge1 = "1",
                 Code = "Some drug action")
               )

  expect_equal(dechallenge2_res,
               dplyr::tibble(
                 Dechallenge2 = "1",
                 Code = "Some outcome occurring")
               )

  expect_equal(frequency_res,
               dplyr::tibble(
                 FrequencyU = "123",
                 Code = "Some frequency of administration")
               )

  expect_equal(gender_res,
               dplyr::tibble(
                 Gender = "1",
                 Code = "Some gender")
               )

  expect_equal(notifier_res,
               dplyr::tibble(
                 Type = 1L,
                 Code = "Some notifier")
  )

  expect_equal(outcome_res,
               dplyr::tibble(
                 Outcome = "1",
                 Code = "Some outcome")
  )

  expect_equal(rechallenge_res,
               dplyr::tibble(
                 Rechallenge1 = "1",
                 Code = "A rechallenge action")
  )

  expect_equal(rechallenge2_res,
               dplyr::tibble(
                 Rechallenge2 = "1",
                 Code = "A reaction recurrence status")
  )

  expect_equal(region_res,
               dplyr::tibble(
                 Region = "1",
                 Code = "A world region")
  )

  expect_equal(rep_basis_res,
               dplyr::tibble(
                 Basis = "1",
                 Code = "A reputation basis")
  )

  expect_equal(report_type_res,
               dplyr::tibble(
                 ReportType = "1",
                 Code = "A type of report")
  )

  expect_equal(route_of_adm_res,
               dplyr::tibble(
                 Route = 1L,
                 Code = "A route of admnistration")
  )

  expect_equal(seriousness_res,
               dplyr::tibble(
                 Seriousness = 1L,
                 Code = "Some seriousness criteria")
  )

  expect_equal(size_unit_res,
               dplyr::tibble(
                 AmountU = 1L,
                 Code = "A dosing unit")
  )

  unlink(tmp_folder, recursive = TRUE)

})

