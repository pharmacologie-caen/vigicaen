test_that("you can subset on drecno, age, meddra_id", {

  wd_in <- tempdir()

  wd_in <- paste0(wd_in, "\\", "tb_subset_t1") # avoid windows #1224
  # never rewrite on the same tempfile in ANY test.

  dir.create(path = wd_in)

  mini_data <- rlang::list2(
    demo =
      data.table(
        UMCReportId = c(1, 2, 3, 4),
        AgeGroup = c(1, 2, 7, 9)
      ),
       drug =
         data.table(
           UMCReportId = c(1, 2, 3, 4),
           Drug_Id = c("d1", "d2", "d3", "d4"),
           DrecNo = c(133138448, 133138448, 111841511, 111841511),
           MedicinalProd_Id = c(25027716, 97354576, 104264760, 37484408)
         ),
       adr  =
         data.table(
           UMCReportId = c(1, 2, 3, 4),
           Adr_Id = c("a1", "a2", "a3", "a4"),
           MedDRA_Id = c(110049083, 146319904, 146319904, 72535511)
         ),
    link =
      data.table(
        Drug_Id = c("d1", "d2", "d3", "d4"),
        Adr_Id = c("a1", "a2", "a3", "a4")
      ),
    srce =
      data.table(
        UMCReportId = c(1, 2, 3, 4)
      ),
    ind  =
      data.table(
        Drug_Id = c("d1", "d2", "d3", "d4")
      ),
    out  =
      data.table(
        UMCReportId = c(1, 2, 3, 4)
      ),
    followup =
      data.table(
        UMCReportId = c(1, 2, 3, 4)
      ),
    suspdup =
      data.table(
        UMCReportId = c(3),
        SuspectedduplicateReportId = c(4)
      )
  )

     purrr::iwalk(
       mini_data,
       function(dataset, name)
         arrow::write_parquet(
           dataset |>
             arrow::as_arrow_table(),
           sink = paste0(wd_in, "/", name, ".parquet")
         )

     )

  # ---- drecno

  sv_selection_drecno <-
    list(ipi = c(133138448, 133138448))

  expect_snapshot(
    tb_subset(
      wd_in = paste0(wd_in, "/"),
      wd_out = paste0(wd_in, "/", "subset_drecno", "/"),
      subset_var = "drecno",
      sv_selection = sv_selection_drecno
    ),
    transform =
      function(chr_line)
        stringr::str_replace(
          chr_line,
          "(?>=\\d{1,3}\\%\\s| ).*(?= \\|)",
          " percent, seconds"
        )
  )


  drug_sub <-
    dt_parquet(paste0(wd_in, "/", "subset_drecno", "/"), "drug")

  demo_sub <-
    dt_parquet(paste0(wd_in, "/", "subset_drecno", "/"), "demo")

  expect_equal(
    sort(unique(demo_sub$UMCReportId)),
    sort(unique(drug_sub$UMCReportId))
    )

  para_test <-
    drug_sub %>%
    dplyr::group_by(UMCReportId) %>%
    dplyr::summarise(
      has_para = max(DrecNo %in% sv_selection_drecno$ipi)
    )

  expect_equal(
    all(para_test$has_para == 1),
    TRUE
  )

  # --- drecno several items

  sv_selection_drecno_item <-
    list(
      ipi = c(133138448),
      nivo = c(111841511)
    )

  expect_snapshot(tb_subset(
    wd_in = paste0(wd_in, "/"),
    wd_out = paste0(wd_in, "/", "subset_drecno_item", "/"),
    subset_var = "drecno",
    sv_selection = sv_selection_drecno_item
  ),
  transform =
    function(chr_line)
      stringr::str_replace(
        chr_line,
        "(?>=\\d{1,3}\\%\\s| ).*(?= \\|)",
        " percent, seconds"
      ))

  demo_sub_item <-
    dt_parquet(paste0(wd_in, "/", "subset_drecno_item", "/"), "demo")

  drug_sub_item <-
    dt_parquet(paste0(wd_in, "/", "subset_drecno_item", "/"), "drug")

  expect_equal(
    nrow(demo_sub_item),
    3 # one duplicate
  )

  # ---- age

  expect_snapshot(tb_subset(
    wd_in = paste0(wd_in, "/"),
    wd_out = paste0(wd_in, "/", "subset_age", "/"),
    subset_var = "age",
    sv_selection = c(7, 8)
  ),
  transform =
    function(chr_line)
      stringr::str_replace(
        chr_line,
        "(?>=\\d{1,3}\\%\\s| ).*(?= \\|)",
        " percent, seconds"
      ))

  drug_sub <-
    dt_parquet(paste0(wd_in, "/", "subset_age", "/"), "drug")

  demo_sub <-
    dt_parquet(paste0(wd_in, "/", "subset_age", "/"), "demo")

  expect_equal(
    sort(unique(demo_sub$UMCReportId)),
    sort(unique(drug_sub$UMCReportId))
  )

  age_test <-
    demo_sub %>%
    dplyr::mutate(
      good_age = AgeGroup %in% c(7, 8)
    )

  expect_equal(
    all(age_test$good_age == 1),
    TRUE
  )

  # ---- adr

  sv_selection_mid <-
    list(colitis = c(146319904, 72535511))

  wd_out <- paste0(wd_in, "/", "subset_meddraid", "/")

  expect_snapshot(tb_subset(
    wd_in = paste0(wd_in, "/"),
    wd_out = paste0(wd_in, "/", "subset_meddraid", "/"),
    subset_var = "meddra_id",
    sv_selection = sv_selection_mid
  ),
  transform =
    function(chr_line)
      stringr::str_replace(
        chr_line,
        "(?>=\\d{1,3}\\%\\s| ).*(?= \\|)",
        " percent, seconds"
      ))

  adr_sub <-
    dt_parquet(paste0(wd_in, "/", "subset_meddraid", "/"), "adr")

  demo_sub <-
    dt_parquet(paste0(wd_in, "/", "subset_meddraid", "/"), "demo")

  expect_equal(
    sort(unique(demo_sub$UMCReportId)),
    sort(unique(adr_sub$UMCReportId))
  )

  colitis_test <-
    adr_sub %>%
    dplyr::group_by(UMCReportId) %>%
    dplyr::summarise(
      has_colitis = max(MedDRA_Id %in% sv_selection_mid$colitis)
    )

  expect_equal(
    all(colitis_test$has_colitis == 1),
    TRUE
  )

  unlink(wd_in, recursive = TRUE)

})

test_that("wd_in exists", {
  expect_error(
    tb_subset(
      wd_in = "that_dir_doesnt_exists",
      sv_selection = list(a = c(1, 2))
      ),
    "that_dir_doesnt_exists was not found, check spelling and availability."
  )
})

test_that("you can keep suspdup", {

  wd_in <- tempdir()

  wd_in <- paste0(wd_in, "\\", "sub") # avoid windows #1224

  dir.create(path = wd_in)

  mini_data <- rlang::list2(
    demo =
      data.table(
        UMCReportId = c(1, 2, 3, 4),
        AgeGroup = c(1, 7, 7, 8)
      ),
    drug =
      data.table(
        UMCReportId = c(1, 2, 3, 4),
        Drug_Id = c("d1", "d2", "d3", "d4"),
        DrecNo = c(133138448, 133138448, 111841511, 111841511),
        MedicinalProd_Id = c(25027716, 97354576, 104264760, 37484408)
      ),
    adr  =
      data.table(
        UMCReportId = c(1, 2, 3, 4),
        Adr_Id = c("a1", "a2", "a3", "a4"),
        MedDRA_Id = c(110049083, 31672047, 146319904, 72535511)
      ),
    link =
      data.table(
        Drug_Id = c("d1", "d2", "d3", "d4"),
        Adr_Id = c("a1", "a2", "a3", "a4")
      ),
    srce =
      data.table(
        UMCReportId = c(1, 2, 3, 4)
      ),
    ind  =
      data.table(
        Drug_Id = c("d1", "d2", "d3", "d4")
      ),
    out  =
      data.table(
        UMCReportId = c(1, 2, 3, 4)
      ),
    followup =
      data.table(
        UMCReportId = c(1, 2, 3, 4)
      ),
    suspdup =
      data.table(
        UMCReportId = c(3),
        SuspectedduplicateReportId = c(4)
      )
  )

  purrr::iwalk(
    mini_data,
    function(dataset, name)
      arrow::write_parquet(
        dataset |>
          arrow::as_arrow_table(),
        sink = paste0(wd_in, "/", name, ".parquet")
      )

  )

  # ---- age

  expect_snapshot(tb_subset(
    wd_in = paste0(wd_in, "/"),
    wd_out = paste0(wd_in, "/", "subset_age_suspdup", "/"),
    subset_var = "age",
    sv_selection = c(7, 8),
    rm_suspdup = FALSE
  ),
  transform =
    function(chr_line)
      stringr::str_replace(
        chr_line,
        "(?>=\\d{1,3}\\%\\s| ).*(?= \\|)",
        " percent, seconds"
      ))

  expect_snapshot(tb_subset(
    wd_in = paste0(wd_in, "/"),
    wd_out = paste0(wd_in, "/", "subset_age", "/"),
    subset_var = "age",
    sv_selection = c(7, 8),
    rm_suspdup = TRUE
  ),
  transform =
    function(chr_line)
      stringr::str_replace(
        chr_line,
        "(?>=\\d{1,3}\\%\\s| ).*(?= \\|)",
        " percent, seconds"
      ))

  drug_sub <-
    dt_parquet(paste0(wd_in, "/", "subset_age_suspdup", "/"), "drug")

  demo_sub <-
    dt_parquet(paste0(wd_in, "/", "subset_age_suspdup", "/"), "demo")

  demo_nosuspdup <-
    dt_parquet(paste0(wd_in, "/", "subset_age", "/"), "demo")

  expect_equal(
    sort(unique(demo_sub$UMCReportId)),
    sort(unique(drug_sub$UMCReportId))
  )

  age_test <-
    demo_sub %>%
    dplyr::mutate(
      good_age = AgeGroup %in% c(7, 8)
    )

  expect_equal(
    all(age_test$good_age == 1),
    TRUE
  )

  expect_equal(
    nrow(demo_sub),
    nrow(demo_nosuspdup) + 1
  )

  unlink(wd_in, recursive = TRUE)

})


test_that("alternative syntaxes work", {

  wd_in <- tempdir()

  wd_in <- paste0(wd_in, "\\", "sub2") # avoid windows #1224

  dir.create(path = wd_in)

  mini_data <- rlang::list2(
    demo =
      data.table(
        UMCReportId = c(1, 2, 3, 4),
        AgeGroup = c(1, 7, 7, 8)
      ),
    drug =
      data.table(
        UMCReportId = c(1, 2, 3, 4),
        Drug_Id = c("d1", "d2", "d3", "d4"),
        DrecNo = c(133138448, 133138448, 111841511, 111841511),
        MedicinalProd_Id = c(25027716, 97354576, 104264760, 37484408)
      ),
    adr  =
      data.table(
        UMCReportId = c(1, 2, 3, 4),
        Adr_Id = c("a1", "a2", "a3", "a4"),
        MedDRA_Id = c(110049083, 31672047, 146319904, 72535511)
      ),
    link =
      data.table(
        Drug_Id = c("d1", "d2", "d3", "d4"),
        Adr_Id = c("a1", "a2", "a3", "a4")
      ),
    srce =
      data.table(
        UMCReportId = c(1, 2, 3, 4)
      ),
    ind  =
      data.table(
        Drug_Id = c("d1", "d2", "d3", "d4")
      ),
    out  =
      data.table(
        UMCReportId = c(1, 2, 3, 4)
      ),
    followup =
      data.table(
        UMCReportId = c(1, 2, 3, 4)
      ),
    suspdup =
      data.table(
        UMCReportId = c(3),
        SuspectedduplicateReportId = c(4)
      )
  )

  purrr::iwalk(
    mini_data,
    function(dataset, name)
      arrow::write_parquet(
        dataset |>
          arrow::as_arrow_table(),
        sink = paste0(wd_in, "/", name, ".parquet")
      )

  )

  # no end slashes at wd_in and wd_out
  expect_snapshot(
    tb_subset(
    wd_in = wd_in,
    wd_out = paste0(wd_in, "/", "subset_age"),
    subset_var = "age",
    sv_selection = c(7, 8)
  ),
  transform =
    function(chr_line)
      stringr::str_replace(
        chr_line,
        "(?>=\\d{1,3}\\%\\s| ).*(?= \\|)",
        " percent, seconds"
      ))

  drug_sub <-
    dt_parquet(paste0(wd_in, "/", "subset_age", "/"), "drug")

  demo_sub <-
    dt_parquet(paste0(wd_in, "/", "subset_age", "/"), "demo")

  expect_equal(
    sort(unique(demo_sub$UMCReportId)),
    sort(unique(drug_sub$UMCReportId))
  )

  age_test <-
    demo_sub %>%
    dplyr::mutate(
      good_age = AgeGroup %in% c(7, 8)
    )

  expect_equal(
    all(age_test$good_age == 1),
    TRUE
  )


 unlink(wd_in, recursive = TRUE)

})
