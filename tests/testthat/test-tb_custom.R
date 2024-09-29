test_that("you can subset on drecno, age, meddra_id", {

  wd_in <- tempdir()

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
        DrecNo = c("dr1", "dr2", "dr3", "dr4"),
        MedicinalProd_Id = c("mp1", "mp2", "mp3", "mp4")
      ),
    adr  =
      data.table(
        UMCReportId = c(1, 2, 3, 4),
        Adr_Id = c("a1", "a2", "a3", "a4"),
        MedDRA_Id = c("m1", "m2", "m3", "m4")
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

  sv_selection_drecno <- # spurious paracetamol drecnos
    c("dr3", "dr4")

  expect_snapshot(
    tb_custom(
      wd_in = paste0(wd_in, "/"),
      wd_out = paste0(wd_in, "/", "subset_drecno", "/"),
      subset_var = "drecno",
      sv_selection = sv_selection_drecno
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
      has_para = max(DrecNo %in% sv_selection_drecno)
    )

  expect_equal(
    all(para_test$has_para == 1),
    TRUE
  )

  # ---- age

  expect_snapshot(tb_custom(
    wd_in = paste0(wd_in, "/"),
    wd_out = paste0(wd_in, "/", "subset_age", "/"),
    subset_var = "age",
    sv_selection = c(7, 8)
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

  sv_selection_mid <- # spurious colitis codes
    c("m1", "m2")

  wd_out <- paste0(wd_in, "/", "subset_meddraid", "/")

  expect_snapshot(tb_custom(
    wd_in = paste0(wd_in, "/"),
    wd_out = paste0(wd_in, "/", "subset_meddraid", "/"),
    subset_var = "meddra_id",
    sv_selection = sv_selection_mid
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
      has_colitis = max(MedDRA_Id %in% sv_selection_mid)
    )

  expect_equal(
    all(colitis_test$has_colitis == 1),
    TRUE
  )

})

test_that("wd_in exists", {
  expect_error(
    tb_custom(
      wd_in = "that_dir_doesnt_exists"
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
        DrecNo = c("dr1", "dr2", "dr3", "dr4"),
        MedicinalProd_Id = c("mp1", "mp2", "mp3", "mp4")
      ),
    adr  =
      data.table(
        UMCReportId = c(1, 2, 3, 4),
        Adr_Id = c("a1", "a2", "a3", "a4"),
        MedDRA_Id = c("m1", "m2", "m3", "m4")
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

  expect_snapshot(tb_custom(
    wd_in = paste0(wd_in, "/"),
    wd_out = paste0(wd_in, "/", "subset_age_suspdup", "/"),
    subset_var = "age",
    sv_selection = c(7, 8),
    rm_suspdup = FALSE
  ))

  expect_snapshot(tb_custom(
    wd_in = paste0(wd_in, "/"),
    wd_out = paste0(wd_in, "/", "subset_age", "/"),
    subset_var = "age",
    sv_selection = c(7, 8),
    rm_suspdup = TRUE
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

})
