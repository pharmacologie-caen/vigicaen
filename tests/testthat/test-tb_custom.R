test_that("you can subset on drecno, age, meddra_id", {

  wd_in <- tempdir()

  suspdup <-
    data.table(
      UMCReportId = c(17658707, 17658774, 17658793, 17658806),
      SuspectedduplicateReportId = c(17658708, 17658775, 17658794, 17658807)
    )

   datasets <- rlang::list2(
     demo = demo_,
     drug = drug_,
     adr  = adr_,
     link = link_,
     srce = srce_,
     ind  = ind_,
     out  = out_,
     followup = followup_,
     suspectedduplicates = suspdup
  )

     purrr::iwalk(
       datasets,
       function(dataset, name)
         fst::write_fst(
           dataset,
           path = paste0(wd_in, "\\", name, ".fst")
         )

     )

     # drecno

  d_drecno <-
    pharmacocaen::get_drecno(
      d_sel = list(para = "paracetamol"),
      mp_short = pharmacocaen::mp_short_
    )

  tb_custom(
    wd_in = paste0(wd_in, "\\"),
    wd_out = paste0(wd_in, "\\subset\\"),
    subset_var = "drecno",
    sv_selection = d_drecno$para
    )


  drug_sub <-
    pharmacocaen::dt_fst(paste0(wd_in, "\\subset\\"), "drug")

  demo_sub <-
    pharmacocaen::dt_fst(paste0(wd_in, "\\subset\\"), "demo")

  expect_equal(
    sort(unique(demo_sub$UMCReportId)),
    sort(unique(drug_sub$UMCReportId))
    )

  para_test <-
    drug_sub %>%
    group_by(UMCReportId) %>%
    summarise(
      has_para = max(DrecNo %in% d_drecno$para)
    )

  expect_equal(
    all(para_test$has_para == 1),
    TRUE
  )

  # age

  tb_custom(
    wd_in = paste0(wd_in, "\\"),
    wd_out = paste0(wd_in, "\\subset_age\\"),
    subset_var = "age",
    sv_selection = c(7, 8)
  )

  drug_sub <-
    pharmacocaen::dt_fst(paste0(wd_in, "\\subset_age\\"), "drug")

  demo_sub <-
    pharmacocaen::dt_fst(paste0(wd_in, "\\subset_age\\"), "demo")

  expect_equal(
    sort(unique(demo_sub$UMCReportId)),
    sort(unique(drug_sub$UMCReportId))
  )

  age_test <-
    demo_sub %>%
    mutate(
      good_age = AgeGroup %in% c(7, 8)
    )

  expect_equal(
    all(age_test$good_age == 1),
    TRUE
  )

  # adr

  sv_selection <-
   ex_$a_llt$a_colitis

  wd_out <- paste0(wd_in, "\\", "subset_colitis", "\\")

  tb_custom(
    wd_in = paste0(wd_in, "\\"),
    wd_out = paste0(wd_in, "\\subset_colitis\\"),
    subset_var = "meddra_id",
    sv_selection = sv_selection
  )

  adr_sub <-
    pharmacocaen::dt_fst(paste0(wd_in, "\\subset_colitis\\"), "adr")

  demo_sub <-
    pharmacocaen::dt_fst(paste0(wd_in, "\\subset_colitis\\"), "demo")

  expect_equal(
    sort(unique(demo_sub$UMCReportId)),
    sort(unique(adr_sub$UMCReportId))
  )

  colitis_test <-
    adr_sub %>%
    group_by(UMCReportId) %>%
    summarise(
      has_colitis = max(MedDRA_Id %in% ex_$a_llt$a_colitis)
    )

  expect_equal(
    all(colitis_test$has_colitis == 1),
    TRUE
  )

})
