test_that("get drecno of single drug, or combination allowed", {

  nivo_drecno <- 111841511

  ipi_drecno <- 133138448

  ipi_nivo_drecno <- 98742214

  d_sel_names <- rlang::list2(
   nivolumab = "nivolumab",
   ipilimumab = "ipilimumab",
   nivo_ipi = c("nivolumab", "ipilimumab")
   )

  d_drecno_nocomb <- rlang::list2(
    nivo_drecno,
    ipi_drecno,
    c(nivo_drecno, ipi_drecno)
  ) %>%
    rlang::set_names(names(d_sel_names))

  d_drecno_comb <- rlang::list2(
    c(nivo_drecno, ipi_nivo_drecno),
    c(ipi_drecno, ipi_nivo_drecno),
    c(nivo_drecno, ipi_nivo_drecno, ipi_drecno)
  ) %>%
    rlang::set_names(names(d_sel_names))


  expect_snapshot({
    d_drecno_res <-
      get_drecno(
        d_sel = d_sel_names,
        mp = mp_,
        allow_combination = FALSE,
        method = "drug_name",
        show_all = FALSE
      )
  })

  expect_equal(
    d_drecno_res,
    d_drecno_nocomb
  )

  expect_snapshot({
    d_drecno_res_comb <-
      get_drecno(
        d_sel = d_sel_names,
        mp = mp_,
        allow_combination = TRUE,
        method = "drug_name",
        show_all = FALSE
      )
  })


  expect_equal(
    d_drecno_res_comb,
    d_drecno_comb
  )

  # long list of matchings are truncated

  expect_snapshot(
    r1 <-
      get_drecno(list(a = "paracetamol"),
             allow_combination = TRUE,
             mp = mp_,
             verbose = TRUE)
  )


})

test_that("d_sel has inappropriate structure", {

  # list in list

  drug1 <- rlang::list2(atra = rlang::list2(
    nivo_ipi = c("nivolumab", "ipilimumab")
    )
  )

  # internally running check_id_list

  expect_snapshot(error = TRUE, {
    get_drecno(drug1, mp_,
               verbose = FALSE)
    })

})

test_that("show_all is deprecated", {

  # list in list

  drug1 <- rlang::list2(
    nivo = "nivolumab")

r1 <-
  expect_snapshot(
    r1 <-
      expect_warning(get_drecno(drug1, mp_,
             show_all = TRUE,
             verbose = FALSE),
             "deprecated"
      ))

})

test_that("non WHO names raise appropriate warnings", {
  # Intended for the special case a DrecNo has multiple nonproprietary names, but can be extended to commercial names

  drug1 <- rlang::list2(atra = "all-trans retinoic acid")

  drug2 <- rlang::list2(doli = "doliprane")

  drug3 <- rlang::list2(medi = "medicament")

  drug4 <- rlang::list2(doli = "paracetam") # spelling error

  drug5 <- rlang::list2(att = "antithrombin") # correct name would be antithrombine iii

  drug6 <- rlang::list2(
    medi = c("medicament", "medicament2"),
    medi2 = c("autremedic")
  )

  drug7 <- rlang::list2(
    notwho = c("all-trans retinoic acid", "doliprane", "paracetamol"),
    notwho2 = c("doliprane"),
    who = "nivolumab"
  )

  drug8 <- rlang::list2(
    Medi = c("medicament", "medicament2", "enalapril"),
    mix = c("all-trans retinoic acid", "enalaprilat", "renitec", "enalapril"),
    notwho2 = c("doliprane"),
    who = "nivolumab"
  )


  expect_snapshot(
    expect_warning(
      r1 <- get_drecno(drug1, mp_,
                       verbose = FALSE),
      "Switch to .* WHO names.")
  )

  expect_snapshot(
    expect_warning(
      r1 <- get_drecno(drug2, mp_,
                       verbose = FALSE),
      "Switch to .* WHO names.")
  )

  expect_snapshot(r1 <- get_drecno(drug3, mp_,
                            verbose = FALSE))

  expect_snapshot(r1 <- get_drecno(drug4, mp_,
                            verbose = FALSE))

  expect_snapshot(r1 <- get_drecno(drug5, mp_,
                            verbose = FALSE))

  # complex output structure

  expect_snapshot(r1 <- get_drecno(drug6, mp_,
                            verbose = FALSE))

  # with both matchings, non who names
  expect_snapshot(
    expect_warning(
      r1 <- get_drecno(drug7, mp_,
                       allow_combination = TRUE,
                       verbose = FALSE),
      "Switch to .* WHO names.")
  )

  expect_snapshot(
      r1 <- get_drecno(drug7, mp_,
                       allow_combination = FALSE,
                       verbose = FALSE)
  )

  # with both matchings, non matching, non who names
  # in combination or not

  expect_snapshot(
    r1 <- get_drecno(drug8, mp_, verbose = FALSE)
  )

  expect_snapshot(
    r1 <- get_drecno(drug8, mp_, allow_combination = FALSE, verbose = TRUE)
  )

  expect_snapshot(
    expect_warning(
      r1 <- get_drecno(drug8, mp_, allow_combination = TRUE, verbose = TRUE),
      "Switch to .* WHO names."
    )
  )
})

test_that("works for drugs, which is the default setting", {

  drug1 <- rlang::list2(atra = "tretinoin")

  atra_drecno <- 133241834

  drug2 <- rlang::list2(pc = "protein c (coagulation inhibitor)") # name with parenthesis

  pc_drecno <- 108022014

  res_drug1 <- get_drecno(drug1,
                           mp_,
                           allow_combination = FALSE,
                          verbose = FALSE
  )

  expect_equal(length(res_drug1[["atra"]]),
               1)

  expect_equal(res_drug1[["atra"]], atra_drecno)

  res_drug2 <- get_drecno(drug2,
                           mp_,
                           allow_combination = TRUE,
                          verbose = FALSE
  )

  expect_equal(length(res_drug2[["pc"]]),
               3)

  res_drug2_bis <- get_drecno(drug2,
                               mp_,
                               allow_combination = FALSE,
                              verbose = FALSE
  )

  expect_equal(length(res_drug2_bis[["pc"]]),
               1)

  expect_equal(res_drug2_bis[["pc"]], pc_drecno)

})

test_that("works for mpi_list as well", {

  mpi <- rlang::list2(
    para = mp_[DrecNo == 42225260, MedicinalProd_Id]
  )

  res_mpi <- get_drecno(mpi,
                         mp_,
                         method = "mpi_list",
                         # show_all = FALSE,
                         allow_combination = FALSE,
                        verbose = FALSE)

  expect_snapshot(
    r1 <- get_drecno(
      mpi,
      mp_,
      method = "mpi_list",
      allow_combination = FALSE,
      verbose = TRUE
    )
  )


  expect_equal(length(res_mpi[["para"]]),
               1)

  expect_equal(res_mpi[["para"]], 42225260)

  expect_message(r1 <- get_drecno(mpi,
                             mp_,
                             method = "mpi_list",
                             allow_combination = TRUE,
                            verbose = FALSE),
                 "allow_combination.* is ignored")

})

test_that("verbose works", {
  # with method = drug_name

  d_one <-
    list(
      set1 = c("enalapril")
    )

  expect_snapshot({

    r_verbose_1 <-
      get_drecno(d_one,
                 mp = mp_,
                 method = "drug_name",
                 allow_combination = FALSE,
                 verbose = TRUE)

  })


  d_one <-
    list(
      set1 = c("nivolumab")
    )


  expect_snapshot({

    r_verbose_2 <-
    get_drecno(d_one,
             mp = mp_,
             method = "drug_name",
             allow_combination = TRUE,
             verbose = TRUE)

  })

   expect_no_message(
     r_silent <-
       get_drecno(
        d_one,
        mp = mp_,
        method = "drug_name",
        allow_combination = TRUE,
        verbose = FALSE
      )
   )

  expect_equal(
    r_verbose_2,
    r_silent
  )


  d_min <-
    list(
      set1 = c("ipilimumab",
                        "nivolumab")
    )

  expect_snapshot({
    r_insp2 <-
      get_drecno(
        d_sel = d_min,
        mp = mp_,
        method = "drug_name",
        allow_combination = TRUE
      )
  })

  # with method = mpi_list

  mpi <- rlang::list2(
    set1 = mp_[DrecNo == 740486, MedicinalProd_Id]
  )

  expect_snapshot({
    r_insp3 <- get_drecno(mpi,
                        mp_,
                        method = "mpi_list",
                        verbose = TRUE,
                        allow_combination = FALSE)
  })


  expect_equal(
    r_insp3,
    r_verbose_1
  )
})

test_that("inspect and show_all are deprecated", {
  d_one <-
    list(
      thrombophilia = c("tramadol")
    )


  expect_snapshot(r_inspect <-
                    expect_warning(
                      get_drecno(
                        d_one,
                        mp = mp_,
                        method = "drug_name",
                        allow_combination = FALSE,
                        inspect = TRUE,
                        verbose = FALSE
                      ),
                      "deprecated"
                    ))

  expect_snapshot(r_inspect <-
                    expect_warning(
                      get_drecno(
                        d_one,
                        mp = mp_,
                        method = "drug_name",
                        allow_combination = FALSE,
                        show_all = TRUE,
                        verbose = FALSE
                      ),
                      "deprecated"
                    ))
})

test_that("names of d_sel were tolower-ed and trimed warning", {
  d_sel_names <- rlang::list2(
    Nivolumab = "nivolumab",
    Ipilimumab = "ipilimumab",
    Nivo_ipi = c("nivolumab", "ipilimumab")
  )

  d_sel_long <- rlang::list2(
    Drug1 = "nivolumab",
    Drug2 = "nivolumab",
    Drug3 = "nivolumab",
    Drug4 = "nivolumab",
    Drug5 = "nivolumab",
    Drug6 = "nivolumab"
  )

  d_sel_mix <- rlang::list2(
    drug1 = "nivolumab",
    Drug2 = "nivolumab"
  )


  expect_snapshot(
    r1 <- get_drecno(
      d_sel = d_sel_names,
      mp = mp_,
      allow_combination = TRUE,
      method = "drug_name",
      verbose = FALSE
    )
  )

  expect_snapshot(
    r1 <- get_drecno(
      d_sel = d_sel_long,
      mp = mp_,
      allow_combination = TRUE,
      method = "drug_name",
      verbose = FALSE
    )
  )

  expect_snapshot(
    r1 <- get_drecno(
      d_sel = d_sel_mix,
      mp = mp_,
      allow_combination = TRUE,
      method = "drug_name",
      verbose = FALSE
    )
  )
})

test_that("works with mp as Table (out of memory)", {

  nivo_drecno <- 111841511

  ipi_drecno <- 133138448

  ipi_nivo_drecno <- 98742214

  d_sel_names <- rlang::list2(
    nivolumab = "nivolumab",
    ipilimumab = "ipilimumab",
    nivo_ipi = c("nivolumab", "ipilimumab")
  )

  d_drecno_nocomb <- rlang::list2(
    nivo_drecno,
    ipi_drecno,
    c(nivo_drecno, ipi_drecno)
  ) %>%
    rlang::set_names(names(d_sel_names))

  d_drecno_comb <- rlang::list2(
    c(nivo_drecno, ipi_nivo_drecno),
    c(ipi_drecno, ipi_nivo_drecno),
    c(nivo_drecno, ipi_nivo_drecno, ipi_drecno)
  ) %>%
    rlang::set_names(names(d_sel_names))

  mp_ <-
    arrow::as_arrow_table(mp_)

  expect_equal(
    get_drecno(d_sel = d_sel_names,
               mp = mp_,
               allow_combination = FALSE,
               method = "drug_name",
               verbose = FALSE
    ),
    d_drecno_nocomb
  )
})

test_that("find_drug_and_check_exist returns correct output", {

  # --- with find_isolated

  # with a who name

  r1 <-
    find_drug_and_check_exist("paracetamol",
                              vigicaen:::find_isolated,
                              mp = mp_)

  r_true <-
    list(
      drecno_table =
        data.table::data.table(
          DrecNo = 42225260,
          drug_name_t = "paracetamol",
          who = TRUE),
      d_no_match = NULL,
      d_not_who = NULL
    )

  expect_equal(r1, r_true)


  # with an incorrect name

  r_not_match <-
    find_drug_and_check_exist(
    "prace",
    vigicaen:::find_isolated,
    mp = mp_
  )

  r_not_match_true <-
    list(
      drecno_table =
        data.table::data.table(
          DrecNo = numeric(0),
          drug_name_t = character(0),
          who = logical(0)),
      d_no_match = "prace",
      d_not_who = NULL
    )

  expect_equal(
    r_not_match,
    r_not_match_true
  )

  # with a non who name

  r_not_who <-
    find_drug_and_check_exist(
      "doliprane",
      vigicaen:::find_isolated,
      mp = mp_
    )

  r_not_who_true <-
    list(
      drecno_table =
        data.table::data.table(
          DrecNo = 42225260,
          drug_name_t = "doliprane",
          who = FALSE),
      d_no_match = NULL,
      d_not_who = c("paracetamol" = "doliprane")
    )

  expect_equal(
    r_not_who,
    r_not_who_true
  )


  # with a combination of the previous cases

  # with find_combination too

  r_comb <-
    find_drug_and_check_exist(
    "nivolumab",
    vigicaen:::find_combination,
    mp = mp_
  )

  r_comb_true <-
    list(
      drecno_table =
        data.table::data.table(
          DrecNo = c(111841511, 98742214),
          drug_name_t = c("nivolumab", "ipilimumab;nivolumab"),
          who = c(TRUE, TRUE)),
      d_no_match = NULL,
      d_not_who = NULL
    )

  expect_equal(
    r_comb,
    r_comb_true
  )

  r_comb_not_who <-
    find_drug_and_check_exist(
    "doliprane",
    vigicaen:::find_combination,
    mp = mp_
  )

  expect_equal(
    r_comb_not_who,
    r_not_who_true
  )

  r_comb_no_match <-
    find_drug_and_check_exist(
    "prace",
    vigicaen:::find_combination,
    mp = mp_
  )

  expect_equal(
    r_comb_no_match,
    r_not_match_true
  )

  # d_sel structure
  drug1 <- rlang::list2(atra = rlang::list2(
    nivo_ipi = c("nivolumab", "ipilimumab")
  )
  )

  # internal error
  expect_error(find_drug_and_check_exist(
    drug1[[1]][[1]],
    vigicaen:::find_isolated,
    mp_
  ))
})
