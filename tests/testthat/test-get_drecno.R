test_that("get drecno of a single drug, no combination allowed", {

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

  expect_equal(
    get_drecno(d_sel = d_sel_names,
               mp = mp_,
               allow_combination = FALSE,
               method = "drug_name",
               show_all = FALSE,
               inspect = FALSE
               ),
    d_drecno_nocomb
  )

  get_drecno(d_sel = d_sel_names,
             mp = mp_,
             allow_combination = FALSE,
             method = "drug_name",
             show_all = FALSE,
             inspect = TRUE
  )

  expect_equal(
    get_drecno(d_sel = d_sel_names,
               mp = mp_,
               allow_combination = TRUE,
               method = "drug_name",
               show_all = FALSE,
               inspect = FALSE
               ),
    d_drecno_comb
  )


})

test_that("d_sel has inappropriate structure", {

  # list in list

  drug1 <- rlang::list2(atra = rlang::list2(
    nivo_ipi = c("nivolumab", "ipilimumab")
    )
  )

  expect_error(
    get_drecno(drug1, mp_),
    "Function is meant to be used for a single drug at a time. Check `d_sel` structure.")

})

test_that("show_all works", {

  # list in list

  drug1 <- rlang::list2(
    nivo = "nivolumab")

r1 <-
  get_drecno(drug1, mp_,
             show_all = TRUE)

  expect_equal(
    class(r1),
    "list")

  expect_equal(
    class(r1$nivo),
    c("data.table", "data.frame")
  )

  expect_equal(
    r1$nivo$drug_name_t,
    c("nivolumab", "ipilimumab;nivolumab")
  )

})

test_that("non WHO names raise appropriate warnings", {
  # Intended for the special case a DrecNo has multiple nonproprietary names, but can be extended to commercial names

  drug1 <- rlang::list2(atra = "all-trans retinoic acid")

  drug2 <- rlang::list2(doli = "doliprane")

  drug3 <- rlang::list2(medi = "medicament")

  drug4 <- rlang::list2(doli = "paracetam") # spelling error

  drug5 <- rlang::list2(att = "antithrombin") # correct name would be antithrombine iii

  expect_warning(get_drecno(drug1, mp_),
                 "NOT a WHO name")

  expect_warning(get_drecno(drug2, mp_),
                 "NOT a WHO name")

  expect_warning(get_drecno(drug3, mp_),
                 "there is no match")

  expect_warning(get_drecno(drug4, mp_),
                 "there is no match")

  expect_warning(get_drecno(drug5, mp_),
                 "there is no match")
})

test_that("works for drugs, which is the default setting", {

  drug1 <- rlang::list2(atra = "tretinoin")

  atra_drecno <- 133241834

  drug2 <- rlang::list2(pc = "protein c (coagulation inhibitor)") # name with parenthesis

  pc_drecno <- 108022014

  res_drug1 <- get_drecno(drug1,
                           mp_,
                           allow_combination = FALSE
  )

  expect_equal(length(res_drug1[["atra"]]),
               1)

  expect_equal(res_drug1[["atra"]], atra_drecno)

  res_drug2 <- get_drecno(drug2,
                           mp_,
                           allow_combination = TRUE
  )

  expect_equal(length(res_drug2[["pc"]]),
               3)

  res_drug2_bis <- get_drecno(drug2,
                               mp_,
                               allow_combination = FALSE
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
                         show_all = FALSE,
                         inspect = FALSE,
                         allow_combination = FALSE)

  expect_equal(length(res_mpi[["para"]]),
               1)

  expect_equal(res_mpi[["para"]], 42225260)

  expect_warning(get_drecno(mpi,
                             mp_,
                             method = "mpi_list",
                             show_all = FALSE,
                             allow_combination = TRUE),
                 "allow_combination set to TRUE but mpi requested")

})

test_that("inspection works", {
  # with method = drug_name
  d_one <-
    list(
      thrombophilia = c("tramadol")
    )


  r_inspect <-
    get_drecno(d_one,
             mp = mp_,
             method = "drug_name",
             inspect = TRUE,
             allow_combination = TRUE)


  r_expected_dim <- c(17, 9)

  expect_equal(
    dim(r_inspect[[1]]),
    r_expected_dim
  )

  expect_equal(
    all(stringr::str_detect(
      r_inspect[[1]]$drug_name_t,
      "tramadol"
    )),
    TRUE
  )

  expect_equal(
    names(r_inspect),
    names(d_one)
  )

  d_min <-
    list(
      thrombophilia = c("tramadol",
                        "nivolumab")
    )

  r_insp2 <-
    get_drecno(
    d_sel = d_min,
             mp = mp_,
             method = "drug_name",
             inspect = TRUE,
             allow_combination = TRUE)

  r_expected2_dim <- c(19, 9)

  expect_equal(
    dim(r_insp2[[1]]),
    r_expected2_dim
  )

  expect_equal(
    all(stringr::str_detect(
      r_insp2[[1]]$drug_name_t,
      "(tramadol|nivolumab)"
    )),
    TRUE
  )

  # with method = mpi_list

  mpi <- rlang::list2(
    para = mp_[DrecNo == 42225260, MedicinalProd_Id]
  )

  r_insp3 <- get_drecno(mpi,
                        mp_,
                        method = "mpi_list",
                        show_all = FALSE,
                        inspect = TRUE,
                        allow_combination = FALSE)

  r_expected3_dim <- c(1, 9)

  expect_equal(
    dim(r_insp3[[1]]),
    r_expected3_dim
  )

  expect_equal(
    all(stringr::str_detect(
      r_insp3[[1]]$drug_name_t,
      "paracetamol"
    )),
    TRUE
  )

})

test_that("names of d_sel were tolower-ed and trimed warning", {
  d_sel_names <- rlang::list2(
    Nivolumab = "nivolumab",
    Ipilimumab = "ipilimumab",
    Nivo_ipi = c("nivolumab", "ipilimumab")
  )

    expect_warning(
    get_drecno(
      d_sel = d_sel_names,
      mp = mp_,
      allow_combination = TRUE,
      method = "drug_name",
      inspect = TRUE
    ),
    "names of d_sel were tolower-ed and trimed"
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
               show_all = FALSE,
               inspect = FALSE
    ),
    d_drecno_nocomb
  )
})
