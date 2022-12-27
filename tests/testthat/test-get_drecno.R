test_that("get drecno of a single drug, no combination allowed", {

  nivo_drecno <- "078729"

  ipi_drecno <- "057456"

  ipi_nivo_drecno <- "139470"

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

  mp_short <- ex_$mp_short

  expect_equal(
    get_drecno(d_sel = d_sel_names,
               mp_short = mp_short,
               allow_combination = FALSE,
               method = "drug_name",
               show_all = FALSE,
               inspect = FALSE
               ),
    d_drecno_nocomb
  )

  get_drecno(d_sel = d_sel_names,
             mp_short = mp_short,
             allow_combination = FALSE,
             method = "drug_name",
             show_all = FALSE,
             inspect = TRUE
  )

  expect_equal(
    get_drecno(d_sel = d_sel_names,
               mp_short = mp_short,
               allow_combination = TRUE,
               method = "drug_name",
               show_all = FALSE,
               inspect = FALSE
               ),
    d_drecno_comb
  )


})

test_that("non WHO names raise appropriate warnings", {
  # Intended for the special case a DrecNo has multiple nonproprietary names, but can be extended to commercial names

  drug1 <- rlang::list2(atra = "all-trans retinoic acid")

  drug2 <- rlang::list2(doli = "doliprane")

  drug3 <- rlang::list2(medi = "medicament")

  drug4 <- rlang::list2(doli = "paracetamo") # spelling error

  drug5 <- rlang::list2(att = "antithrombin") # correct name would be antithrombine iii

  expect_warning(get_drecno(drug1, ex_$mp_short),
                 "NOT a WHO name")

  expect_warning(get_drecno(drug2, ex_$mp_short),
                 "NOT a WHO name")

  expect_warning(get_drecno(drug3, ex_$mp_short),
                 "there is no match")

  expect_warning(get_drecno(drug4, ex_$mp_short),
                 "there is no match")

  expect_warning(get_drecno(drug5, ex_$mp_short),
                 "there is no match")
})

test_that("works for drugs, which is the default setting", {

  drug1 <- rlang::list2(atra = "tretinoin")

  atra_drecno <- "002796"

  drug2 <- rlang::list2(pc = "protein c (coagulation inhibitor)") # name with parenthesis

  pc_drecno <- "012493"

  res_drug1 <- get_drecno(drug1,
                           ex_$mp_short,
                           allow_combination = FALSE
  )

  expect_equal(length(res_drug1[["atra"]]),
               1)

  expect_equal(res_drug1[["atra"]], atra_drecno)

  res_drug2 <- get_drecno(drug2,
                           ex_$mp_short,
                           allow_combination = TRUE
  )

  expect_equal(length(res_drug2[["pc"]]),
               3)

  res_drug2_bis <- get_drecno(drug2,
                               ex_$mp_short,
                               allow_combination = FALSE
  )

  expect_equal(length(res_drug2_bis[["pc"]]),
               1)

  expect_equal(res_drug2_bis[["pc"]], pc_drecno)

})


test_that("works for mpi_list as well", {

  mpi <- rlang::list2(
    para = ex_$mp_short[DrecNo == "000200", MedicinalProd_Id]
  )

  res_mpi <- get_drecno(mpi,
                         ex_$mp_short,
                         method = "mpi_list",
                         show_all = FALSE,
                         allow_combination = FALSE)

  expect_equal(length(res_mpi[["para"]]),
               1)

  expect_equal(res_mpi[["para"]], "000200")

  expect_warning(get_drecno(mpi,
                             ex_$mp_short,
                             method = "mpi_list",
                             show_all = FALSE,
                             allow_combination = TRUE),
                 "allow_combination set to TRUE but mpi requested")

})


