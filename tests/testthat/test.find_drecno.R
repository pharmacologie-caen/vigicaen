context("find_drecno")

test_that("non WHO names raise appropriate warnings", {
  # Intended for the special case a DrecNo has multiple nonproprietary names, but can be extended to commercial names

  drug1 <- "all-trans retinoic acid"

  drug2 <- "doliprane"

  drug3 <- "medicament"

  drug4 <- "paracetamo" # spelling error

  drug5 <- "antithrombin" # correct name would be antithrombine iii

  expect_warning(find_drecno(drug1, ex_$mp_short),
                 "NOT a WHO name")

  expect_warning(find_drecno(drug2, ex_$mp_short),
                 "NOT a WHO name")

  expect_warning(find_drecno(drug3, ex_$mp_short),
                 "there is no match")

  expect_warning(find_drecno(drug4, ex_$mp_short),
                 "there is no match")

  expect_warning(find_drecno(drug5, ex_$mp_short),
                 "there is no match")
})

test_that("works for drugs, which is the default setting", {

  drug1 <- "tretinoin"

  drug2 <- "protein c (coagulation inhibitor)" # name with parenthesis

  res_drug1 <- find_drecno(drug1,
                         ex_$mp_short,
                         allow_combination = FALSE
                         )

  expect_equal(nrow(res_drug1),
               1)

  expect_equal(res_drug1[, DrecNo], "002796")

  res_drug2 <- find_drecno(drug2,
                           ex_$mp_short,
                           allow_combination = TRUE
  )

  expect_equal(nrow(res_drug2),
               3)

  res_drug2_bis <- find_drecno(drug2,
                           ex_$mp_short,
                           allow_combination = FALSE
  )

  expect_equal(nrow(res_drug2_bis),
               1)

  expect_equal(res_drug2_bis[, DrecNo], "012493")

})


test_that("works for mpi_list as well", {

  mpi <- ex_$mp_short[DrecNo == "000200", MedicinalProd_Id]

  res_mpi <- find_drecno(mpi,
                         ex_$mp_short,
                         method = "mpi_list",
                         show_all = FALSE,
                         allow_combination = FALSE,
                         mpi_meth_name = "atc")

  expect_equal(nrow(res_mpi),
               1)

  expect_equal(res_mpi[, DrecNo], "000200")

  expect_warning(find_drecno(mpi,
                             ex_$mp_short,
                             method = "mpi_list",
                             show_all = FALSE,
                             allow_combination = TRUE,
                             mpi_meth_name = "atc"),
                 "allow_combination set to TRUE but mpi requested")

})

