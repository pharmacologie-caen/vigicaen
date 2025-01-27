test_that("gives proper counts", {

  d_drecno_test <-
    rlang::list2(
      ici1 = 21,
      ici2 = 22,
      ici3 = 23
    )

  drug_test <-
    data.table(
      Drug_Id = c("d1_ici1", "d2_ici2", "d3_ici3", "d4_ici1", "d5_ici1"),
      Basis   = c(1, 1, 1, 1, 1),
      DrecNo  = c(21, 22, 21, 21, 21),
      UMCReportId = c(1, 1, 2, 2, 3),
      MedicinalProd_Id = NA
    )

  adr_list_test <-
    rlang::list2(
      adr1 = 12,
      adr2 = 13,
      adr3 = 14,
      adr4 = 15
    )

  adr_test <-
    data.table(
      UMCReportId = c(1, 1, 2, 2, 3),
      Adr_Id = c("a1_adr1", "a2_adr4", "a3_adr2", "a4_adr4", "a5_adr2"),
      MedDRA_Id = c(12, 15, 12, 15, 13),
      Outcome = c(1, 2, 3, 2, 2)
    )

  expect_snapshot({
    adr_test <-
      adr_test %>%
      add_drug(d_code = d_drecno_test, drug_data = drug_test) %>%
      add_adr(a_code = adr_list_test, adr_data = adr_test)
  })


  d_out_test <-
    desc_outcome(adr_test,
                 drug_s = "ici1",
                 adr_s = "adr1")

  d_out_correct <-
    dplyr::tibble(
      drug_s = c("ici1", "ici1"),
      adr_s = c("adr1", "adr1"),
      n_cas = c(1, 1),
      out_label = c("Recovered/resolved",
                    "Not recovered/not resolved")
    )

  expect_equal(
    d_out_test,
    d_out_correct
  )

})
