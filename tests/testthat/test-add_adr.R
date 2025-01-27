test_that("works with regular names for demo and adr", {

  demo <- demo_
  adr <- adr_

  n_adr <- length(ex_$a_llt)

  a_names <- paste0("adr_", names(ex_$a_llt))

  expect_snapshot({
    demo <-
      demo %>%
      add_adr(a_code = ex_$a_llt,
              a_names = a_names,
              adr_data = adr)
  })

  expect_snapshot({
    demo_a <-
      demo |>
      arrow::as_arrow_table() |>
      add_adr(
        a_code = ex_$a_llt,
        a_names = a_names,
        adr_data = arrow::as_arrow_table(adr)
      ) |>
      dplyr::collect()
  })

  expect_equal(ncol(demo),
               ncol(demo_) + n_adr)

  purrr::walk(
    a_names,
    function(a_n){
      expect_gt(sum(demo[[a_n]]),
                expected = 1)
    }
  )

  expect_equal(demo, demo_a)

})

test_that("works with irregular names for demo and adr", {

  dema <- demo_
  adra <- adr_

  n_adr <- length(ex_$a_llt)

  a_names <- paste0("adr_", names(ex_$a_llt))

  expect_snapshot({
    dema <-
      dema %>%
      add_adr(a_code = ex_$a_llt,
              a_names = a_names,
              adr_data = adra)
  })

  expect_snapshot({
    dema_a <-
      dema |>
      arrow::as_arrow_table() |>
      add_adr(
        a_code = ex_$a_llt,
        a_names = a_names,
        adr_data = arrow::as_arrow_table(adra)
      ) |>
      dplyr::collect()
  })

  expect_equal(ncol(dema),
               ncol(demo_) + n_adr)

  purrr::walk(
    a_names,
    function(a_n){
      expect_gt(sum(dema[[a_n]]),
                expected = 1)
    }
  )

})

test_that("works with link data, adr identification is Adr_Id wise, not UMCReportId wise", {
  adr_list_test <-
    rlang::list2(
      adr1 = 12,
      adr2 = 13,
      adr3 = 14,
      adr4 = 15
    )

  adr_test <-
    data.table(
      Adr_Id = c("a1_adr1", "a2_adr4", "a3_adr2", "a4_adr4", "a5_adr2"),
      MedDRA_Id = c(12, 15, 13,  15, 13),
      UMCReportId = c(1, 1, 2, 2, 3),
      Outcome = NA
    )

  expect_snapshot({
    link_test <-
      data.table(
        Drug_Id =  c("d1_ici1", "d2_ici2", "d3_ici3", "d4_ici1", "d5_ici1"),
        Adr_Id = c("a1_adr1", "a2_adr4", "a3_adr2", "a4_adr4", "a5_adr2"),
        UMCReportId = c(1, 1, 2, 2, 3),
        Dechallenge1 = NA,
        TimeToOnsetMin = NA
      ) %>%
      add_adr(a_code = adr_list_test, adr_data = adr_test)
  })

  expect_snapshot({
    link_test_a <-
      data.table(
        Drug_Id =  c("d1_ici1", "d2_ici2", "d3_ici3", "d4_ici1", "d5_ici1"),
        Adr_Id = c("a1_adr1", "a2_adr4", "a3_adr2", "a4_adr4", "a5_adr2"),
        UMCReportId = c(1, 1, 2, 2, 3),
        Dechallenge1 = NA,
        TimeToOnsetMin = NA
      ) |>
      arrow::as_arrow_table() |>
      add_adr(a_code = adr_list_test,
              adr_data = arrow::as_arrow_table(adr_test)) |>
      dplyr::collect()
  })

  link_correct <-
    data.table(
      Drug_Id =  c("d1_ici1", "d2_ici2", "d3_ici3", "d4_ici1", "d5_ici1"),
      Adr_Id = c("a1_adr1", "a2_adr4", "a3_adr2", "a4_adr4", "a5_adr2"),
      UMCReportId = c(1, 1, 2, 2, 3),
      Dechallenge1 = NA,
      TimeToOnsetMin = NA,
      adr1 = c(1, 0, 0, 0, 0),
      adr2 = c(0, 0, 1, 0, 1),
      adr3 = c(0, 0, 0, 0, 0),
      adr4 = c(0, 1, 0, 1, 0)
    )

  expect_equal(
    link_test,
    link_correct
  )

  expect_equal(
    link_test,
    link_test_a
  )
}
)

test_that("works with adr data as the .data argument", {
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
      MedDRA_Id = c(12, 15, 13,  15, 13),
      Outcome = c(1, 2, 3, 2, 2)
    )


  expect_snapshot({
    adr_try <-
      adr_test |>
      add_adr(a_code = adr_list_test,
              adr_data = adr_test)
  })

  expect_snapshot({
    adr_try_a <-
      adr_test |>
      arrow::as_arrow_table() |>
      add_adr(
        a_code = adr_list_test,
        adr_data = arrow::as_arrow_table(adr_test)
      ) |>
      dplyr::collect()
  })


  adr_correct <-
    data.table(
      UMCReportId = c(1, 1, 2, 2, 3),
      Adr_Id = c("a1_adr1", "a2_adr4", "a3_adr2", "a4_adr4", "a5_adr2"),
      MedDRA_Id = c(12, 15, 13,  15, 13),
      Outcome = c(1, 2, 3, 2, 2),
      adr1 = c(1, 0, 0, 0, 0),
      adr2 = c(0, 0 ,1, 0, 1),
      adr3 = c(0, 0, 0, 0, 0),
      adr4 = c(0, 1, 0, 1, 0)

    )

  expect_equal(
    adr_try,
    adr_correct
  )

  expect_equal(
    adr_try,
    adr_try_a
  )

})

test_that("works with drug data as the .data argument", {
  adr_list_test <-
    rlang::list2(
      adr1 = 12,
      adr2 = 13,
      adr3 = 14,
      adr4 = 15
    )

  drug_test <-
    data.table(
      Drug_Id = c("d1_ici1", "d2_ici2", "d3_ici3", "d4_ici1", "d5_ici1"),
      Basis   = c(1, 1, 1, 1, 1),
      DrecNo  = c("ici1", "ici2", "ici3", "ici1", "ici1"),
      MedicinalProd_Id = NA,
      UMCReportId = c(1, 1, 2, 2, 3)
    )

  adr_test <-
    data.table(
      UMCReportId = c(1, 1, 2, 2, 3),
      Adr_Id = c("a1_adr1", "a2_adr4", "a3_adr2", "a4_adr4", "a5_adr2"),
      MedDRA_Id = c(12, 15, 13,  15, 13),
      Outcome = c(1, 2, 3, 2, 2)
    )

  expect_snapshot({
    drug_try <-
      drug_test |>
      add_adr(a_code = adr_list_test, adr_data = adr_test)
  })

  expect_snapshot({
    drug_try_a <-
      drug_test |>
      arrow::as_arrow_table() |>
      add_adr(a_code = adr_list_test,
              adr_data = arrow::as_arrow_table(adr_test)) |>
      dplyr::collect()
  })


  drug_correct <-
    data.table(
      Drug_Id = c("d1_ici1", "d2_ici2", "d3_ici3", "d4_ici1", "d5_ici1"),
      Basis   = c(1, 1, 1, 1, 1),
      DrecNo  = c("ici1", "ici2", "ici3", "ici1", "ici1"),
      MedicinalProd_Id = NA,
      UMCReportId = c(1, 1, 2, 2, 3),
      adr1 = c(1, 1, 0, 0, 0),
      adr2 = c(0, 0, 1, 1, 1),
      adr3 = c(0, 0, 0, 0, 0),
      adr4 = c(1, 1, 1, 1, 0)

    )

  expect_equal(
    drug_try,
    drug_correct
  )

  expect_equal(
    drug_correct,
    drug_try_a
  )

})

test_that("handle ambiguous names in .data", {
  adr_list_test <-
    rlang::list2(
      adr1 = 12,
      adr2 = 13,
      adr3 = 14,
      adr4 = 15
    )

  adr_test <-
    data.table(
      Adr_Id = c("a1_adr1", "a2_adr4", "a3_adr2", "a4_adr4", "a5_adr2"),
      MedDRA_Id = c(12, 15, 13,  15, 13),
      UMCReportId = c(1, 1, 2, 2, 3),
      Outcome = NA
    )

  demo_test <-
    data.table(
      UMCReportId = c(1, 2, 3, 4, 5),
      Region = NA,
      DateDatabase = NA,
      Type = NA,

      # an ambiguous column name
      adr_test = c(1, 1, 0, 0, 0)
    )

  expect_snapshot({
    res <-
    demo_test %>%
    add_adr(a_code = adr_list_test,
            adr_data = adr_test)
  })

  expect_snapshot({
  res_a <-
    demo_test|>
    arrow::as_arrow_table() |>
    add_adr(a_code = adr_list_test,
            adr_data = arrow::as_arrow_table(adr_test)) |>
    dplyr::collect()
  })

  expect_equal(
    res$adr1,
    c(1, 0, 0, 0, 0)
  )

  expect_equal(
    res$adr4,
    c(1, 1, 0, 0, 0)
  )

  expect_equal(res, res_a)
})

test_that("Providing data_type arg raises deprecation warn", {

  expect_snapshot({
    r1 <-
      demo_ |>
      add_adr(
        a_code = ex_$a_llt,
        adr_data = adr_,
        data_type = "demo"
      )
  })
})

test_that("adr_data should be a valid adr type data", {
  expect_snapshot(error = TRUE,
                  {
                    demo_ |>
                      add_adr(
                        a_code = ex_$a_llt,
                        adr_data = drug_
                      )
                  })
})
