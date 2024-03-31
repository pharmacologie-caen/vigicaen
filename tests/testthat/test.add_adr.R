test_that("works with regular names for demo and adr", {

  demo <- demo_
  adr <- adr_

  n_adr <- length(ex_$a_llt)

  a_names <- paste0("adr_", names(ex_$a_llt))

  demo <-
   demo %>%
     add_adr(
       a_code = ex_$a_llt,
       a_names = a_names,
       adr_data = adr
     )

  expect_equal(ncol(demo),
               ncol(demo_) + n_adr)

  purrr::walk(
    a_names,
    function(a_n){
      expect_gt(sum(demo[[a_n]]),
                expected = 1)
    }
  )

})

test_that("works with irregular names for demo and adr", {

  dema <- demo_
  adra <- adr_

  n_adr <- length(ex_$a_llt)

  a_names <- paste0("adr_", names(ex_$a_llt))

  dema <-
    dema %>%
    add_adr(
      a_code = ex_$a_llt,
      a_names = a_names,
      adr_data = adra
    )

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

test_that("a dataset with no Drug_Id and Adr_Id columns (like link) breaks the function if data_type is set to demo", {
  adr_list_test <-
    rlang::list2(
      adr1 = "adr1",
      adr2 = "adr2",
      adr3 = "adr3",
      adr4 = "adr4"
    )

  adr_test <-
    data.table(
      Adr_Id = c("a1_adr1", "a2_adr4", "a3_adr2", "a4_adr4", "a5_adr2"),
      MedDRA_Id = c("adr1", "adr4", "adr2", "adr4", "adr2"),
      UMCReportId = c(1, 1, 2, 2, 3)
    )

  link_test <-
    data.table(
      Drug_Id =  c("d1_ici1", "d2_ici2", "d3_ici3", "d4_ici1", "d5_ici1"),
      Adr_Id = c("a1_adr1", "a2_adr4", "a3_adr2", "a4_adr4", "a5_adr2"),
      UMCReportId = c(1, 1, 2, 2, 3)
    )

  expect_error(
    link_test %>%
      add_adr(a_code = adr_list_test,
              adr_data = adr_test,
              data_type = "demo"),
    "The dataset has Drug_Id or Adr_Id columns"
  )
}
)


test_that("a dataset with no Drug_Id and Adr_Id columns (like demo, adr) breaks the function if data_type is set to link", {
  adr_list_test <-
    rlang::list2(
      adr1 = "adr1",
      adr2 = "adr2",
      adr3 = "adr3",
      adr4 = "adr4"
    )

  adr_test <-
    data.table(
      Adr_Id = c("a1_adr1", "a2_adr4", "a3_adr2", "a4_adr4", "a5_adr2"),
      MedDRA_Id = c("adr1", "adr4", "adr2", "adr4", "adr2"),
      UMCReportId = c(1, 1, 2, 2, 3)
    )

  demo_test <-
    data.table(
      UMCReportId = c(1, 2, 3, 4, 5)
    )

  adr_test <-
    data.table(
      UMCReportId = c(1, 1, 2, 2, 3),
      Adr_Id = c("a1_adr1", "a2_adr4", "a3_adr2", "a4_adr4", "a5_adr2"),
      MedDRA_Id = c(100000, 20000, 30000, 40000, 50000),
      Outcome = c(1, 2, 3, 2, 2)
    )

  purrr::walk(c(demo_test, adr_test), function(wrong_data)
    expect_error(
      adr_test %>%
        add_adr(a_code = adr_list_test,
                adr_data = adr_test,
                data_type = "link"),
      "The dataset does not have Drug_Id and Adr_Id columns"
      )
  )
}
)

test_that("a dataset with no Adr_Id, MedDRA_Id, and Outcome (like demo, link) breaks the function if data_type is set to adr", {
  adr_list_test <-
    rlang::list2(
      adr1 = "adr1",
      adr2 = "adr2",
      adr3 = "adr3",
      adr4 = "adr4"
    )

  demo_test <-
    data.table(
      UMCReportId = c(1, 2, 3, 4, 5)
    )

  link_test <-
    data.table(
      Drug_Id = 1,
      Adr_Id = 2
    )

  adr_test <-
    data.table(
      UMCReportId = c(1, 1, 2, 2, 3),
      Adr_Id = c("a1_adr1", "a2_adr4", "a3_adr2", "a4_adr4", "a5_adr2"),
      MedDRA_Id = c(100000, 20000, 30000, 40000, 50000),
      Outcome = c(1, 2, 3, 2, 2)
    )

  purrr::walk(c(demo_test, link_test), function(wrong_data)
    expect_error(
      wrong_data %>%
        add_adr(a_code = adr_list_test,
                adr_data = adr_test,
                data_type = "adr"),
      "The dataset does not have Adr_Id, MedDRA_Id, and/or Outcome columns"
    )
  )
}
)

test_that("works with link data, adr identification is Adr_Id wise, not UMCReportId wise", {
  adr_list_test <-
    rlang::list2(
      adr1 = "adr1",
      adr2 = "adr2",
      adr3 = "adr3",
      adr4 = "adr4"
    )

  adr_test <-
    data.table(
      Adr_Id = c("a1_adr1", "a2_adr4", "a3_adr2", "a4_adr4", "a5_adr2"),
      MedDRA_Id = c("adr1", "adr4", "adr2", "adr4", "adr2"),
      UMCReportId = c(1, 1, 2, 2, 3)
    )

  luda_test <-
    data.table(
      Drug_Id =  c("d1_ici1", "d2_ici2", "d3_ici3", "d4_ici1", "d5_ici1"),
      Adr_Id = c("a1_adr1", "a2_adr4", "a3_adr2", "a4_adr4", "a5_adr2"),
      UMCReportId = c(1, 1, 2, 2, 3)
    ) %>%
    add_adr(a_code = adr_list_test,
            adr_data = adr_test,
            data_type = "link")

  luda_correct <-
    data.table(
      Drug_Id =  c("d1_ici1", "d2_ici2", "d3_ici3", "d4_ici1", "d5_ici1"),
      Adr_Id = c("a1_adr1", "a2_adr4", "a3_adr2", "a4_adr4", "a5_adr2"),
      UMCReportId = c(1, 1, 2, 2, 3),
      adr1 = c(1, 0, 0, 0, 0),
      adr2 = c(0, 0, 1, 0, 1),
      adr3 = c(0, 0, 0, 0, 0),
      adr4 = c(0, 1, 0, 1, 0)
    )

  expect_equal(
    luda_test,
    luda_correct
  )
}
)

test_that("works with adr data as the .data argument", {
  adr_list_test <-
    rlang::list2(
      adr1 = "adr1",
      adr2 = "adr2",
      adr3 = "adr3",
      adr4 = "adr4"
    )

  adr_test <-
    data.table(
      UMCReportId = c(1, 1, 2, 2, 3),
      Adr_Id = c("a1_adr1", "a2_adr4", "a3_adr2", "a4_adr4", "a5_adr2"),
      MedDRA_Id = c("adr1", "adr4", "adr2", "adr4", "adr2"),
      Outcome = c(1, 2, 3, 2, 2)
    )

  adr_try <-
    adr_test |>
    add_adr(
      a_code = adr_list_test,
      adr_data = adr_test,
      data_type = "adr"
    )

  adr_correct <-
    data.table(
      UMCReportId = c(1, 1, 2, 2, 3),
      Adr_Id = c("a1_adr1", "a2_adr4", "a3_adr2", "a4_adr4", "a5_adr2"),
      MedDRA_Id = c("adr1", "adr4", "adr2", "adr4", "adr2"),
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

})


test_that("handle ambiguous names in .data", {
  adr_list_test <-
    rlang::list2(
      adr1 = "adr1",
      adr2 = "adr2",
      adr3 = "adr3",
      adr4 = "adr4"
    )

  adr_test <-
    data.table(
      Adr_Id = c("a1_adr1", "a2_adr4", "a3_adr2", "a4_adr4", "a5_adr2"),
      MedDRA_Id = c("adr1", "adr4", "adr2", "adr4", "adr2"),
      UMCReportId = c(1, 1, 2, 2, 3)
    )

  demo_test <-
    data.table(
      UMCReportId = c(1, 2, 3, 4, 5),

      # an ambiguous column name
      adr_test = c(1, 1, 0, 0, 0)
    )

  res <-
    demo_test %>%
    add_adr(a_code = adr_list_test,
            adr_data = adr_test,
            data_type = "demo")

  expect_equal(
    res$adr1,
    c(1, 0, 0, 0, 0)
  )

  expect_equal(
    res$adr4,
    c(1, 1, 0, 0, 0)
  )
})
