test_that("works with drecnos, regular names for demo and drug", {

  demo <- demo_
  drug <- drug_

  d_names <- rlang::list2(
    nivolumab = "nivolumab",
    pembrolizumab = "pembrolizumab"
    )

  d_count <- rlang::list2(
    nivolumab     = 225,
    pembrolizumab = 298
  )

  n_drug <- length(d_names)

  d_drecno <-
    d_names |>
    get_drecno(
      mp = mp_,
      verbose = FALSE
    )

  expect_snapshot({
    demo_n <-
      demo |>
      add_drug(
        d_code = d_drecno,
        method = "DrecNo",
        repbasis = "sci",
        drug_data = drug
      )
  })

  expect_snapshot({
    demo_a <-
      demo |>
      arrow::as_arrow_table() |>
      add_drug(
        d_code = d_drecno,
        method = "DrecNo",
        repbasis = "sci",
        drug_data = arrow::as_arrow_table(drug)
      ) |>
      dplyr::collect()
  })

  expect_equal(ncol(demo_n),
               ncol(demo_) + n_drug)

  expect_equal(ncol(demo_a),
               ncol(demo_) + n_drug)

  purrr::iwalk(
    d_count,
    function(d_n, id_n){
      expect_equal(sum(demo_n[[id_n]]),
                expected = d_n)
    }
  )

  purrr::iwalk(
    d_count,
    function(d_n, id_n){
      expect_equal(sum(demo_a[[id_n]]),
                   expected = d_n)
    }
  )

})

test_that("works with irregular names for demo and drug", {

  dema <- demo_
  druga <- drug_

  d_names <- rlang::list2(
    nivolumab = "nivolumab",
    pembrolizumab = "pembrolizumab"
  )

  d_count <- rlang::list2(
    nivolumab     = 225,
    pembrolizumab = 298
  )

  n_drug <- length(d_names)

  d_drecno <-
    d_names |>
    get_drecno(
      mp = mp_,
      verbose = FALSE
    )

  expect_snapshot({
    dema <-
      dema |>
      add_drug(
        d_code = d_drecno,
        method = "DrecNo",
        repbasis = "sci",
        drug_data = druga
      )
  })

  expect_equal(ncol(dema),
               ncol(demo_) + n_drug)

  purrr::iwalk(
    d_count,
    function(d_n, id_n){
      expect_equal(sum(dema[[id_n]]),
                   expected = d_n)
    }
  )

})

test_that("works with mpi_list", {

  mpi <- rlang::list2(
    para = mp_[DrecNo == "42225260", MedicinalProd_Id]
  )

  mpi_count <- rlang::list2(
    para = 64
  )

  n_drug <- length(mpi)

  expect_snapshot({
    demo <-
      demo_ |>
      add_drug(
        d_code = mpi,
        method = "MedicinalProd_Id",
        repbasis = "sci",
        drug_data = drug_
      )
  })

  expect_snapshot({
    demo_a <-
      demo_ |>
      arrow::as_arrow_table() |>
      add_drug(
        d_code = mpi,
        method = "MedicinalProd_Id",
        repbasis = "sci",
        drug_data = arrow::as_arrow_table(drug_)
      )
  })

  expect_equal(ncol(demo),
               ncol(demo_) + n_drug)

  purrr::iwalk(
    mpi_count,
    function(d_n, id_n){
      expect_equal(sum(demo[[id_n]]),
                   expected = d_n)
    }
  )

})

test_that("selecting only s, c, i works and provide less cases than sci altogether", {

  d_names <- rlang::list2(
    nivolumab = "nivolumab",
    pembrolizumab = "pembrolizumab"
  )

  n_drug <- length(d_names)

  d_drecno <-
    d_names |>
    get_drecno(
      mp = mp_,
      verbose = FALSE
    )

  bas <- c("s", "c", "i")

  expect_snapshot({
    res_each <-
      purrr::map(bas, function(repbasis_)
        demo_ |>
          add_drug(
            d_code = d_drecno,
            method = "DrecNo",
            repbasis = repbasis_,
            drug_data = drug_
          ))
  })

  expect_snapshot({
    res_each_a <-
      purrr::map(bas, function(repbasis_)
        arrow::as_arrow_table(demo_) |>
          add_drug(
            d_code = d_drecno,
            method = "DrecNo",
            repbasis = repbasis_,
            drug_data = arrow::as_arrow_table(drug_)
          ) |>
          dplyr::collect())
  })

  expect_snapshot({
    res_all <-
      demo_ |>
      add_drug(
        d_code = d_drecno,
        method = "DrecNo",
        repbasis = "sci",
        drug_data = drug_
      )
  })



  expect_snapshot({
    res_all_a <-
      arrow::as_arrow_table(demo_) |>
      add_drug(
        d_code = d_drecno,
        method = "DrecNo",
        repbasis = "sci",
        drug_data = arrow::as_arrow_table(drug_)
      ) |>
      dplyr::collect()
  })


  res_each_nivo_sum <-
    res_each |>
    purrr::map_dbl(function(x)
      sum(x[[names(d_names)[1]]]))

  res_each_a_nivo_sum <-
    res_each_a |>
    purrr::map_dbl(function(x)
      sum(x[[names(d_names)[1]]]))

  res_all_nivo_sum <-
    sum(res_all[[names(d_names)[1]]])

  res_all_a_nivo_sum <-
    sum(res_all[[names(d_names)[1]]])

  expect_equal(ncol(res_all),
               ncol(demo_) + n_drug)

  expect_equal(ncol(res_all_a),
               ncol(demo_) + n_drug)

  # found cases with s, c, i
  purrr::walk(
    res_each_nivo_sum,
    function(x){
      expect_gt(x,
                expected = 1)
    }
  )

  purrr::walk(
    res_each_a_nivo_sum,
    function(x){
      expect_gt(x,
                expected = 1)
    }
  )

  # found equally or more cases with s, c, i alone and sci altogether (see below)
  expect_gte(sum(res_each_nivo_sum),
             res_all_nivo_sum)

  expect_gte(sum(res_each_a_nivo_sum),
             res_all_a_nivo_sum)

  # found different number of reports with s, c, i
  expect_equal(res_each_nivo_sum[1] == res_each_nivo_sum[2], FALSE)
  expect_equal(res_each_nivo_sum[2] == res_each_nivo_sum[3], FALSE)
  expect_equal(res_each_nivo_sum[1] == res_each_nivo_sum[3], FALSE)

  expect_equal(res_each_a_nivo_sum[1] == res_each_a_nivo_sum[2], FALSE)
  expect_equal(res_each_a_nivo_sum[2] == res_each_a_nivo_sum[3], FALSE)
  expect_equal(res_each_a_nivo_sum[1] == res_each_a_nivo_sum[3], FALSE)

  # additional details : why is it ok to have MORE reports with sum(res_each_nivo_sum) ? Because some reports have the same drug reported with 2 or 3 repbasis (i.e. suspect and interacting, concomitant and interacting)

  # this is the demonstration
  # drecno_nivo <-
  #   ex_$d_drecno |>
  #   filter(drug == "nivolumab") |>
  #   pull(DrecNo)
  #
  # drug |>
  #   filter(DrecNo %in% drecno_nivo) |>
  #   add_count(UMCReportId) |>
  #   filter(n > 1) |>
  #   group_by(UMCReportId) |>
  #   filter(length(unique(Basis)) > 1)

})


test_that("works with link and drug data, drug identification is Drug_Id wise, not UMCReportId wise", {
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
      DrecNo  = c(21, 22, 23, 21, 21),
      MedicinalProd_Id = NA,
      UMCReportId = c(1, 1, 2, 2, 3)
    )

  expect_snapshot({
    link_test <-
      data.table(
        Drug_Id =  c("d1_ici1", "d2_ici2", "d3_ici3", "d4_ici1", "d5_ici1"),
        Adr_Id = c("a1_adr1", "a2_adr4", "a3_adr2", "a4_adr4", "a5_adr2"),
        UMCReportId = c(1, 1, 2, 2, 3),
        Dechallenge1 = NA,
        TimeToOnsetMin = NA
      ) |>
      add_drug(d_code = d_drecno_test,
               drug_data = drug_test)
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
      add_drug(
        d_code = d_drecno_test,
        drug_data = arrow::as_arrow_table(drug_test)
      ) |>
      dplyr::collect()
  })

  link_correct <-
    data.table(
      Drug_Id =  c("d1_ici1", "d2_ici2", "d3_ici3", "d4_ici1", "d5_ici1"),
      Adr_Id = c("a1_adr1", "a2_adr4", "a3_adr2", "a4_adr4", "a5_adr2"),
      UMCReportId = c(1, 1, 2, 2, 3),
      Dechallenge1 = NA,
      TimeToOnsetMin = NA,
      ici1 = c(1, 0, 0, 1, 1),
      ici2 = c(0, 1, 0, 0, 0),
      ici3 = c(0, 0, 1, 0, 0)
    )

  expect_equal(
    link_test,
    link_correct
  )

  expect_equal(
    link_test_a,
    link_correct
  )

  expect_snapshot({
    drug_output_test <-
      drug_test |>
      add_drug(d_code = d_drecno_test,
               drug_data = drug_test)
  })

  expect_snapshot({
    drug_output_test_a <-
      arrow::as_arrow_table(drug_test) |>
      add_drug(
        d_code = d_drecno_test,
        drug_data = arrow::as_arrow_table(drug_test)
      ) |>
      dplyr::collect()
  })

  drug_output_correct <-
    data.table(
      Drug_Id = c("d1_ici1", "d2_ici2", "d3_ici3", "d4_ici1", "d5_ici1"),
      Basis   = c(1, 1, 1, 1, 1),
      DrecNo  = c(21, 22, 23, 21, 21),
      MedicinalProd_Id = NA,
      UMCReportId = c(1, 1, 2, 2, 3),
      ici1 = c(1, 0, 0, 1, 1),
      ici2 = c(0, 1, 0, 0, 0),
      ici3 = c(0, 0, 1, 0, 0)
    )

  expect_equal(drug_output_test, drug_output_correct)

  expect_equal(drug_output_test_a, drug_output_correct)
}
)

test_that("works with adr data, drug identification is UMCReportId wise", {
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
      DrecNo  = c(21, 22, 23, 21, 21),
      UMCReportId = c(1, 1, 2, 2, 3),
      MedicinalProd_Id = NA
    )

  expect_snapshot({
    adr_test <-
      data.table(
        UMCReportId = c(1, 1, 2, 2, 3),
        Adr_Id = c("a1_adr1", "a2_adr4", "a3_adr2", "a4_adr4", "a5_adr2"),
        MedDRA_Id = c(100000, 20000, 30000, 40000, 50000),
        Outcome = c(1, 2, 3, 2, 2)
      ) |>
      add_drug(d_code = d_drecno_test,
               drug_data = drug_test)
  })

  expect_snapshot({
    adr_test_a <-
      data.table(
        UMCReportId = c(1, 1, 2, 2, 3),
        Adr_Id = c("a1_adr1", "a2_adr4", "a3_adr2", "a4_adr4", "a5_adr2"),
        MedDRA_Id = c(100000, 20000, 30000, 40000, 50000),
        Outcome = c(1, 2, 3, 2, 2)
      ) |>
      arrow::as_arrow_table() |>
      add_drug(
        d_code = d_drecno_test,
        drug_data = arrow::as_arrow_table(drug_test)
      ) |>
      dplyr::collect()
  })

  adr_correct <-
    data.table(
      UMCReportId = c(1, 1, 2, 2, 3),
      Adr_Id = c("a1_adr1", "a2_adr4", "a3_adr2", "a4_adr4", "a5_adr2"),
      MedDRA_Id = c(100000, 20000, 30000, 40000, 50000),
      Outcome = c(1, 2, 3, 2, 2),
      ici1 = c(1, 1, 1, 1, 1),
      ici2 = c(1, 1, 0, 0, 0),
      ici3 = c(0, 0, 1, 1, 0)
    )

  expect_equal(
    adr_test,
    adr_correct
  )

  expect_equal(
    adr_test_a,
    adr_correct
  )
}
)

test_that("handle ambiguous names in .data", {
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
      DrecNo  = c(21, 22, 23, 21, 21),
      UMCReportId = c(1, 1, 2, 2, 3),
      MedicinalProd_Id = NA
    )

  demo_test <-
    data.table(
      UMCReportId = c(1, 2, 3, 4, 5),
      Region = NA,
      DateDatabase = NA,
      Type = NA,

      # ambiguous column name
      drug_test = c(0, 0, 0, 0, 1)
    )

  expect_snapshot({
    res <-
    demo_test |>
    add_drug(
      d_code = d_drecno_test,
      method = "DrecNo",
      repbasis = "sci",
      drug_data = drug_test
    )
  })

  expect_snapshot({
    res_a <-
      arrow::as_arrow_table(demo_test)  |>
      add_drug(
        d_code = d_drecno_test,
        method = "DrecNo",
        repbasis = "sci",
        drug_data = arrow::as_arrow_table(drug_test)
      ) |>
      dplyr::collect()
  })

  expect_equal(
    res$ici1,
    c(1, 1, 1, 0, 0)
  )

  expect_equal(
    res$ici3,
    c(0, 1, 0, 0, 0)
  )

  expect_equal(
    res_a$ici1,
    c(1, 1, 1, 0, 0)
  )

  expect_equal(
    res_a$ici3,
    c(0, 1, 0, 0, 0)
  )
})

test_that("you can choose output column names with d_names", {
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
      DrecNo  = c(21, 22, 23, 21, 21),
      UMCReportId = c(1, 1, 2, 2, 3),
      MedicinalProd_Id = NA
    )

  demo_test <-
    data.table(
      UMCReportId = c(1, 2, 3, 4, 5),
      Region = NA,
      DateDatabase = NA,
      Type = NA,
      # ambiguous column name
      drug_test = c(0, 0, 0, 0, 1)
    )

  changed_names <-
    c("t1", "t2", "t3")

  expect_snapshot({
    res <-
      demo_test |>
      add_drug(
        d_code = d_drecno_test,
        d_names = changed_names,
        method = "DrecNo",
        repbasis = "sci",
        drug_data = drug_test
      )
  })

  expect_equal(
    all(changed_names %in% names(res)),
    TRUE
  )

  expect_snapshot({
    res_a <-
      arrow::as_arrow_table(demo_test) |>
      add_drug(
        d_code = d_drecno_test,
        d_names = changed_names,
        method = "DrecNo",
        repbasis = "sci",
        drug_data = arrow::as_arrow_table(drug_test)
      )
  })

  expect_equal(
    all(changed_names %in% names(res_a)),
    TRUE
  )

})

test_that("you can use arrow/parquet format", {
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
      DrecNo  = c(21, 22, 23, 21, 21),
      UMCReportId = c(1, 1, 2, 2, 3),
      MedicinalProd_Id = NA
    )

  demo_test <-
    data.table(
      UMCReportId = c(1, 2, 3, 4, 5),
      Region = NA,
      DateDatabase = NA,
      Type = NA,
      # ambiguous column name
      drug_test = c(0, 0, 0, 0, 1)
    )

  tmp_folder <- tempdir()

  arrow::write_parquet(demo_test,
                       sink = paste0(tmp_folder, "\\demo.parquet"))
  arrow::write_parquet(drug_test,
                       sink = paste0(tmp_folder, "\\drug.parquet"))

  demo_parquet <- arrow::read_parquet(paste0(tmp_folder, "\\demo.parquet"))
  drug_parquet <- arrow::read_parquet(paste0(tmp_folder, "\\drug.parquet"))

  expect_snapshot({
    res <-
      demo_parquet |>
      add_drug(
        d_code = d_drecno_test,
        method = "DrecNo",
        repbasis = "sci",
        drug_data = drug_parquet
      )
  })

  expect_snapshot({
    res_a <-
      demo_test |>
      add_drug(
        d_code = d_drecno_test,
        method = "DrecNo",
        repbasis = "sci",
        drug_data = drug_test
      )
  })

  expect_equal(res, res_a)


})

test_that("Providing data_type arg raises deprecation warn", {

  expect_snapshot({
  r1 <-
    demo_ |>
    add_drug(
      d_code = ex_$d_drecno,
      method = "DrecNo",
      drug_data = drug_,
      data_type = "demo"
    )
  })
})

test_that("drug_data should be a valid drug type data", {
  expect_snapshot(error = TRUE,
                  {
      demo_ |>
      add_drug(
        d_code = ex_$d_drecno,
        method = "DrecNo",
        drug_data = adr_,
        data_type = "demo"
      )
  })
})
