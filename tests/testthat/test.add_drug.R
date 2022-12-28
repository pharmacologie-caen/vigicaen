test_that("works with drecnos, regular names for demo and drug", {

  demo <- demo_
  drug <- drug_

  d_names <- rlang::list2(
    nivolumab = "nivolumab",
    pembrolizumab = "pembrolizumab"
    )

  d_count <- rlang::list2(
    nivolumab     = 36729,
    pembrolizumab = 18994
  )

  n_drug <- length(d_names)

  d_drecno <-
    d_names %>%
    get_drecno(
      mp_short = ex_$mp_short
    )

  demo <-
    demo_ %>%
    add_drug(
      d_code = d_drecno,
      method = "DrecNo",
      repbasis = "sci",
      drug_data = drug
    )

  expect_equal(ncol(demo),
               ncol(demo_) + n_drug)

  purrr::iwalk(
    d_count,
    function(d_n, id_n){
      expect_equal(sum(demo[[id_n]]),
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
    nivolumab     = 36729,
    pembrolizumab = 18994
  )

  n_drug <- length(d_names)

  d_drecno <-
    d_names %>%
    get_drecno(
      mp_short = ex_$mp_short
    )

  dema <-
    dema %>%
    add_drug(
      d_code = d_drecno,
      method = "DrecNo",
      repbasis = "sci",
      drug_data = druga
    )

  expect_equal(ncol(dema),
               ncol(demo_) + n_drug)

  purrr::iwalk(
    d_count,
    function(d_n, id_n){
      expect_equal(sum(demo[[id_n]]),
                   expected = d_n)
    }
  )

})

test_that("works with mpi_list", {

  mpi <- rlang::list2(
    para = ex_$mp_short[DrecNo == "000200", MedicinalProd_Id]
  )

  mpi_count <- rlang::list2(
    para = 3311
  )

  n_drug <- length(mpi)

  demo <-
    demo_ %>%
    add_drug(
      d_code = mpi,
      method = "MedicinalProd_Id",
      repbasis = "sci",
      drug_data = drug_
    )

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
    d_names %>%
    get_drecno(
      mp_short = ex_$mp_short
    )

  bas <- c("s", "c", "i")

  res_each <-
    purrr::map(bas, function(repbasis_)
      demo_ %>%
        add_drug(
          d_code = d_drecno,
          method = "DrecNo",
          repbasis = repbasis_,
          drug_data = drug_
        ))

  res_all <-
    demo_ %>%
    add_drug(
      d_code = d_drecno,
      method = "DrecNo",
      repbasis = "sci",
      drug_data = drug_
    )

  res_each_nivo_sum <-
    res_each %>%
    purrr::map_dbl(function(x)
      sum(x[[names(d_names)[1]]]))

  res_all_nivo_sum <-
    sum(res_all[[names(d_names)[1]]])

  expect_equal(ncol(res_all),
               ncol(demo_) + n_drug)

  # found cases with s, c, i
  purrr::walk(
    res_each_nivo_sum,
    function(x){
      expect_gt(x,
                expected = 1)
    }
  )

  # found equally or more cases with s, c, i alone and sci altogether (see below)
  expect_gte(sum(res_each_nivo_sum),
             res_all_nivo_sum)

  # found different number of reports with s, c, i
  expect_equal(res_each_nivo_sum[1] == res_each_nivo_sum[2], FALSE)
  expect_equal(res_each_nivo_sum[2] == res_each_nivo_sum[3], FALSE)
  expect_equal(res_each_nivo_sum[1] == res_each_nivo_sum[3], FALSE)

  # additional details : why is it ok to have MORE reports with sum(res_each_nivo_sum) ? Because some reports have the same drug reported with 2 or 3 repbasis (i.e. suspect and interacting, concomitant and interacting)

  # this is the demonstration
  # drecno_nivo <-
  #   ex_$d_drecno %>%
  #   filter(drug == "nivolumab") %>%
  #   pull(DrecNo)
  #
  # drug %>%
  #   filter(DrecNo %in% drecno_nivo) %>%
  #   add_count(UMCReportId) %>%
  #   filter(n > 1) %>%
  #   group_by(UMCReportId) %>%
  #   filter(length(unique(Basis)) > 1)

})



