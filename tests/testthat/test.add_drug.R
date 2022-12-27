test_that("works with drecnos, regular names for demo and drug", {

  demo <- demo_
  drug <- drug_

  d_names <- c("nivolumab", "pembrolizumab")

  n_drug <- length(d_names)

  demo <-
    demo %>%
    add_drug(
      exposure_df = ex_$d_drecno,
      drug_names = d_names,
      method = "DrecNo",
      repbasis = "sci",
      drug_data = drug
    )

  expect_equal(ncol(demo),
               ncol(demo_) + n_drug)

  purrr::walk(
    d_names,
    function(d_n){
      expect_gt(sum(demo[[d_n]]),
                expected = 1)
    }
  )

})

test_that("works with irregular names for demo and drug", {

  dema <- demo_
  druga <- drug_

  d_names <- c("nivolumab", "pembrolizumab")

  n_drug <- length(d_names)

  dema <-
    dema %>%
    add_drug(
      exposure_df = ex_$d_drecno,
      drug_names = d_names,
      method = "DrecNo",
      repbasis = "sci",
      drug_data = druga
    )

  expect_equal(ncol(dema),
               ncol(demo_) + n_drug)

  purrr::walk(
    d_names,
    function(d_n){
      expect_gt(sum(dema[[d_n]]),
                expected = 1)
    }
  )

})

test_that("works with mpi_list", {

  demo <- demo_
  drug <- drug_

  d_names <- c("nivolumab", "pembrolizumab")

  n_drug <- length(d_names)

  demo <-
    demo %>%
    add_drug(
      exposure_df = ex_$d_drecno,
      drug_names = d_names,
      method = "MedicinalProd_Id",
      repbasis = "sci",
      drug_data = drug
    )

  expect_equal(ncol(demo),
               ncol(demo_) + n_drug)

  purrr::walk(
    d_names,
    function(d_n){
      expect_gt(sum(demo[[d_n]]),
                expected = 1)
    }
  )

})

test_that("selecting only s, c, i works and provide less cases than sci altogether", {
  demo <- demo_
  drug <- drug_

  d_names <- c("nivolumab", "pembrolizumab")

  n_drug <- length(d_names)

  bas <- c("s", "c", "i")

  res_each <-
    purrr::map(bas, function(repbasis_)
      demo %>%
        add_drug(
          exposure_df = ex_$d_drecno,
          drug_names = d_names,
          method = "DrecNo",
          repbasis = repbasis_,
          drug_data = drug
        ))

  res_all <-
    demo %>%
    add_drug(
      exposure_df = ex_$d_drecno,
      drug_names = d_names,
      method = "DrecNo",
      repbasis = "sci",
      drug_data = drug
    )

  res_each_nivo_sum <-
    res_each %>%
    purrr::map_dbl(function(x)
      sum(x[[d_names[1]]]))

  res_all_nivo_sum <-
    sum(res_all[[d_names[1]]])

  expect_equal(ncol(demo),
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



