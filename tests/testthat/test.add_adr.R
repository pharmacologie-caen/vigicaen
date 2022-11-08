context("add_adr")

test_that("works with regular names for demo and adr", {

  demo <- demo_
  adr <- adr_

  n_adr <- length(ex_$adr_list)

  a_names <- paste0("adr_", names(ex_$adr_list))

  demo <-
   demo %>%
     add_adr(
       adr_list = ex_$adr_list,
       a_names = a_names,
       adr = adr
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

  n_adr <- length(ex_$adr_list)

  a_names <- paste0("adr_", names(ex_$adr_list))

  dema <-
    dema %>%
    add_adr(
      adr_list = ex_$adr_list,
      a_names = a_names,
      adr = adra
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
