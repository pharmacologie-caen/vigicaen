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
