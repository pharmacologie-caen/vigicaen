test_that("does compute an IC tail", {
  expect_equal(
    ic_tail(
      n_obs = 12,
      n_exp = 5
    ),
    log(qgamma(p = 0.025, shape = 12 + .5, rate = 5 + .5), 2)
  )
})

test_that("fails if n_obs and/or n_exp not provided", {

  expect_error(
    ic_tail(
      n_obs = 12
    ),
    regexp = "n_exp",
   class = "rlang_error"
  )

  expect_error(
    ic_tail(
      n_exp = 5
    ),
    regexp = "n_obs",
    class = "rlang_error"
  )

  expect_error(
    ic_tail(
    ),
    regexp = "n_obs", # first non matching
    class = "rlang_error"
  )
})
