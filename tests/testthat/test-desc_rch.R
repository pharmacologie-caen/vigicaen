test_that("find proper counts on a known dataset", {
  rch_test <-
    rch_desc(luda_data = luda_test_all,
             demo_data = demo_rch_test,
             adr_s = "a_colitis",
             drug_s = "pd1")

  true_n_overall <- 10

  expect_equal(
    rch_test$n_overall,
    true_n_overall
  )

  true_n_rch <- 8

  expect_equal(
    rch_test$n_rch,
    true_n_rch
  )

  true_n_inf <- 7

  expect_equal(
    rch_test$n_inf,
    true_n_inf
  )

  true_n_rec <- 7

  expect_equal(
    rch_test$n_rec,
    true_n_rec
  )

  true_n_tto_avail_rch <- 4 # not 5, we're not collecting negative ttos

  expect_equal(
    rch_test$n_tto_avail_rch,
    true_n_tto_avail_rch
  )

  true_tto_rch <-
    paste0(
      (24 + 56) / 2, # median
      " (",
      round(quantile(c(7, 24, 56, 66), .25)),
      "-",
      round(quantile(c(7, 24, 56, 66), .75)),
      ")"
    )

  expect_equal(
    rch_test$tto_rch,
    true_tto_rch
  )

    })
