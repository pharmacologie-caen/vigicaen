test_that("find proper counts on a known dataset", {
  rch_test <-
    rch_desc(luda_data = luda_,
             demo_data = demo_rch_,
             adr_s = "a_colitis",
             drug_s = "pd1")

  true_n_overall <- 8

  expect_equal(
    rch_test$n_overall,
    true_n_overall
  )

  true_n_rch <- 6

  expect_equal(
    rch_test$n_rch,
    true_n_rch
  )

  true_n_inf <- 5

  expect_equal(
    rch_test$n_inf,
    true_n_inf
  )

  true_n_rec <- 5

  expect_equal(
    rch_test$n_rec,
    true_n_rec
  )

  true_n_tto_avail_rch <- 3 # we're not collecting negative ttos

  expect_equal(
    rch_test$n_tto_avail_rch,
    true_n_tto_avail_rch
  )

  true_tto_rch <-
    paste0(
      (174), # median
      " (",
      round(quantile(c(56, 174, 448), .25)),
      "-",
      round(quantile(c(56, 174, 448), .75)),
      ")"
    )

  expect_equal(
    rch_test$tto_rch,
    true_tto_rch
  )

    })

test_that("output type is consistent in presence or absence of tto data", {
  rch_a1 <- # adr with some data for n_tto_avail_no_rch
    rch_desc(luda_data = luda_,
             demo_data = demo_rch_,
             adr_s = "a_colitis",
             drug_s = "pd1")

  rch_a2 <- # adr with no data for n_tto_avail_no_rch
    rch_desc(luda_data = luda_,
             demo_data = demo_rch_,
             adr_s = "a_pneumonitis",
             drug_s = "pd1")

  # counts (n) should all be of type "integer"
  expect_equal(
    class(rch_a1$n_tto_avail_no_rch),
    class(rch_a2$n_tto_avail_no_rch)
  )

  expect_equal(
    class(rch_a1$n_tto_avail_no_rch),
    "integer"
  )

  expect_equal(
    class(rch_a2$n_tto_avail_no_rch),
    "integer"
  )

  # generalization
  all_classes_a1 <-
    lapply(rch_a1, class)

  all_classes_a2 <-
    lapply(rch_a2, class)

  expect_equal(
    all_classes_a1,
    all_classes_a2
  )
})
