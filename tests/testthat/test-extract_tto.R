test_that("find proper ttos on a known dataset", {
  tto_test <-
    extract_tto(luda_data = luda_,
                adr_s = "a_colitis",
                drug_s = "pd1")

  true_n_tto_avail <- 3 # we're not collecting negative ttos

  expect_equal(
    nrow(tto_test),
    true_n_tto_avail
  )

  true_tto_rch <-
    c(56, 174, 448)

  expect_equal(
    tto_test$tto_max,
    true_tto_rch
  )
})

test_that("works with vectorized adrs and drugs", {
  tto_many_adr <-
    extract_tto(luda_data = luda_,
             adr_s = c("a_colitis", "a_pneumonitis"),
             drug_s = c("pd1"))


  correct_many_adr <-
    data.frame(
      tto_max = c(56, 174, 448, 602, 43, 187, 17),
      adr_s  = c(rep("a_colitis", 3), rep("a_pneumonitis", 4))
    )

  expect_equal(
    tto_many_adr %>%
      select(tto_max, adr_s),
    correct_many_adr
  )

  tto_many_drug <-
    extract_tto(luda_data = luda_,
                adr_s = c("a_colitis"),
                drug_s = c("pd1", "pdl1"))

  correct_many_drug <-
    data.frame(
      tto_max = c(56, 174, 448, 681),
      drug_s  = c("pd1", "pd1", "pd1", "pdl1")
    )

  expect_equal(
    tto_many_drug %>%
      select(tto_max, drug_s),
    correct_many_drug
  )

  tto_many_both <-
    extract_tto(luda_data = luda_,
                adr_s = c("a_colitis", "a_pneumonitis"),
                drug_s = c("pd1", "pdl1"))

  correct_many_both <-
    data.frame(
      tto_max = c(56, 174, 448, 681, 602, 43, 187, 17, 84),
      drug_s = c(rep("pd1", 3), "pdl1", rep("pd1", 4), "pdl1"),
      adr_s  = c(rep("a_colitis", 4), rep("a_pneumonitis", 5))
    )

  expect_equal(
    tto_many_both %>%
      select(tto_max, drug_s, adr_s),
    correct_many_both
  )

})

test_that("output type is consistent in presence or absence of tto data", {
  tto_a1 <- # adr with some tto data
    extract_tto(luda_data = luda_,

                adr_s = "a_colitis",
                drug_s = "pd1")

  # adr with no tto data

  expect_warning(
    # https://stackoverflow.com/questions/60417969/r-how-to-omit-tested-warning-message-from-test-report-when-testing-for-result-a
    tto_a2 <<-
      extract_tto(luda_data = luda_ %>%
                  filter(a_pneumonitis == 0),

                adr_s = "a_pneumonitis",
                drug_s = "pd1")
    )

  # counts (n) should all be of type "integer"
  expect_equal(
    class(tto_a1$tto_max),
    class(tto_a2$tto_max)
  )

  expect_equal(
    class(tto_a1$tto_max),
    "numeric"
  )

  # generalization
  all_classes_a1 <-
    lapply(tto_a1, class)

  all_classes_a2 <-
    lapply(tto_a2, class)

  expect_equal(
    all_classes_a1,
    all_classes_a2
  )
})
