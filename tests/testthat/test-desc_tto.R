test_that("find proper ttos on a known dataset", {
  tto_test <-
    desc_tto(luda_data = luda_,
                adr_s = "a_colitis",
                drug_s = "pd1")


  expect_equal(
    tto_test$value,
    "174.0 (115.0-311.0) [56.0-448.0]"
  )
})

test_that("works with vectorization", {
  tto_test <-
    desc_tto(luda_data = luda_,
             adr_s = c("a_colitis", "a_pneumonitis"),
             drug_s = "pd1")


  expect_equal(
    tto_test$value,
    c("174.0 (115.0-311.0) [56.0-448.0]",
      "115.0 (36.5-290.8) [17.0-602.0]"
    )
  )

  expect_equal(
    tto_test$adr_s,
    c("a_colitis", "a_pneumonitis")
  )
})

test_that("format is appropriately passed to desc_cont", {
  tto_test <-
    desc_tto(luda_data = luda_,
             adr_s = "a_colitis",
             drug_s = "pd1",
             format = "median [q1-q3] ; (min to max)")


  expect_equal(
    tto_test$value,
    "174.0 [115.0-311.0] ; (56.0 to 448.0)"
  )
})

test_that("digits are appropriately passed to desc_cont", {
  tto_test <-
    desc_tto(luda_data = luda_,
             adr_s = "a_colitis",
             drug_s = "pd1",
             digits = 0)


  expect_equal(
    tto_test$value,
    "174 (115-311) [56-448]"
  )
})



