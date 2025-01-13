test_that("find proper ttos on a known dataset", {

  expect_snapshot({
    link_ <-
      link_ %>%
      add_drug(d_code = ex_$d_groups_drecno, drug_data = drug_) %>%
      add_adr(a_code = ex_$a_llt, adr_data = adr_)
  })


   tto_test <-
    desc_tto(.data = link_,
                adr_s = "a_colitis",
                drug_s = "pd1")


  expect_equal(
    tto_test$value,
    "59.0 (26.5-190.0) [1.0-1,207.0]"
  )
})

test_that("works with vectorization", {

  expect_snapshot({
    link_ <-
      link_ %>%
      add_drug(d_code = ex_$d_groups_drecno, drug_data = drug_) %>%
      add_adr(a_code = ex_$a_llt, adr_data = adr_)
  })

  tto_test <-
    desc_tto(.data = link_,
             adr_s = c("a_colitis", "a_pneumonitis"),
             drug_s = "pd1")


  expect_equal(
    tto_test$value,
    c("59.0 (26.5-190.0) [1.0-1,207.0]",
      "61.5 (32.8-127.8) [0.0-1,050.0]"
    )
  )

  expect_equal(
    tto_test$adr_s,
    c("a_colitis", "a_pneumonitis")
  )

  tto_test2 <-
    desc_tto(.data = link_,
             adr_s = c("a_colitis"),
             drug_s = c("pd1", "pdl1")
    )

  expect_equal(
    tto_test2$value,
    c("59.0 (26.5-190.0) [1.0-1,207.0]",
      "67.0 (18.5-114.0) [1.0-681.0]"
    )
  )

  expect_equal(
    tto_test2$drug_s,
    c("pd1", "pdl1")
  )
})

test_that("format is appropriately passed to desc_cont", {


  expect_snapshot({
    link_ <-
      link_ %>%
      add_drug(d_code = ex_$d_groups_drecno, drug_data = drug_) %>%
      add_adr(a_code = ex_$a_llt, adr_data = adr_)
  })


  tto_test <-
    desc_tto(.data = link_,
             adr_s = "a_colitis",
             drug_s = "pd1",
             format = "median [q1-q3] ; (min to max)")


  expect_equal(
    tto_test$value,
    "59.0 [26.5-190.0] ; (1.0 to 1,207.0)"
  )
})

test_that("digits are appropriately passed to desc_cont", {

  expect_snapshot({
    link_ <-
      link_ %>%
      add_drug(d_code = ex_$d_groups_drecno, drug_data = drug_) %>%
      add_adr(a_code = ex_$a_llt, adr_data = adr_)
  })


  tto_test <-
    desc_tto(.data = link_,
             adr_s = "a_colitis",
             drug_s = "pd1",
             digits = 0)


  expect_equal(
    tto_test$value,
    "59 (26-190) [1-1,207]"
  )
})



