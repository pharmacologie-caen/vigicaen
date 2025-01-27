test_that("find proper counts on a known dataset", {

  expect_snapshot({
    link_ <-
      link_ %>%
      add_drug(d_code = ex_$d_groups_drecno, drug_data = drug_) %>%
      add_adr(a_code = ex_$a_llt, adr_data = adr_)
  })

  rch_test <-
    desc_rch(.data = link_,
             demo_data = demo_,
             adr_s = "a_colitis",
             drug_s = "pd1")

  true_n_overall <- 81

  expect_equal(
    rch_test$n_overall,
    true_n_overall
  )

  true_n_rch <- 54

  expect_equal(
    rch_test$n_rch,
    true_n_rch
  )

  true_n_inf <- 44

  expect_equal(
    rch_test$n_inf,
    true_n_inf
  )

  true_n_rec <- 16

  expect_equal(
    rch_test$n_rec,
    true_n_rec
  )


    })

test_that("can be vectorized", {


  expect_snapshot({
    link_ <-
      link_ %>%
      add_drug(d_code = ex_$d_groups_drecno, drug_data = drug_) %>%
      add_adr(a_code = ex_$a_llt, adr_data = adr_)
  })

  rch_test <-
    desc_rch(.data = link_,
             demo_data = demo_,
             adr_s = c("a_colitis", "a_pneumonitis"),
             drug_s = "pd1")

  true_rch <-
    data.table(
      drug_s = c("pd1", "pd1"),
      adr_s = c("a_colitis", "a_pneumonitis"),
      n_overall = c(81, 96),
      n_rch     = c(54, 66),
      n_inf     = c(44, 55),
      n_rec     = c(16, 12)
    )


  expect_equal(
    rch_test,
    true_rch
  )


  rch_test2 <-
    desc_rch(.data = link_,
             demo_data = demo_,
             adr_s = c("a_colitis"),
             drug_s = c("pd1", "pdl1")
    )

  true_rch2 <-
    data.table(
      drug_s = c("pd1", "pdl1"),
      adr_s = c("a_colitis", "a_colitis"),
      n_overall = c(81, 16),
      n_rch     = c(54, 7),
      n_inf     = c(44, 0),
      n_rec     = c(16, 0)
    )


  expect_equal(
    rch_test2,
    true_rch2
  )

})

test_that("works with few data", {
   link_rch <- data.table(
     UMCReportId = c(1, 1, 2, 3, 4, 5, 5, 6, 7, 8),
     Drug_Id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
     Adr_Id = c(101, 102, 103, 104, 105, 106, 107, 108, 109, 110),
     adr1         = c(0, 1, 1, 0, 1, 0, 1, 1, 1,   0),
     adr2         = c(1, 0, 1, 0, 1, 0, 1, 0, 0,   1),
     drug1        = c(1, 0, 1, 1, 0, 1, 0, 1, 1,   1),
     drug2        = c(0, 1, 0, 1, 1, 0, 1, 0, 1,   0),
     Dechallenge1 =
     as.character(c(  3, 1, 5, 2, 4, 6, 1, 3, 2,   4)),
     Dechallenge2 =
     as.character(c(  4, 2, 1, 5, 3, 2, 4, 1, "-", 3)),
     Rechallenge1 =
     as.character(c(  1, 1, 1, 1, 1, 1, 0, 0, 0,   0)),
     Rechallenge2 =
     as.character(c(  3, 3, 3, 3, 3, 3, 0, 0, 0,   0)),
     tto_mean = 1,
     range = 1
   )

   demo_rch_test <- data.table(
     UMCReportId =
       c(1, 2, 3,4 ,5, 6, 7, 8)
   )


   res <- desc_rch(
     .data = link_rch,
     demo_data = demo_rch_test,
     adr_s = "adr1",
     drug_s = "drug1"
   )

   expect_equal(
     res$n_rch,
     1
   )

   expect_equal(
     res$n_inf,
     0
   )
})
