test_that("find proper counts on a known dataset", {
  rch_test <-
    desc_rch(luda_data = luda_,
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


    })

test_that("works with few data", {
   luda_rch <- data.table(
     UMCReportId = c(1, 1, 2, 3, 4, 5, 5, 6, 7, 8),
     Drug_Id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
     Adr_Id = c(101, 102, 103, 104, 105, 106, 107, 108, 109, 110),
     adr1         = c(0, 1, 1, 0, 1, 0, 1, 1, 1,  0),
     adr2         = c(1, 0, 1, 0, 1, 0, 1, 0, 0,  1),
     drug1        = c(1, 0, 1, 1, 0, 1, 0, 1, 1,  1),
     drug2        = c(0, 1, 0, 1, 1, 0, 1, 0, 1,  0),
     Dechallenge1 =
     as.character(c(  3, 1, 5, 2, 4, 6, 1, 3, 2,  4)),
     Dechallenge2 =
     as.character(c(  4, 2, 1, 5, 3, 2, 4, 1, "-", 3)),
     Rechallenge1 =
     as.character(c(  1, 1, 1, 1, 1, 1, 0, 0, 0,   0)),
     Rechallenge2 =
     as.character(c(  3, 3, 3, 3, 3, 3, 0, 0, 0,   0))
   )

   demo_rch_test <- data.table(
     UMCReportId =
       c(1, 2, 3,4 ,5, 6, 7, 8)
   )


   res <- desc_rch(
     luda_data = luda_rch,
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
