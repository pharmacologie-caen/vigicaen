test_that("finds appropriate counts on a known dataset", {
  link_dch <- data.table(
   UMCReportId = 1:13,
   Drug_Id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
   Adr_Id = c(101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113),
   adr1         = c(1, 1, 1, 1, 1, 1, 1, 1, 0,   0, 0, 0, 0),
   adr2         = c(1, 0, 1, 0, 1, 0, 1, 0, 1,   1, 1, 1, 1),
   drug1        = c(1, 1, 1, 1, 1, 1, 0, 0, 0,   1, 0, 1, 1),
   drug2        = c(0, 1, 0, 1, 1, 0, 1, 0, 1,   0, 1, 0, 1),
   Dechallenge1 =
   as.character(c(  1, 4, 2, 1, 2, 2, 1, 3, 2,   4, 1, 1, 2)),
   Dechallenge2 =
   as.character(c(  1, 2, 1, 1, 3, 2, 4, 1, "-", 3, 2, 2, 3)),
   tto_mean = 1,
   range = 1
  )

  r1 <- desc_dch(link_dch,
          adr_s = "adr1",
          drug_s = "drug1")


  true_r1 <-
    data.frame(
      drug_s = "drug1",
      adr_s = "adr1",
      pos_dch = 3
    )

  expect_equal(
    r1,
    true_r1
  )

})

test_that("works with named arguments and lists", {
  link_dch <- data.table(
    UMCReportId = 1:13,
    Drug_Id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
    Adr_Id = c(101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113),
    adr1         = c(1, 1, 1, 1, 1, 1, 1, 1, 0,   0, 0, 0, 0),
    adr2         = c(1, 0, 1, 0, 1, 0, 1, 0, 1,   1, 1, 1, 1),
    drug1        = c(1, 1, 1, 1, 1, 1, 0, 0, 0,   1, 0, 1, 1),
    drug2        = c(0, 1, 0, 1, 1, 0, 1, 0, 1,   0, 1, 0, 1),
    Dechallenge1 =
      as.character(c(  1, 4, 2, 1, 2, 2, 1, 3, 2,   4, 1, 1, 2)),
    Dechallenge2 =
      as.character(c(  1, 2, 1, 1, 3, 2, 4, 1, "-", 3, 2, 2, 3)),
    tto_mean = 1,
    range = 1
  )

  drug_choice <-
    c("d1" = "drug1")

  drug_list <-
    list(d1 = "drug1")

  r1_named <- desc_dch(link_dch,
                 adr_s = "adr1",
                 drug_s = drug_choice)

  r1_list <- desc_dch(link_dch,
                       adr_s = "adr1",
                       drug_s = drug_list)

  r1 <- desc_dch(link_dch,
                 adr_s = "adr1",
                 drug_s = "drug1")

  expect_equal(
    r1,
    r1_named
  )


  expect_equal(
    r1,
    r1_list
  )

})

test_that("works with vectorization", {
  link_dch <- data.table(
    UMCReportId = 1:13,
    Drug_Id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
    Adr_Id = c(101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113),
    adr1         = c(1, 1, 1, 1, 1, 1, 1, 1, 0,   0, 0, 0, 0),
    adr2         = c(1, 0, 1, 0, 1, 0, 1, 0, 1,   1, 1, 1, 1),
    drug1        = c(1, 1, 1, 1, 1, 1, 0, 0, 0,   1, 0, 1, 1),
    drug2        = c(0, 1, 0, 1, 1, 0, 1, 0, 1,   0, 1, 0, 1),
    Dechallenge1 =
      as.character(c(  1, 4, 2, 1, 2, 2, 1, 3, 2,   4, 1, 1, 2)),
    Dechallenge2 =
      as.character(c(  1, 2, 1, 1, 3, 2, 4, 1, "-", 3, 2, 2, 3)),
    tto_mean = 1,
    range = 1
  )

  r1 <- desc_dch(link_dch,
                 adr_s = c("adr1", "adr2"),
                 drug_s = c("drug1")
  )

  true_r1 <-
    data.frame(
      drug_s = "drug1",
      adr_s = c("adr1", "adr2"),
      pos_dch = c(3, 2)
    )

  expect_equal(
    r1,
    true_r1
  )

  r2 <- desc_dch(link_dch,
                 adr_s = c("adr1"),
                 drug_s = c("drug1", "drug2")
  )

  true_r2 <-
    data.frame(
      drug_s = c("drug1", "drug2"),
      adr_s = c("adr1"),
      pos_dch = c(3, 1)
    )

  expect_equal(
    r2,
    true_r2
  )


})
