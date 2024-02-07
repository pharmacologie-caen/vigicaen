test_that("finds appropriate counts on a known dataset", {
  luda_dch <- data.table(
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
   as.character(c(  1, 2, 1, 1, 3, 2, 4, 1, "-", 3, 2, 2, 3))
  )

  r1 <- desc_dch(luda_dch,
          adr_s = "adr1",
          drug_s = "drug1",
          display_all_levels = FALSE)


  true_r1 <-
    data.frame(
      drug_s = "drug1",
      adr_s = "adr1",
      drug_action = c("Drug withdrawn",
                      "Dose reduced",
                      "Dose reduced",
                      "Dose reduced",
                      "Dose not changed"
                      ),
      adr_outcome = c("Reaction abated",
                      "No effect observed",
                      "Reaction abated",
                      "Fatal",
                      "Fatal"
                      ),
      n = c(2, 1, 1, 1, 1)
    ) %>%
    dplyr::as_tibble()

  expect_equal(
    r1,
    true_r1
  )

})

test_that("can display combinations with zero cases", {
  luda_dch <- data.table(
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
      as.character(c(  1, 2, 1, 1, 3, 2, 4, 1, "-", 3, 2, 2, 3))
  )

  r1 <- desc_dch(luda_dch,
                 adr_s = "adr1",
                 drug_s = "drug1",
                 display_all_levels = TRUE)

  true_r1_nrow <- 30

  true_r1_counts <-
    c(0, 2, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, rep(0, 17))

  expect_equal(
    c(nrow(r1), r1$n),
    c(true_r1_nrow, true_r1_counts)
  )

})


test_that("works with vectorization", {
  luda_dch <- data.table(
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
      as.character(c(  1, 2, 1, 1, 3, 2, 4, 1, "-", 3, 2, 2, 3))
  )

  r1 <- desc_dch(luda_dch,
                 adr_s = c("adr1", "adr2"),
                 drug_s = c("drug1"),
                 display_all_levels = FALSE
  )

  true_r1 <-
    data.frame(
      drug_s = "drug1",
      adr_s = c(rep("adr1", 5), rep("adr2", 5)),
      drug_action = c("Drug withdrawn",
                      "Dose reduced",
                      "Dose reduced",
                      "Dose reduced",
                      "Dose not changed",
                      "Drug withdrawn",
                      "Drug withdrawn",
                      "Dose reduced",
                      "Dose reduced",
                      "Dose not changed"
                      ),
      adr_outcome = c("Reaction abated",
                      "No effect observed",
                      "Reaction abated",
                      "Fatal",
                      "Fatal",
                      "Reaction abated",
                      "Fatal",
                      "No effect observed",
                      "Reaction abated",
                      "No effect observed"
                      ),
      n = c(2, 1, 1, 1, 1, 1, 1, 2, 1, 1)
    ) %>%
    dplyr::as_tibble()

  expect_equal(
    r1,
    true_r1
  )

  r2 <- desc_dch(luda_dch,
                 adr_s = c("adr1"),
                 drug_s = c("drug1", "drug2"),
                 display_all_levels = FALSE
  )

  r2_nrow <- 9
  r2_counts <- c(2, 1, 1, 1, 1, 1, 1, 1, 1)

  expect_equal(
    c(nrow(r2), r2$n),
    c(r2_nrow, r2_counts)
  )

})
