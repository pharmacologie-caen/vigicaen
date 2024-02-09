test_that("computation is accurate", {
  demo <-
    demo_ %>%
    add_drug(
      d_code = ex_$d_drecno,
      drug_data = drug_
    ) %>%
    add_adr(
      a_code = ex_$a_llt,
      adr_data = adr_
    )

  res <-
    demo %>%
    compute_or_abcd(
      y = "a_colitis",
      x = "nivolumab"
    )

  exp_res <- rlang::list2(
    or = cff(1.88, dig = 2),
    ic = cff(0.49, dig = 2)
  )

  expect_equal(
    res[["orl"]],
    exp_res[["or"]]
  )

  expect_equal(
    cff(res[["ic"]], dig = 2),
    exp_res[["ic"]]
  )
})
