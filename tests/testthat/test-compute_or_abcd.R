test_that("computation is accurate", {
  demo <-
    demo %>%
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
      y = "colitis",
      x = "nivolumab"
    )

  exp_res <- rlang::list2(
    ror = cff(0.75, dig = 2),
    ic = cff(-0.20, dig = 2)
  )

  expect_equal(
    res[["rorl"]],
    exp_res[["ror"]]
  )

  expect_equal(
    cff(res[["ic"]], dig = 2),
    exp_res[["ic"]]
  )
})
