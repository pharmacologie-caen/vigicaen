test_that("accurate results", {
  expect_snapshot({
    demo <-
      demo_ %>%
      add_drug(d_code = ex_$d_drecno, drug_data = drug_) %>%
      add_adr(a_code = ex_$a_llt, adr_data = adr_)
  })

  # Compute the model
  mod <- glm(a_colitis ~ nivolumab, data = demo, family = "binomial")

  # Extract coefficients
  coef_table <-
   mod %>%
   summary() %>%
   .$coefficients

  # Transform coefficients into ORs with their CI

  r1 <-
    coef_table %>%
    compute_or_mod(
      estimate = Estimate,
      std_er = Std..Error,
      p_val = Pr...z..)

  r1_expect <- rlang::list2(
    orl = c("0.13", "1.88"),
    low_ci = c("0.10", "1.23")
  )

  expect_equal(
    r1$orl,
    r1_expect[["orl"]]
  )

  expect_equal(
    cff(r1$low_ci, dig = 2),
    r1_expect[["low_ci"]]
  )
})

test_that("works with and without p_val arg", {
  expect_snapshot({
    demo <-
      demo_ %>%
      add_drug(d_code = ex_$d_drecno, drug_data = drug_) %>%
      add_adr(a_code = ex_$a_llt, adr_data = adr_)
  })

  # Compute the model
  mod <- glm(a_colitis ~ nivolumab, data = demo, family = "binomial")

  # Extract coefficients
  coef_table <-
    mod %>%
    summary() %>%
    .$coefficients

  # Transform coefficients into ORs with their CI

  r1 <-
    coef_table %>%
    compute_or_mod(
      estimate = Estimate,
      std_er = Std..Error,
      p_val = Pr...z..)

  r1_nop <-
    coef_table %>%
    compute_or_mod(
      estimate = Estimate,
      std_er = Std..Error
    )

  expect_equal(
    r1$or,
    r1_nop$or
  )

  expect_equal(
    r1$p_val,
    c("<.0001", "<.01")
  )

  expect_null(
    r1_nop$p_val
  )
})
