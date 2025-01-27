test_that("standard interaction", {
  suppressMessages(
    demo <-
      demo_  |>
      add_drug(d_code = ex_$d_drecno, drug_data = drug_) |>
      add_adr(a_code = ex_$a_llt, adr_data = adr_)
  )

  z = "a_colitis"
  y = "nivolumab"
  x = "ipilimumab"

  res_int <-
    compute_interaction(x = x,
                        y = y,
                        z = z,
                        .data = demo)

  res_Table <-
    demo |>
    arrow::as_arrow_table() |>
    compute_interaction(x = x,
                        y = y,
                        z = z)

  n_exp_true <-
    c(41 * 2 * 26 * 750 / # 2 by 2 pairs and n total
        (25 * 140 * 58) # each variable
      )

  ic_true <-
    log((18 + .5)  / (n_exp_true + .5), base = 2)

  ic_tail_true <-
    ic_tail(18, n_exp_true)

  expect_equal(res_int$n_exp, n_exp_true)

  expect_equal(res_int$ic, ic_true)

  expect_equal(res_int$ic_tail, ic_tail_true)

  expect_equal(res_int, res_Table)

})

test_that("vectorization works", {
  suppressMessages(
    demo <-
      demo_  |>
      add_drug(d_code = ex_$d_drecno, drug_data = drug_) |>
      add_adr(a_code = ex_$a_llt, adr_data = adr_)
  )

  z = c("a_colitis", "a_pneumonitis")
  y = c("nivolumab", "pembrolizumab")
  x = c("ipilimumab", "atezolizumab")

  res_int <-
    compute_interaction(x = x,
                        y = y,
                        z = z,
                        .data = demo)

  # a bit short
  expect_snapshot(res_int)

})

test_that("formatting output works", {
  suppressMessages(
    demo <-
      demo_  |>
      add_drug(d_code = ex_$d_drecno, drug_data = drug_) |>
      add_adr(a_code = ex_$a_llt, adr_data = adr_)
  )

  z = c("a_colitis", "a_pneumonitis")
  y = "nivolumab"
  x = "ipilimumab"

  res_int_raw <-
    compute_interaction(x = x,
                        y = y,
                        z = z,
                        .data = demo,
                        export_raw_values = TRUE)

  # a bit short too
  expect_snapshot(res_int_raw)

  res_int_minn <-
    compute_interaction(x = x,
                        y = y,
                        z = z,
                        .data = demo,
                        min_n_obs = 15)

  # a bit short too
  expect_snapshot(res_int_minn)

})
