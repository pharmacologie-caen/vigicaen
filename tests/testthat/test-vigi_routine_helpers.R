# Tests for the internal vigi_routine() helpers vr_case_sets() and
# vr_ic_from_counts().

test_that("vr_ic_from_counts reproduces compute_dispro's IC on the same 2x2", {
  a <- 8; b <- 12; c <- 30; d <- 500

  # a table with exactly these a/b/c/d counts for one drug x one adr
  df <- data.frame(
    drug = c(rep(1, a + b), rep(0, c + d)),
    adr  = c(rep(1, a), rep(0, b), rep(1, c), rep(0, d))
  )

  cd <- compute_dispro(df, y = "adr", x = "drug", export_raw_values = TRUE)
  vr <- vr_ic_from_counts(a = a, b = b, c = c, d = d)

  # the IC and its building blocks must match compute_dispro exactly
  expect_equal(vr$n_obs,   cd$n_obs)
  expect_equal(vr$n_exp,   cd$n_exp)
  expect_equal(vr$ic,      cd$ic)
  expect_equal(vr$ic_tail, cd$ic_tail)
  expect_equal(vr$a, cd$a)
  expect_equal(vr$d, cd$d)
  # drug / adr marginals agree (compute_dispro labels b/c the other way round)
  expect_equal(vr$a + vr$b, cd$a + cd$c)   # n with the drug
  expect_equal(vr$a + vr$c, cd$a + cd$b)   # n with the adr
})

test_that("vr_ic_from_counts returns the documented columns and a positive signal", {
  vr <- vr_ic_from_counts(a = 50, b = 100, c = 200, d = 100000)

  expect_named(
    vr,
    c("a", "b", "c", "d", "n_obs", "n_exp", "ic", "ic_tail",
      "ci_level", "signif_ic")
  )
  expect_equal(nrow(vr), 1L)
  expect_equal(vr$n_obs, 50)
  # strong over-representation -> IC025 > 0 -> signalled
  expect_gt(vr$ic_tail, 0)
  expect_equal(vr$signif_ic, 1)
  expect_equal(vr$ci_level, "95%")
})

test_that("vr_ic_from_counts honours alpha and min_n_obs", {
  # alpha drives the credibility level label
  expect_equal(vr_ic_from_counts(10, 10, 10, 1000, alpha = 0.01)$ci_level, "99%")

  # below min_n_obs, numeric outputs are blanked to NA. (`a` itself is blanked
  # first, so the downstream ci_level test sees NA and also yields NA rather than
  # na_format -- preserved verbatim from the original inline code. In practice
  # vigi_routine() always calls with min_n_obs = 0, so this branch is inert.)
  blanked <- vr_ic_from_counts(3, 10, 10, 1000, min_n_obs = 5, na_format = "-")
  expect_true(is.na(blanked$ic))
  expect_true(is.na(blanked$ic_tail))
  expect_true(is.na(blanked$signif_ic))
  expect_true(is.na(blanked$ci_level))
})

test_that("vr_case_sets identifies drug, adr and intersection case sets", {
  d_code <- ex_$d_drecno["nivolumab"]
  a_code <- ex_$a_llt["a_colitis"]
  basis_sel <- c(1, 2, 3)

  exp_drug <- drug_ |>
    dplyr::filter(.data$DrecNo %in% d_code[[1]] & .data$Basis %in% basis_sel) |>
    dplyr::pull(.data$UMCReportId) |>
    unique()
  exp_adr <- adr_ |>
    dplyr::filter(.data$MedDRA_Id %in% a_code[[1]]) |>
    dplyr::pull(.data$UMCReportId) |>
    unique()

  cs <- vr_case_sets(drug_, adr_, d_code, a_code, basis_sel)

  expect_setequal(cs$umc_drug, exp_drug)
  expect_setequal(cs$umc_adr, exp_adr)
  expect_setequal(cs$umc_cases, intersect(exp_drug, exp_adr))
})

test_that("vr_case_sets dual-drug mode intersects the two drugs' cases", {
  d_code   <- ex_$d_drecno["nivolumab"]
  d_code_2 <- ex_$d_drecno["ipilimumab"]
  a_code   <- ex_$a_llt["a_colitis"]
  basis_sel <- c(1, 2, 3)

  umc_for <- function(code) {
    drug_ |>
      dplyr::filter(.data$DrecNo %in% code & .data$Basis %in% basis_sel) |>
      dplyr::pull(.data$UMCReportId) |>
      unique()
  }
  exp_drug <- intersect(umc_for(d_code[[1]]), umc_for(d_code_2[[1]]))

  cs <- vr_case_sets(drug_, adr_, d_code, a_code, basis_sel,
                     d_code_2 = d_code_2)

  expect_setequal(cs$umc_drug, exp_drug)
  expect_true(all(cs$umc_cases %in% cs$umc_drug))
})
