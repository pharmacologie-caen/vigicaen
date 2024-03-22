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

test_that("vectorization works inside and outside the function", {
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

  many_drugs <-
    c("nivolumab",
      "pembrolizumab"
      )

  many_adrs <-
    c("a_colitis",
      "a_pneumonitis"
      )

  true_orl <-
    c("1.88",
      "1.75",
      "0.94",
      "1.82")

  vect_outside <-
    many_drugs %>%
   purrr::map(
     function(a_drug) {
       many_adrs |>
         purrr::map(
           function(an_adr)
             demo |>
             compute_or_abcd(
               y = an_adr,
               x = a_drug
               )
           ) |>
         purrr::list_rbind()
     }
   )  |>
   purrr::list_rbind()

  vect_inside <-
    demo |>
    compute_or_abcd(
      y = many_adrs,
      x = many_drugs
      )

  expect_equal(vect_outside, vect_inside)

  expect_equal(vect_inside$orl,
               true_orl)


})
