test_that("computation is accurate", {
  expect_message(expect_message({
    demo <-
      demo_  |>
      add_drug(d_code = ex_$d_drecno, drug_data = drug_) |>
      add_adr(a_code = ex_$a_llt, adr_data = adr_)
  }))

  res <-
    demo %>%
    compute_dispro(
      y = "a_colitis",
      x = "nivolumab",
      export_raw_values = TRUE
    )

  res_a <-
    demo |>
    arrow::as_arrow_table() |>
    compute_dispro(
      y = "a_colitis",
      x = "nivolumab",
      export_raw_values = TRUE
    )

  exp_res <- rlang::list2(
    or = cff(1.88, dig = 2),
    ic = cff(0.49, dig = 2)
  )

  expect_equal(
    res[["or"]],
    exp_res[["or"]]
  )

  expect_equal(
    cff(res[["ic"]], dig = 2),
    exp_res[["ic"]]
  )

  expect_equal(res, res_a)
})

test_that("handles 0 cases in y/x combination", {
  demo <-
    data.table::data.table(
      a_colitis = c(1, 1, 0, 0, 0),
      nivolumab = c(1, 1, 0, 1, 0)
    )

  res_misb <-
    demo %>%
    compute_dispro(
      y = "a_colitis",
      x = "nivolumab",
      export_raw_values = TRUE
    )

  res_misb_a <-
    demo |>
    arrow::as_arrow_table() |>
    compute_dispro(
      y = "a_colitis",
      x = "nivolumab",
      export_raw_values = TRUE
    )

  expect_equal(
    res_misb[["b"]],
    0
  )

  expect_equal(res_misb, res_misb_a)

  demo_misc <-
    data.table::data.table(
      a_colitis = c(1, 1, 0, 0, 0),
      nivolumab = c(1, 0, 0, 0, 0)
    )

  res_misc <-
    demo_misc %>%
    compute_dispro(
      y = "a_colitis",
      x = "nivolumab",
      export_raw_values = TRUE
    )

  res_misc_a <-
    demo_misc |>
    arrow::as_arrow_table() |>
    compute_dispro(
      y = "a_colitis",
      x = "nivolumab",
      export_raw_values = TRUE
    )


  expect_equal(
    res_misc[["c"]],
    0
  )

  expect_equal(res_misc, res_misc_a)

  demo_misa <-
    data.table::data.table(
      a_colitis = c(1, 1, 0, 0, 0),
      nivolumab = c(0, 0, 1, 0, 0)
    )

  res_misa <-
    demo_misa %>%
    compute_dispro(
      y = "a_colitis",
      x = "nivolumab",
      export_raw_values = TRUE
    )

  res_misa_a <-
    demo_misa |>
    arrow::as_arrow_table() |>
    compute_dispro(
      y = "a_colitis",
      x = "nivolumab",
      export_raw_values = TRUE
    )


  expect_equal(
    res_misa[["a"]],
    0
  )

  expect_equal(res_misa, res_misa_a)


  demo_misd <-
    data.table::data.table(
      a_colitis = c(1, 1, 1, 1, 1),
      nivolumab = c(0, 1, 0, 1, 1)
    )

  res_misd <-
    demo_misd %>%
    compute_dispro(
      y = "a_colitis",
      x = "nivolumab",
      export_raw_values = TRUE
    )

  res_misd_a <-
    demo_misd |>
    arrow::as_arrow_table() |>
    compute_dispro(
      y = "a_colitis",
      x = "nivolumab",
      export_raw_values = TRUE
    )


  expect_equal(
    res_misd[["d"]],
    0
  )

  expect_equal(res_misd, res_misd_a)

})

test_that("vectorization works inside and outside the function", {
  expect_snapshot({
    demo <-
      demo_  |>
      add_drug(d_code = ex_$d_drecno, drug_data = drug_)  |>
      add_adr(a_code = ex_$a_llt, adr_data = adr_)
  })

  many_drugs <-
    c("nivolumab",
      "pembrolizumab"
      )

  many_adrs <-
    c("a_colitis",
      "a_pneumonitis"
      )

  true_or <-
    c("1.88",
      "1.75",
      "0.94",
      "1.82")

  vect_outside <-
    many_drugs  |>
   purrr::map(
     function(a_drug) {
       many_adrs |>
         purrr::map(
           function(an_adr)
             demo |>
             compute_dispro(
               y = an_adr,
               x = a_drug,
               export_raw_values = TRUE
               )
           ) |>
         purrr::list_rbind()
     }
   )  |>
   purrr::list_rbind()

  vect_outside_a <-
    many_drugs  |>
    purrr::map(
      function(a_drug) {
        many_adrs |>
          purrr::map(
            function(an_adr)
              demo  |>
              arrow::as_arrow_table() |>
              compute_dispro(
                y = an_adr,
                x = a_drug,
                export_raw_values = TRUE
              )
          ) |>
          purrr::list_rbind()
      }
    )  |>
    purrr::list_rbind()

  vect_inside <-
    demo |>
    compute_dispro(
      y = many_adrs,
      x = many_drugs,
      export_raw_values = TRUE
      )

  vect_inside_a <-
    demo |>
    arrow::as_arrow_table() |>
    compute_dispro(
      y = many_adrs,
      x = many_drugs,
      export_raw_values = TRUE
    )

  expect_equal(vect_outside, vect_inside)

  expect_equal(vect_outside, vect_outside_a)

  expect_equal(vect_inside,  vect_inside_a)

  expect_equal(vect_inside$or,
               true_or)


})

test_that("works with large numbers", {

  demo <-
    data.table::data.table(
      event1 =
        c(rep(1, 10000000),
          rep(0, 20000000)
          ),

      drug1  =
        c(rep(1, 02000000),
          rep(0, 08000000),
          rep(1, 16000000),
          rep(0, 04000000)
        )
    )

  r1 <-
    demo |>
    compute_dispro(
      y = "event1",
      x = "drug1",
      export_raw_values = TRUE
    )

  z_val <- qnorm(1 - 0.05 / 2)

  r1_true <-
    dplyr::tibble(
      y = "event1",
      x = "drug1",
      n_obs = as.numeric(2000000),
      n_exp = 6000000,
      or    = "0.06",
      or_ci  = "(0.06-0.06)",
      ci_level = "95%",

      a = as.numeric(2000000),
      b = as.numeric(8000000),
      c = as.numeric(16000000),
      d = as.numeric(4000000),
      std_er = sqrt((1 / .data$a) +
                      (1 / .data$b) +
                      (1 / .data$c) +
                      (1 / .data$d)
      ),
      ic = log((.data$a + .5) / (.data$n_exp + .5), base = 2),
      ic_tail = ic_tail(
        n_obs = .data$a,
        n_exp = .data$n_exp,
        p = 0.05 / 2
      ),
      or_raw     =  .data$a * .data$d / (.data$b * .data$c),
      low_ci  = .data$or_raw * exp(- .env$z_val * .data$std_er),
      up_ci  = .data$or_raw * exp(+ .env$z_val * .data$std_er),
      signif_or = 0,
      signif_ic = 0
    ) |>
    dplyr::select(dplyr::all_of(
      c("y", "x", "n_obs", "n_exp", "or", "or_ci", "ic", "ic_tail", "ci_level",
        "a", "b", "c", "d", "std_er", "or_raw", "low_ci", "up_ci",
        "signif_or", "signif_ic")))


  expect_equal(
    r1,
    r1_true
  )

  expect_equal(
    round(r1$ic, 2),
    -1.58
  )

})

test_that("short nice format and min_n_obs format works", {
  expect_message(expect_message({
    demo <-
      demo_ %>%
      add_drug(d_code = ex_$d_drecno, drug_data = drug_) %>%
      add_adr(a_code = ex_$a_llt, adr_data = adr_)
  }))

  res <-
    demo %>%
    compute_dispro(
      y = "a_colitis",
      x = "nivolumab",
      export_raw_values = FALSE
    ) |>
    dplyr::mutate(
      ic = cff(ic, dig = 3),
      ic_tail = cff(ic_tail, dig = 4)
    )

  res_true <-
    dplyr::tibble(
      y = "a_colitis",
      x = "nivolumab",
      n_obs = 44,
      n_exp = 31.2,
      or = "1.88",
      or_ci = "(1.23-2.88)",
      ic = "0.489",
      ic_tail = "0.0314",
      ci_level = "95%"
    )

  expect_equal(res, res_true)

  res <-
    demo %>%
    compute_dispro(
      y = c("a_colitis", "a_pneumonitis"),
      x = "nivolumab",
      na_format = "wow",
      min_n_obs = 43,
      export_raw_values = TRUE
    )

  expect_equal(
    res |> dplyr::filter(y == "a_pneumonitis") |>
      dplyr::pull(or),
    NA_character_
  )
})
