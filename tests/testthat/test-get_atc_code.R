test_that("Works for single and multiple ATC selections", {

  atc_sel <-
    rlang::list2(l03_l04 = c("L03", "L04"),
                 c01 = c("C01")
    )

  atc_sel_drecno_counts <-
    rlang::list2(l03_l04 = 301,
                 c01 = 547
    )

  atc_drecno <-
    get_atc_code(atc_sel = atc_sel,
                 mp_short = ex_$mp_short,
                 thg_data = thg_,
                 vigilyze = TRUE)

  purrr::iwalk(
    atc_sel_drecno_counts,
    function(a, a_n)
      expect_equal(
        length(atc_drecno[[a_n]]),
        a
        )
  )

  expect_message(
    get_atc_code(atc_sel = atc_sel,
                 mp_short = ex_$mp_short,
                 thg_data = thg_,
                 vigilyze = TRUE),
    "DrecNo"
  )

  })

test_that("extracts mpi if vigilyze is FALSE", {

  atc_sel <-
    rlang::list2(l03_l04 = c("L03", "L04"),
                 c01 = c("C01")
    )

  atc_sel_mpi_counts <-
    rlang::list2(l03_l04 = 4436,
                 c01 = 5685
    )

  atc_drecno <-
    get_atc_code(atc_sel = atc_sel,
                 mp_short = ex_$mp_short,
                 thg_data = thg_,
                 vigilyze = FALSE)

  purrr::iwalk(
    atc_sel_mpi_counts,
    function(a, a_n)
      expect_equal(
        length(atc_drecno[[a_n]]),
        a
      )
  )

  expect_message(
    get_atc_code(atc_sel = atc_sel,
                 mp_short = ex_$mp_short,
                 thg_data = thg_,
                 vigilyze = FALSE),
    "MedicinalProd"
  )

})
