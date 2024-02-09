test_that("Works for single and multiple ATC selections", {

  atc_sel <-
    rlang::list2(l03_j01 = c("L03AA", "J01CA"),
                 c09aa = c("C09AA")
    )

  atc_sel_drecno_counts <-
    rlang::list2(l03_j01 = 77,
                 c09aa = 26
    )

  expect_message({
  atc_drecno <<-
    get_atc_code(atc_sel = atc_sel,
                 mp_short = mp_short_,
                 thg_data = thg_,
                 vigilyze = TRUE)
  },
  "DrecNo"
  )

  purrr::iwalk(
    atc_sel_drecno_counts,
    function(a, a_n)
      expect_equal(
        length(atc_drecno[[a_n]]),
        a
        )
  )



  })

test_that("extracts mpi if vigilyze is FALSE", {

  atc_sel <-
    rlang::list2(l03_j01 = c("L03AA", "J01CA"),
                 c09aa = c("C09AA")
    )

  atc_sel_mpi_counts <-
    rlang::list2(l03_j01 = 1721,
                 c09aa = 2185
    )

  expect_message({
    atc_drecno <-
      get_atc_code(atc_sel = atc_sel,
                   mp_short = mp_short_,
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
  },
  "MedicinalProd"
  )

})
