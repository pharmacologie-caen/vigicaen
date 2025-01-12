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
  atc_drecno <-
    get_atc_code(atc_sel = atc_sel,
                 mp = mp_,
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
                   mp = mp_,
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

test_that("names are tolower-ed and trimed", {
  atc_sel <-
    rlang::list2(L03_J01 = c("L03AA", "J01CA"),
                 C09aA = c("C09AA")
    )

  tolower_names <-
    names(atc_sel) |>
    stringr::str_to_lower() |>
    stringr::str_trim()

  # with vigilyze to TRUE

  expect_warning({
    expect_message({
    atc_drecno <<-
      get_atc_code(atc_sel = atc_sel,
                   mp = mp_,
                   thg_data = thg_,
                   vigilyze = TRUE)
  },
  "DrecNo"
  )
  }, "tolower-ed"
  )

  expect_equal(
    names(atc_drecno),
    tolower_names
  )

  # with vigilyze to FALSE

  expect_warning({
    expect_message({
      atc_drecno_vigifalse <<-
        get_atc_code(
          atc_sel = atc_sel,
          mp = mp_,
          thg_data = thg_,
          vigilyze = FALSE
        )
    },
    "Medicinal")
  },
  "tolower-ed")

  expect_equal(
    names(atc_drecno_vigifalse),
    tolower_names
  )

})

test_that("works with thg or mp as Table (out of memory)", {
  atc_sel <-
    rlang::list2(l03_j01 = c("L03AA", "J01CA"),
                 c09aa = c("C09AA")
    )

  atc_sel_drecno_counts <-
    rlang::list2(l03_j01 = 77,
                 c09aa = 26
    )

  expect_message({
    atc_drecno <-
      get_atc_code(atc_sel = atc_sel,
                   mp = mp_ |>
                     arrow::as_arrow_table(),
                   thg_data = thg_ |>
                     arrow::as_arrow_table(),
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
