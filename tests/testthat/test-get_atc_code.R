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

test_that("codes of different length work", {
  atc_sel <-
    rlang::list2(l03_j01 = c("J01", "L03AA")
    )

  atc_sel2 <-
    list(l03_j01 = c("J01A", "L03"))

  atc_sel3 <-
    list(l03_j01 = c("J01AA", "L03"))

  thg_test <-
    thg_ |>
    dplyr::add_row(
      Therapgroup_Id = 10,
      ATC.code = "J01A",
      Create.date = "",
      Official.ATC.code = "Y",
      MedicinalProd_Id = 13
    ) |>
    dplyr::add_row(
      Therapgroup_Id = 11,
      ATC.code = "L03AB",
      Create.date = "",
      Official.ATC.code = "Y",
      MedicinalProd_Id = 15
    )

  mp_test <- # we already have L03AA and J01CA
    mp_ |>
    dplyr::add_row(
      MedicinalProd_Id = 13,
      Sequence.number.1 = "01",
      Sequence.number.2 = "001",
      DrecNo = 130,
      drug_name_t = "test_d1",
      Create.date = "",
      Date.changed = "",
      Country = ""
    ) |>
    dplyr::add_row(
      MedicinalProd_Id = 15,
      Sequence.number.1 = "01",
      Sequence.number.2 = "001",
      DrecNo = 150,
      drug_name_t = "test_d2",
      Create.date = "",
      Date.changed = "",
      Country = ""
    )

  atc_res <-
    suppressMessages(
      get_atc_code(atc_sel, mp_test, thg_test)
    )

  # should find Drecno 130 from J01A, not Drecno 150 from L03AB

  expect_equal(
    !150 %in% atc_res$l03_j01,
    TRUE
  )

  expect_in(
    130,
    atc_res$l03_j01
  )

  atc_res2 <-
    suppressMessages(
      get_atc_code(atc_sel2, mp_test, thg_test)
    )

  # expecting both 130 from J01A and 150 from L03AB
  expect_in(
    c(130, 150),
    atc_res2$l03_j01
  )

  atc_res3 <-
    suppressMessages(
      get_atc_code(atc_sel3, mp_test, thg_test)
    )

  # expecting 150 only
  expect_in(
    150,
    atc_res3$l03_j01
  )

  expect_equal(
    !130 %in% atc_res3$l03_j01,
    TRUE
  )

})
