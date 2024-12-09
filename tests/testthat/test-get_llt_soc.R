test_that("basic use works", {
  llt_extraction_soc <-
    get_llt_soc(
      rlang::list2(pneumonitis = "Pneumonitis"),
      term_level = "pt",
      meddra = meddra_
    )

  llt_extraction_true <-
    list(pneumonitis =
           c(77064677,  56481317, 136339570,  56401565, 111041017, 116513289)
    )

  expect_equal(llt_extraction_soc, llt_extraction_true)

})

test_that("there is no duplicate in extracted llts", {
  # Intended for the rechallenge data management
  llt_extraction_soc <-
    get_llt_soc(
      rlang::list2(colitis = "Colitis"),
      term_level = "pt",
      meddra = meddra_)

  expect_equal(purrr::map(llt_extraction_soc, length),
               purrr::map(llt_extraction_soc, ~ length(unique(.x)))
               )
  })

test_that("throws warning when nothing is found", {
  expect_warning(
    get_llt_soc(
      rlang::list2(rate = "youps"),
      term_level = "pt",
      meddra = meddra_
    ),
    "the following terms were not found at pt level: youps"
  )
})

test_that("warning even if one term good and one term bad", {
  expect_warning(
    get_llt_soc(
      rlang::list2(rate = c("Hepatitis", "youps")),
      term_level = "pt",
      meddra = meddra_
    ),
    "the following terms were not found at pt level: youps"
  )
})

test_that("works with unique and multiple terms", {
  pt_sel <- rlang::list2(
    colitis = c("Colitis"),
    colitis_enteritis = c("Colitis", "Enteritis")
  )

  pt_res_length <- rlang::list2(
    colitis = 25,
    colitis_enteritis = 33
  )

  adr_llt <-
    get_llt_soc(
      pt_sel,
      term_level = "pt",
      meddra = meddra_
    )

  purrr::iwalk(
    adr_llt,
    function(p, p_n)
      expect_equal(length(p), pt_res_length[[p_n]])
  )
})

test_that("works at different term levels (soc, hlgt, hlt, pt, llt)", {

  term_level_set <-
    rlang::list2(
      soc  = rlang::list2(gi = "Gastrointestinal disorders"),
      hlgt = rlang::list2(gi = "Gastrointestinal inflammatory conditions"),
      hlt  = rlang::list2(gi = "Colitis (excl infective)"),
      pt   = rlang::list2(gi = "Colitis"),
      llt  = rlang::list2(gi = "Caecal inflammation")
    )

  tl_res <- purrr::imap(
    term_level_set,
    function(term_list, t_level)
      get_llt_soc(
        term_list,
        term_level = t_level,
        meddra = meddra_
      )
  )

  tl_res_length <-
    purrr::map(
      tl_res,
      ~ length(.x[["gi"]])
    )


  tl_res_correct_length <- rlang::list2(
    soc  = 134,
    hlgt = 56,
    hlt  = 39,
    pt   = 25,
    llt  = 1
  )

    expect_equal(tl_res_length, tl_res_correct_length)
})

test_that("works with meddra as Table (out of memory)", {
  llt_extraction_soc <-
    get_llt_soc(
      rlang::list2(pneumonitis = "Pneumonitis"),
      term_level = "pt",
      meddra = meddra_ |>
        arrow::as_arrow_table()
    )

  llt_extraction_true <-
    list(pneumonitis =
           c(77064677,  56481317, 136339570,  56401565, 111041017, 116513289)
    )

  expect_equal(llt_extraction_soc, llt_extraction_true)

})
