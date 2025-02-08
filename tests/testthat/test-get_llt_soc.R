test_that("basic use works", {
  llt_extraction_soc <-
    get_llt_soc(
      rlang::list2(pneumonitis = "Pneumonitis"),
      term_level = "pt",
      meddra = meddra_,
      verbose = FALSE
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
      meddra = meddra_,
      verbose = FALSE)

  expect_equal(purrr::map(llt_extraction_soc, length),
               purrr::map(llt_extraction_soc, ~ length(unique(.x)))
               )
  })

test_that("verbose works", {
  # Intended for the rechallenge data management
  good_list <-
    rlang::list2(item1 = c("Colitis"),
                 item2 = c("Organising pneumonia",
                           "Pneumonitis")
    )
  expect_snapshot(
    r1 <-
    get_llt_soc(
      good_list,
      term_level = "pt",
      meddra = meddra_)
  )
})

test_that("unmatched terms management", {
  wrong_list <-
    rlang::list2(rate = c("youps", "Youps", "Colitis"),
                 encorerate = c("another", "yet another")
    )

  wrong_with_capital <-
    rlang::list2(rate = c("Youps", "Youps", "Colitis")
    )

  wrong_without_capital <-
    rlang::list2(rate = c("youps"))

  expect_snapshot(
    r1 <- get_llt_soc(
      wrong_without_capital,
      term_level = "pt",
      meddra = meddra_,
      verbose = FALSE
    )
  )

  expect_snapshot(
    r1 <- get_llt_soc(
      wrong_with_capital,
      term_level = "pt",
      meddra = meddra_,
      verbose = FALSE
    )
  )

  # even if one term good and one term bad
  expect_snapshot(
    r1 <- get_llt_soc(
      wrong_list,
      term_level = "pt",
      meddra = meddra_,
      verbose = FALSE
    )
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
      meddra = meddra_,
      verbose = FALSE
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
        meddra = meddra_,
        verbose = FALSE
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

test_that("works with meddra as Table (out of memory) or not data.table", {
  llt_extraction_soc <-
    get_llt_soc(
      rlang::list2(pneumonitis = "Pneumonitis"),
      term_level = "pt",
      meddra = meddra_ |>
        arrow::as_arrow_table(),
      verbose = FALSE
    )

  llt_extraction_soc2 <-
    get_llt_soc(
      rlang::list2(pneumonitis = "Pneumonitis"),
      term_level = "pt",
      meddra = meddra_ |>
        as.data.frame(),
      verbose = FALSE
    )

  llt_extraction_true <-
    list(pneumonitis =
           c(77064677,  56481317, 136339570,  56401565, 111041017, 116513289)
    )

  expect_equal(llt_extraction_soc, llt_extraction_true)

  expect_equal(llt_extraction_soc2, llt_extraction_true)

})
