test_that("there is no duplicate in extracted llts", {
  # Intended for the rechallenge data management
  llt_extraction_soc <-
    get_llt_soc(
      rlang::list2(colitis = "Colitis"),
      term_level = "pt",
      meddra = ex_$meddra)

  expect_equal(purrr::map(llt_extraction_soc, length),
               purrr::map(llt_extraction_soc, ~ length(unique(.x)))
               )
  })

test_that("throws warning when nothing is found", {
  expect_warning(
    get_llt_soc(
      rlang::list2(rate = "youps"),
      term_level = "pt",
      meddra = ex_$meddra
    ),
    "the following terms were not found at pt level: youps"
  )
})

test_that("warning even if one term good and one term bad", {
  expect_warning(
    get_llt_soc(
      rlang::list2(rate = c("Hepatitis", "youps")),
      term_level = "pt",
      meddra = ex_$meddra
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
      meddra = ex_$meddra
    )

  purrr::iwalk(
    adr_llt,
    function(p, p_n)
      expect_equal(length(p), pt_res_length[[p_n]])
  )
})

