test_that("counts are good", {

  s1 <-
    screen_adr(
    adr_data = adr_,
    meddra = meddra_,
    term_level = "pt"
  )

  s1_true <-
    data.frame(
      pt_name = c(NA_character_, "Pneumonitis", "Diarrhoea", "Colitis"),
      n_cas = c(697, 88, 66, 31)
    )

  expect_equal(s1, s1_true)

  s2 <-
    screen_adr(
      adr_data = adr_,
      meddra = meddra_,
      term_level = "hlt"
    )

  s2_true <-
    data.frame(
      hlt_name = c(NA_character_,
                  "Lower respiratory tract inflammatory and immunologic conditions", "Diarrhoea (excl infective)", "Colitis (excl infective)"),
      n_cas = c(697, 88, 66, 31)
    )

  expect_equal(s2, s2_true)

  s3 <-
    screen_adr(
      adr_data = adr_,
      meddra = meddra_,
      term_level = "soc"
    )

  s3_true <-
    data.frame(
      soc_name = c(NA_character_,
                   "Gastrointestinal disorders",
                   "Respiratory, thoracic and mediastinal disorders"),
      n_cas = c(697, 88, 88)
    )

  expect_equal(s3, s3_true)

  s4 <-
    screen_adr(
      adr_data = adr_,
      meddra = meddra_,
      term_level = "hlgt"
    )

  s4_true <-
    data.frame(
      hlgt_name = c(NA_character_,
                   "Lower respiratory tract disorders (excl obstruction and infection)",
                   "Gastrointestinal motility and defaecation conditions",
                   "Gastrointestinal inflammatory conditions"),
      n_cas = c(697, 88, 66, 31)
    )

  expect_equal(s4, s4_true)

})
