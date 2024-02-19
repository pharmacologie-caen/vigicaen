test_that("counts are good", {

  s1 <-
    screen_adr(
    adr_data = adr_,
    meddra = meddra_
  )

  s1_true <-
    data.frame(
      pt_name = c(NA_character_, "Pneumonitis", "Diarrhoea", "Colitis"),
      n_cas = c(313, 92, 67, 32)
    )

  expect_equal(s1, s1_true)

})
