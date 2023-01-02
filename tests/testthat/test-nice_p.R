test_that("accurate rounding", {

  expect_equal(
    nice_p(0.056548),
    ".06"
  )

  expect_equal(
    nice_p(0.0002654),
    "<.001"
  )

  expect_equal(
    nice_p(0.816546),
    ".82"
  )

  expect_equal(
    nice_p(0.0493321),
    ".049"
  )
})

test_that("arg print_zero works", {

  expect_equal(
    nice_p(0.056548),
    ".06"
  )

  expect_equal(
    nice_p(0.056548, print_zero = TRUE),
    "0.06"
  )

})
