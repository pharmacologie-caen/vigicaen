test_that("pads appropriately", {
  expect_invisible(
    texter("I am doing this step", "3%%")
  )
})
