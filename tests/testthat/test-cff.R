test_that("formatting is as expected", {
  num <- c(0.1, 0.02, 1.658)

  expect_equal(
    cff(num),
    c("0", "0", "2")
  )

  expect_equal(
    cff(num, dig = 2),
    c("0.10", "0.02", "1.66")
  )

  expect_equal(
    cff(num = num[[1]],
        low_ci = num[[2]],
        up_ci = num[[3]],
        method = "num_ci",
        dig = 2),
    "0.10 (0.02-1.66)"
  )
})


test_that("big and small number don't turn into scientific notation", {
  num <- c(0.0000001, 1000000000000)

  options(scipen = 1)

  expect_equal(
    cff(num),
    c("0", "1,000,000,000,000")
  )

  expect_equal(
    cff(num, dig = 7),
    c("0.0000001", "1,000,000,000,000.0000000")
  )

  expect_equal(
    cff(
        low_ci = num[[1]],
        up_ci = num[[2]],
        method = "ci",
        dig = 2),
    "(0.00-1,000,000,000,000.00)"
  )
})

