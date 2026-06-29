test_that("parse_repbasis() maps valid strings to Basis codes", {
  expect_equal(parse_repbasis("s"), 1)
  expect_equal(parse_repbasis("c"), 2)
  expect_equal(parse_repbasis("i"), 3)
  expect_equal(parse_repbasis("sci"), c(1, 2, 3))
  expect_equal(parse_repbasis("sc"), c(1, 2))
  expect_equal(parse_repbasis("ci"), c(2, 3))
  # order follows s/c/i, not the order of the input characters
  expect_equal(parse_repbasis("is"), c(1, 3))
})

test_that("parse_repbasis() rejects invalid input with a clear error", {
  expect_error(parse_repbasis(""), class = "invalid_repbasis")
  expect_error(parse_repbasis("xyz"), class = "invalid_repbasis")
  expect_error(parse_repbasis("scix"), class = "invalid_repbasis")
  expect_error(parse_repbasis("S"), class = "invalid_repbasis")
  expect_error(parse_repbasis(c("s", "c")), class = "invalid_repbasis")
  expect_error(parse_repbasis(NA_character_), class = "invalid_repbasis")
  expect_error(parse_repbasis(1), class = "invalid_repbasis")
})

test_that("parse_repbasis() matches the previous inline behaviour for valid input", {
  inline <- function(repbasis) {
    c(
      if (grepl("s", repbasis)) 1,
      if (grepl("c", repbasis)) 2,
      if (grepl("i", repbasis)) 3
    )
  }
  for (rb in c("s", "c", "i", "sc", "si", "ci", "sci")) {
    expect_equal(parse_repbasis(rb), inline(rb))
  }
})
