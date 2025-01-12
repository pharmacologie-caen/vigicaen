test_that("returns proper counts", {

  a_names <- paste0("adr_", names(ex_$a_llt))

  expect_snapshot({
    demo <-
      demo_ %>%
      add_adr(a_code = ex_$a_llt,
              a_names = a_names,
              adr = adr_)
  })

  res <- check_dm(demo, a_names)

  # proper columns

  expect_equal(
    row.names(res),
    a_names
  )

  # proper counts

  r2 <- purrr::map_dbl(
    a_names, function(a_n)
      sum(demo[[a_n]])) %>%
    rlang::set_names(a_names)

  expect_equal(res[, 1],
               r2)

})
