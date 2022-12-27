test_that("returns proper counts", {

  a_names <- paste0("adr_", names(ex_$adr_list))

  demo <-
   demo_ %>%
     add_adr(
       adr_list = ex_$adr_list,
       a_names = a_names,
       adr = adr_
     )

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
