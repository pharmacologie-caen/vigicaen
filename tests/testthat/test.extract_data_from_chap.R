context("extract_data_from_chap")

test_that("you can extract main and subgroup results from metarate", {

  data <-
    data.frame(
      event = 4:1,
      time = c(10, 20, 30, 40),
      n = c(5, 7, 8, 9),
      byvar = c("a", "a", "b", "b")
    )

  m1 <- metarate(
    event = event,
    time = time,
    # n = n,
    byvar = byvar,
    data = data
  )

  res_list <-
    extract_data_from_chap(
      chapelet_mod = list(mod = m1),
      main_or_sbgp = "main",
      common_or_random = "random",
      prop_scaler = 100
    )

  res_unique <-
    extract_data_from_chap(
      m1,
      main_or_sbgp = "main",
      common_or_random = "random",
      prop_scaler = 100
    )

  res_sbgp <-
    extract_data_from_chap(
      chapelet_mod = list(mod = m1),
      main_or_sbgp = "sbgp",
      common_or_random = "random",
      prop_scaler = 100
    )

  res_sbgp_unique <-
    extract_data_from_chap(
      chapelet_mod = m1,
      main_or_sbgp = "sbgp",
      common_or_random = "random",
      prop_scaler = 100
    )

  res_list_common <-
    extract_data_from_chap(
      chapelet_mod = list(mod = m1),
      main_or_sbgp = "main",
      common_or_random = "common",
      prop_scaler = 100
    )

  expect_equal(res_list,
               res_unique)
  expect_equal(res_sbgp,
               res_sbgp_unique)

  expect_equal(res_list[["event"]], sum(data[["event"]]))

  expect_equal(nrow(res_list), 1)
  expect_equal(nrow(res_sbgp), 2)

  expect_equal(nrow(res_list_common), 1)

  expect_false(res_list$`Annualized Rate (95%CI)` ==
                 res_list_common$`Annualized Rate (95%CI)`)

})

test_that("you can extract main and subgroup results from metabin", {

  data <-
    data.frame(
      event.e = 4:1,
      event.c = 2:5,

      n.e = c(5, 7, 8, 9),
      n.c = c(9, 10, 11, 12),
      byvar = c("a", "a", "b", "b")
    )

  m1 <- metabin(
    event.e = event.e,
    event.c = event.c,
    n.e = n.e,
    n.c = n.c,
    byvar = byvar,
    data = data
  )

  res_list <-
    extract_data_from_chap(
      chapelet_mod = list(mod = m1),
      main_or_sbgp = "main",
      common_or_random = "random",
      prop_scaler = 100
    )

  res_unique <-
    extract_data_from_chap(
      m1,
      main_or_sbgp = "main",
      common_or_random = "random",
      prop_scaler = 100
    )

  res_sbgp <-
    extract_data_from_chap(
      chapelet_mod = list(mod = m1),
      main_or_sbgp = "sbgp",
      common_or_random = "random",
      prop_scaler = 100
    )

  res_sbgp_unique <-
    extract_data_from_chap(
      chapelet_mod = m1,
      main_or_sbgp = "sbgp",
      common_or_random = "random",
      prop_scaler = 100
    )

  res_list_common <-
    extract_data_from_chap(
      chapelet_mod = list(mod = m1),
      main_or_sbgp = "main",
      common_or_random = "common",
      prop_scaler = 100
    )

  expect_equal(res_list,
               res_unique)
  expect_equal(res_sbgp,
               res_sbgp_unique)

  expect_equal(res_list[["event.e"]], sum(data[["event.e"]]))

  expect_equal(nrow(res_list), 1)
  expect_equal(nrow(res_sbgp), 2)

  expect_equal(nrow(res_list_common), 1)

  expect_false(res_list$`sm (95%CI)` ==
                 res_list_common$`sm (95%CI)`)

})

test_that("you can extract main and subgroup results from metaprop", {

  data <-
    data.frame(
      event = 4:1,
      n = c(5, 7, 8, 9),
      byvar = c("a", "a", "b", "b")
    )

  m1 <- metaprop(
    event = event,
    n = n,
    byvar = byvar,
    data = data
  )

  res_list <-
    extract_data_from_chap(
      chapelet_mod = list(mod = m1),
      main_or_sbgp = "main",
      common_or_random = "random",
      prop_scaler = 100
    )

  res_unique <-
    extract_data_from_chap(
      m1,
      main_or_sbgp = "main",
      common_or_random = "random",
      prop_scaler = 100
    )

  res_sbgp <-
    extract_data_from_chap(
      chapelet_mod = list(mod = m1),
      main_or_sbgp = "sbgp",
      common_or_random = "random",
      prop_scaler = 100
    )

  res_sbgp_unique <-
    extract_data_from_chap(
      chapelet_mod = m1,
      main_or_sbgp = "sbgp",
      common_or_random = "random",
      prop_scaler = 100
    )

  res_list_common <-
    extract_data_from_chap(
      chapelet_mod = list(mod = m1),
      main_or_sbgp = "main",
      common_or_random = "common",
      prop_scaler = 100
    )

  expect_equal(res_list,
               res_unique)
  expect_equal(res_sbgp,
               res_sbgp_unique)

  expect_equal(res_list[["event"]], sum(data[["event"]]))

  expect_equal(nrow(res_list), 1)
  expect_equal(nrow(res_sbgp), 2)

  expect_equal(nrow(res_list_common), 1)

  expect_false(res_list$`Proportion (95%CI)` ==
                 res_list_common$`Proportion (95%CI)`)

})

test_that("A NULL element in the model list doesnt crash the function", {

  data <-
    data.frame(
      event = 4:1,
      n = c(5, 7, 8, 9),
      byvar = c("a", "a", "b", "b")
    )

  m1 <- metaprop(
    event = event,
    n = n,
    byvar = byvar,
    data = data
  )

  res_mod <-
    extract_data_from_chap(
      list(mod1 = m1)
    )

  mod_list <- list(mod1 = m1, modnull = NULL)

  mod_list2 <- list(modnull = NULL, mod1 = m1) # even if its the first mod?

  res_mod2 <- extract_data_from_chap(
    mod_list
  )

  res_mod3 <- extract_data_from_chap(
    mod_list2
  )

  expect_equal(
    res_mod,
    res_mod2
  )
  expect_equal(
    res_mod,
    res_mod3
  )

})
