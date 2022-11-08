context("adr_merge")

test_that("prioritization, highest N line(s) selected when different N exists for one study arm", {

  nct1_small_n <- 20
  nct1_big_n <- 20000

  nct1_r1_ne <- 1
  nct1_r2_ne <- 2

  df <-
    data.frame(
      study_id = c("NCT1", "NCT1"),
      reaction = c("hypertension", "hypertensive crisis"),
      event_arm1 = c(nct1_r1_ne, nct1_r2_ne),
      n_arm1 = c(nct1_small_n, nct1_big_n)
    )

  group_term_list <-
    list(hta = c("hypertension", "hypertensive crisis")
         )

  res <-
    adr_merge(group_term = "hta",
              group_term_list,
              event = "event_arm1",
              n = "n_arm1",
              reaction = "reaction",
              study_id = "study_id",
              data = df,
              intermediate = FALSE,
              method = "integrative"
    )

  expect_equal(res[["n_arm1"]],
               nct1_big_n)
  expect_equal(res[["event_arm1"]],
               nct1_r2_ne)

})

test_that("sum is applied when integrative method is selected", {

  nct1_small_n <- 20
  nct1_big_n <- 20000

  nct1_r1_ne <- 1
  nct1_r2_ne <- 2

  df <-
    data.frame(
      study_id = c("NCT1", "NCT1"),
      reaction = c("hypertension", "hypertensive crisis"),
      event_arm1 = c(nct1_r1_ne, nct1_r2_ne),
      n_arm1 = c(nct1_small_n, nct1_small_n)
    )

  group_term_list <-
    list(hta = c("hypertension", "hypertensive crisis")
         )

  res <-
    adr_merge(group_term = "hta",
              group_term_list,
              event = "event_arm1",
              n = "n_arm1",
              reaction = "reaction",
              study_id = "study_id",
              data = df,
              intermediate = FALSE,
              method = "integrative"
    )

  expect_equal(res[["n_arm1"]],
               nct1_small_n)
  expect_equal(res[["event_arm1"]],
               nct1_r1_ne + nct1_r2_ne)

})

test_that("max is applied when conservative method is selected", {

  nct1_small_n <- 20
  nct1_big_n <- 20000

  nct1_r1_ne <- 1
  nct1_r2_ne <- 2

  df <-
    data.frame(
      study_id = c("NCT1", "NCT1"),
      reaction = c("hypertension", "hypertensive crisis"),
      event_arm1 = c(nct1_r1_ne, nct1_r2_ne),
      n_arm1 = c(nct1_small_n, nct1_small_n)
    )

  group_term_list <-
    list(hta = c("hypertension", "hypertensive crisis"),
         dermato = c("eczema", "pruritus"))

  res <-
    adr_merge(group_term = "hta",
              group_term_list,
              event = "event_arm1",
              n = "n_arm1",
              reaction = "reaction",
              study_id = "study_id",
              data = df,
              intermediate = FALSE,
              method = "conservative"
    )

  expect_equal(res[["n_arm1"]],
               nct1_small_n)
  expect_equal(res[["event_arm1"]],
               max(nct1_r1_ne, nct1_r2_ne))

})

