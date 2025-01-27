test_that("find proper ttos on a known dataset", {
  expect_snapshot({
    link_ <-
      link_ |>
      add_drug(d_code = ex_$d_groups_drecno, drug_data = drug_) |>
      add_adr(a_code = ex_$a_llt, adr_data = adr_)
  })

  tto_test <-
    extract_tto(.data = link_,
                adr_s = "a_colitis",
                drug_s = "pd1")

  true_n_tto_avail <- 39 # we're not collecting negative ttos

  expect_equal(
    nrow(tto_test),
    true_n_tto_avail
  )

  true_tto_rch <-
    c(205, 175, 36, 740, 379)

  expect_equal(
    head(tto_test$tto_max, 5),
    true_tto_rch
  )
})

test_that("works with vectorized adrs and drugs", {
  expect_snapshot({
    link_ <-
      link_ |>
      add_drug(d_code = ex_$d_groups_drecno, drug_data = drug_) |>
      add_adr(a_code = ex_$a_llt, adr_data = adr_)
  })

  tto_many_adr <-
    extract_tto(.data = link_,
             adr_s = c("a_colitis", "a_pneumonitis"),
             drug_s = c("pd1"))

  tto_many_adr2 <-
    dplyr::bind_rows(
      tto_many_adr |>
        dplyr::filter(adr_s == "a_colitis") |>
        dplyr::slice_head(n = 3),
      tto_many_adr |>
        dplyr::filter(adr_s == "a_pneumonitis") |>
        dplyr::slice_head(n = 3)
    )

  correct_many_adr <-
    data.frame(
      tto_max = c(205, 175, 36, 602, 43, 105),
      adr_s  = c(rep("a_colitis", 3), rep("a_pneumonitis", 3))
    )

  expect_equal(
    tto_many_adr2 |>
      dplyr::select(tto_max, adr_s),
    correct_many_adr
  )

  tto_many_drug <-
    extract_tto(.data = link_,
                adr_s = c("a_colitis"),
                drug_s = c("pd1", "pdl1"))

  tto_many_drug2 <-
    dplyr::bind_rows(
      tto_many_drug |>
        dplyr::filter(drug_s == "pd1") |>
        dplyr::slice_tail(n = 3),
      tto_many_drug |>
        dplyr::filter(drug_s == "pdl1") |>
        dplyr::slice_head(n = 3)
    )

  correct_many_drug <-
    data.frame(
      tto_max = c(85, 26, 59, 681, 29, 105),
      drug_s  = c(rep("pd1", 3), rep("pdl1", 3))
    )

  expect_equal(
    tto_many_drug2 |>
      dplyr::select(tto_max, drug_s),
    correct_many_drug
  )

  tto_many_both <-
    extract_tto(.data = link_,
                adr_s = c("a_colitis", "a_pneumonitis"),
                drug_s = c("pd1", "pdl1"))

  correct_many_both_n <- 88


  expect_equal(
   nrow(tto_many_both),
   correct_many_both_n
  )

})

test_that("output type is consistent in presence or absence of tto data", {
  expect_snapshot({
    link_ <-
      link_ |>
      add_drug(d_code = ex_$d_groups_drecno, drug_data = drug_) |>
      add_adr(a_code = ex_$a_llt, adr_data = adr_)
  })

  tto_a1 <- # adr with some tto data
    extract_tto(.data = link_,

                adr_s = "a_colitis",
                drug_s = "pd1")

  # adr with no tto data

  expect_warning(
    # https://stackoverflow.com/questions/60417969/r-how-to-omit-tested-warning-message-from-test-report-when-testing-for-result-a
    tto_a2 <<-
      extract_tto(.data = link_ |>
                  dplyr::filter(a_pneumonitis == 0),

                adr_s = "a_pneumonitis",
                drug_s = "pd1")
    )

  # counts (n) should all be of type "integer"
  expect_equal(
    class(tto_a1$tto_max),
    class(tto_a2$tto_max)
  )

  expect_equal(
    class(tto_a1$tto_max),
    "numeric"
  )

  # generalization
  all_classes_a1 <-
    lapply(tto_a1, class)

  all_classes_a2 <-
    lapply(tto_a2, class)

  expect_equal(
    all_classes_a1,
    all_classes_a2
  )
})

test_that("breaks if tto_mean or range are missing", {
  expect_snapshot({
    wrong_link <-
      link_ |>
      dplyr::select(-tto_mean) |>
      add_drug(d_code = ex_$d_groups_drecno, drug_data = drug_)  |>
      add_adr(a_code = ex_$a_llt, adr_data = adr_)

    expect_error(
      extract_tto(
        .data = wrong_link,
        adr_s = "a_colitis",
        drug_s = "pd1"
      ),
      "`.data` is not a `link` table",
      fixed = TRUE
    )
  })

  expect_snapshot({
    wrong_luda2 <-
      link_ |>
      dplyr::select(-range) |>
      add_drug(d_code = ex_$d_groups_drecno, drug_data = drug_)  |>
      add_adr(a_code = ex_$a_llt, adr_data = adr_)
  })

  expect_error(
    extract_tto(.data = wrong_luda2,
                adr_s = "a_colitis",
                drug_s = "pd1"),
    "`.data` is not a `link` table",
    fixed = TRUE
  )
})

test_that("works with link as Table (out of memory)", {
  expect_snapshot({
    link_ <-
      link_ |>
      add_drug(d_code = ex_$d_groups_drecno, drug_data = drug_) |>
      add_adr(a_code = ex_$a_llt, adr_data = adr_) |>
      arrow::as_arrow_table()
  })

  tto_test <-
    extract_tto(.data = link_,
                adr_s = "a_colitis",
                drug_s = "pd1")

  true_n_tto_avail <- 39

  expect_equal(
    nrow(tto_test),
    true_n_tto_avail
  )

  true_tto_rch <-
    c(205, 175, 36, 740, 379)

  expect_equal(
    head(tto_test$tto_max, 5),
    true_tto_rch
  )
})
