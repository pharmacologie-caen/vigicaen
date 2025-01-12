test_that("basic use works", {
  mp <-
    data.frame(
      DrecNo = c(1, 1, 2),
      Sequence.number.1 = c("01", "02", "01"),
      Sequence.number.2 = c("001", "001", "001"),
      drug_name_t = c("d1", "d1", "d2")
    )

  drug <-
    data.frame(
      UMCReportId = c(1, 1, 2, 3, 4, 5, 6),
      Drug_Id = c(1, 2, 3, 4, 5, 6, 7),
      DrecNo = c(1, 2, 1, 1, 2, 2, 2),
      MedicinalProd_Id = rep(9, 7)
    )

  sd_true <-
    dplyr::tibble(
      `Drug name` = c("d2", "d1"),
      DrecNo  = c(2, 1),
      N = c(4, 3),
      percentage = c("66.7", "50.0")
    )

  sd_res <-
    screen_drug(.data = drug, mp) |>
    dplyr::mutate(percentage = cff(.data$percentage, dig = 1))

  expect_equal(sd_true, sd_res)

  sd_freq_filter <-
    screen_drug(.data = drug, mp,
                freq_threshold = 0.60) |>
    dplyr::mutate(percentage = cff(.data$percentage, dig = 1))

  sd_topn_filter <-
    screen_drug(.data = drug, mp,
                top_n = 1) |>
    dplyr::mutate(percentage = cff(.data$percentage, dig = 1))

  expect_equal(
    sd_freq_filter,
    sd_true |> dplyr::slice_head(n = 1)
  )

  expect_equal(
    sd_topn_filter,
    sd_true |> dplyr::slice_head(n = 1)
  )
})

test_that("checkers of mp_data and .data are ok", {

  mp_true <-
    data.frame(
      DrecNo = 1,
      Sequence.number.1 = 1,
      Sequence.number.2 = 2,
      drug_name_t = "a"
    )

  mp_false <-
    data.frame(
      DrecNo = 1
    )

  mp_false2 <-
    data.frame(
      drug_name_t = "a"
    )

  drug_true <-
    data.frame(
      UMCReportId = 1,
      Drug_Id = 1,
      DrecNo = 2,
      MedicinalProd_Id = 1
    )

  drug_false <-
    data.frame(
      UMCReportId = 1,
      DrecNo = 1
    )

  cli::test_that_cli(
    "error in mp_data",{
    expect_snapshot(error = TRUE, {
    screen_drug(
      drug_true,
      mp_data = mp_false
      )
      })
    })

  expect_snapshot(error = TRUE, {
    screen_drug(
      drug_true,
      mp_data = mp_false2
    )
  })

  expect_snapshot(error = TRUE, {
    screen_drug(
      drug_false,
      mp_data = mp_true
    )
  })
})

test_that("issue warning if both freq_thresold and top_n provided", {
  mp <-
    data.frame(
      DrecNo = c(1, 1, 2),
      Sequence.number.1 = c("01", "02", "01"),
      Sequence.number.2 = c("001", "001", "001"),
      drug_name_t = c("d1", "d1", "d2")
    )

  drug <-
    data.frame(
      UMCReportId = c(1, 1, 2, 3, 4, 5, 6),
      Drug_Id = c(1, 2, 3, 4, 5, 6, 7),
      DrecNo = c(1, 2, 1, 1, 2, 2, 2),
      MedicinalProd_Id = rep(9, 7)
    )

  expect_warning(
      res <-
        screen_drug(
      drug,
      mp_data = mp,
      freq_threshold = 0.60,
      top_n = 1
    )
    )

})
