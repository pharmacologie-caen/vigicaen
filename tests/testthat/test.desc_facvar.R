library(testthat)

test_that(
  "result is accurate", {
    df <-
      data.frame(
        smoke_status = c("smoker", "non-smoker",
               "smoker", "smoker",
               "smoker", "smoker",
               "non-smoker"
               ),
        hta = c(1, 1, 0, 1, 0, 0, 0),
        bmi = c(18, 30, 25, 22, 23, 21, 22)
      )

    res_smoke <- desc_facvar(vf = c("smoke_status"),
               .data = df,
               format = "n/N (pc%)",
               dig = 0,
               pad_width = 0)

    expect_equal(
      res_smoke$value,
      c("2/7 (29%)", "5/7 (71%)")
    )

    res_hta <- desc_facvar(vf = c("hta"),
                            .data = df,
                            format = "n/N (pc%)",
                            dig = 0,
                            pad_width = 0)

    expect_equal(
      res_hta$value,
      c("4/7 (57%)", "3/7 (43%)")
    )
  }
)

test_that(
  "digit selection works", {
    df <-
      data.frame(
        hta = c(1, 1, 0, 1, 0, 0, 0)
      )

    res_hta_d0 <- desc_facvar(vf = c("hta"),
                               .data = df,
                               format = "n/N (pc%)",
                               dig = 0,
                               pad_width = 0)

    expect_equal(
      res_hta_d0$value,
      c("4/7 (57%)", "3/7 (43%)")
    )

    res_hta_d1 <- desc_facvar(vf = c("hta"),
                               .data = df,
                               format = "n/N (pc%)",
                               dig = 1,
                               pad_width = 0)

    expect_equal(
      res_hta_d1$value,
      c("4/7 (57.1%)", "3/7 (42.9%)")
    )
  }
)

test_that(
  "formatting works", {
    df <-
      data.frame(
        hta = c(1, 1, 0, 1, 0, 0, 0)
      )

    res_hta_f1 <- desc_facvar(vf = c("hta"),
                               .data = df,
                               format = "n/N",
                               dig = 0,
                               pad_width = 0)

    expect_equal(
      res_hta_f1$value,
      c("4/7", "3/7")
    )

    res_hta_f2 <- desc_facvar(vf = c("hta"),
                             .data = df,
                             format = "n yolooo (N; pc%)",
                             dig = 0,
                             pad_width = 0)

    expect_equal(
      res_hta_f2$value,
      c("4 yolooo (7; 57%)",
        "3 yolooo (7; 43%)")
    )

    res_hta_f3 <- desc_facvar(vf = c("hta"),
                             .data = df,
                             format = "n; N with percentage equal to pc",
                             dig = 0,
                             pad_width = 0)

    expect_equal(
      res_hta_f3$value,
      c("4; 7 with percentage equal to 57",
        "3; 7 with percentage equal to 43")
    )
  }
)

test_that(
  "multiple columns selection", {
    df <-
      data.frame(
        smoke_status = c("smoker", "non-smoker",
                         "smoker", "smoker",
                         "smoker", "smoker",
                         "non-smoker"
        ),
        hta = c(1, 1, 0, 1, 0, 0, 0)
      )

    res <- desc_facvar(vf = c("hta", "smoke_status"),
                        .data = df,
                        format = "n/N (pc%)",
                        dig = 0,
                        pad_width = 0)

    expect_equal(
      res %>%
        dplyr::filter(
          var == "hta" &
            level == 0) %>%
        dplyr::pull(value),
      "4/7 (57%)"
    )

    expect_equal(
      res %>%
        dplyr::filter(
          var == "smoke_status" &
            level == "smoker") %>%
        dplyr::pull(value),
      "5/7 (71%)"
    )

    expect_equal(
      res$var,
      c("hta", "hta", "smoke_status", "smoke_status")
    )

  }
)

test_that(
  "missing data", {
    df <-
      data.frame(
        hta = c(1, 1, 0, 1, 0, NA, 0)
      )

    res <- desc_facvar(vf = c("hta"),
                      .data = df,
                      format = "n/N (pc%)",
                      dig = 0,
                      pad_width = 0)

    expect_equal(
      res$value,
      c("3/6 (50%)",
        "3/6 (50%)")
      )


    expect_equal(
      res$n_missing,
      c(1, 1)
    )

  }
)

test_that(
  "doesnt work with continuous columns", {
    df <-
      data.frame(
        smoke_status = c("smoker", "non-smoker",
                         "smoker", "smoker",
                         "smoker", "smoker",
                         "non-smoker"
        ),
        age = c(60, 50, 56, 49, 75, 69, 85)
      )

    expect_error(
      desc_facvar(vf = c("age"),
                 .data = df,
                 format = "n/N (pc%)",
                 dig = 0,
                 pad_width = 0,
                 ncat_max = 3)
      ,
      "too many levels detected"
    )


  }
)


