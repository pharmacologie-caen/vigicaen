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
               format = "n_/N_ (pc_%)",
               dig = 0,
               pad_width = 0)

    expect_equal(
      res_smoke$value,
      c("2/7 (29%)", "5/7 (71%)")
    )

    res_hta <- desc_facvar(vf = c("hta"),
                            .data = df,
                            format = "n_/N_ (pc_%)",
                            dig = 0,
                            pad_width = 0)

    expect_equal(
      res_hta$value,
      c("4/7 (57%)", "3/7 (43%)")
    )
  }
)

test_that(
  "big counts have big marks from cff", {
    df <-
      data.frame(
        hta = c(rep(1, 1200), rep(0, 2000))
      )

    res_hta <- desc_facvar(vf = c("hta"),
                           .data = df,
                           format = "n_/N_ (pc_%)",
                           dig = 0,
                           pad_width = 0)

    expect_equal(
      res_hta$value,
      c("2,000/3,200 (62%)",
        "1,200/3,200 (38%)")
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
                               format = "n_/N_ (pc_%)",
                               dig = 0,
                               pad_width = 0)

    expect_equal(
      res_hta_d0$value,
      c("4/7 (57%)", "3/7 (43%)")
    )

    res_hta_d1 <- desc_facvar(vf = c("hta"),
                               .data = df,
                               format = "n_/N_ (pc_%)",
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
                               format = "n_/N_",
                               dig = 0,
                               pad_width = 0)

    expect_equal(
      res_hta_f1$value,
      c("4/7", "3/7")
    )

    res_hta_f2 <- desc_facvar(vf = c("hta"),
                             .data = df,
                             format = "n_ yolooo (N_; pc_%)",
                             dig = 0,
                             pad_width = 0)

    expect_equal(
      res_hta_f2$value,
      c("4 yolooo (7; 57%)",
        "3 yolooo (7; 43%)")
    )

    res_hta_f3 <- desc_facvar(vf = c("hta"),
                             .data = df,
                             format = "n_; N_ with percentage equal to pc_",
                             dig = 0,
                             pad_width = 0)

    expect_equal(
      res_hta_f3$value,
      c("4; 7 with percentage equal to 57",
        "3; 7 with percentage equal to 43")
    )

    # change order of params

    res_hta_f4 <- desc_facvar(vf = c("hta"),
                              .data = df,
                              format = "pc_ N_ n_",
                              dig = 0,
                              pad_width = 0)

    expect_equal(
      res_hta_f4$value,
      c("57 7 4", "43 7 3")
    )
  }
)

test_that(
  "you can extract only one param", {
    df <-
      data.frame(
        hta = c(1, 1, 0, 1, 0, 0, 0)
      )

    res_hta_f1 <- desc_facvar(vf = c("hta"),
                              .data = df,
                              format = "n_",
                              dig = 0,
                              pad_width = 0)

    expect_equal(
      res_hta_f1$value,
      c("4", "3")
    )

    res_hta_f2 <- desc_facvar(vf = c("hta"),
                              .data = df,
                              format = "N_",
                              dig = 0,
                              pad_width = 0)

    expect_equal(
      res_hta_f2$value,
      c("7",
        "7")
    )

    res_hta_f3 <- desc_facvar(vf = c("hta"),
                              .data = df,
                              format = "pc_",
                              dig = 0,
                              pad_width = 0)

    expect_equal(
      res_hta_f3$value,
      c("57",
        "43")
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
                        format = "n_/N_ (pc_%)",
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
                      format = "n_/N_ (pc_%)",
                      dig = 0,
                      pad_width = 0)

    expect_equal(
      res$value,
      c("3/6 (50%)",
        "3/6 (50%)")
      )


    expect_equal(
      res$n_avail,
      c(6, 6)
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

    expect_snapshot(error = TRUE, {
      desc_facvar(vf = c("age"),
                 .data = df,
                 format = "n_/N_ (pc_%)",
                 dig = 0,
                 pad_width = 0,
                 ncat_max = 3)
    })


  }
)


test_that(
  "doesnt work if format does not have n_, N_ or pc_", {
    df <-
      data.frame(
        smoke_status = c("smoker", "non-smoker",
                         "smoker", "smoker",
                         "smoker", "smoker",
                         "non-smoker"
        )
      )

    expect_error(
      desc_facvar(vf = c("smoke_status"),
                  .data = df,
                  format = "n/N (pc%)",
                  dig = 0,
                  pad_width = 0,
                  ncat_max = 3)
      ,
      "format arg does not contain any of n_, N_, or pc_"
    )


  }
)

test_that(
  "doesnt work if format as two time n_", {
    df <-
      data.frame(
        smoke_status = c("smoker", "non-smoker",
                         "smoker", "smoker",
                         "smoker", "smoker",
                         "non-smoker"
        )
      )

    expect_error(
      desc_facvar(vf = c("smoke_status"),
                  .data = df,
                  format = "n_/n_ (pc%)",
                  dig = 0,
                  pad_width = 0,
                  ncat_max = 3)
      ,
      "format code `n_` is present more than once in `format`."
    )


  }
)

test_that(
  "doesnt work with vf outside of .data names", {

    df <-
      data.frame(
        smoke_status = c("smoker", "non-smoker",
                         "smoker", "smoker",
                         "smoker", "smoker",
                         "non-smoker"
        )
      )

    expect_error(
      desc_facvar(vf = c("bmi"),
                .data = df)
      ,
      "Column(s) bmi is(are) absent of .data",
      fixed = TRUE
    )

    expect_error(
      desc_facvar(vf = c("bmi", "sex"),
                .data = df)
      ,
      "Column(s) bmi, sex is(are) absent of .data",
      fixed = TRUE
    )
  }
)

test_that(
  "exporting raw values work", {
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
                        format = "n_/N_ (pc_%)",
                        dig = 0,
                        pad_width = 0,
                        export_raw_values = TRUE) |>
      dplyr::mutate(
        pc = cff(pc, dig = 2)
      )

    res_true <-
      dplyr::tibble(
        var = c("hta", "hta",
                "smoke_status", "smoke_status"),
        level = c("0", "1", "non-smoker", "smoker"),
        value =
          c("4/7 (57%)", "3/7 (43%)",
            "2/7 (29%)", "5/7 (71%)"),
        n_avail =
          c(7, 7, 7, 7),
        n = c(4, 3, 2, 5),
        pc = c("57.14", "42.86", "28.57", "71.43")
      )

    expect_equal(
      res,
      res_true
    )
  })

