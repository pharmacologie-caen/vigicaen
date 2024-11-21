library(testthat)

# testing desc_cont

test_that(
  "result is accurate", {
    df <-
      data.frame(
        smoke_status = c("smoker", "non-smoker",
               "smoker", "smoker",
               "smoker", "smoker",
               "non-smoker"
               ),
        age = c(60, 50, 56, 49, 75, 69, 85),
        bmi = c(18, 30, 25, 22, 23, 21, 22)
      )

    res_age <- desc_cont(vc = c("age"),
               .data = df,
               format = "median (q1;q3) [min-max]",
               dig = 0)

    expect_equal(
      res_age$value,
      "60 (53;72) [49-85]"
    )

    res_bmi <- desc_cont(vc = c("bmi"),
                          .data = df,
                          format = "median (q1;q3) [min-max]",
                          dig = 0)

    expect_equal(
      res_bmi$value,
      "22 (22;24) [18-30]"
    )
  }
)

test_that(
  "big counts have big marks", {
    df <-
      data.frame(
        tto = c(6000, 5000, 5600, 4900, 7500, 6900, 8500)
      )

    res_tto <- desc_cont(vc = c("tto"),
                         .data = df,
                         format = "median (q1;q3) [min-max]",
                         dig = 0)

    expect_equal(
      res_tto$value,
      "6,000 (5,300;7,200) [4,900-8,500]"
    )
  }
)

test_that(
  "digit selection works", {
    df <-
      data.frame(
        age = c(60, 50, 56, 49, 75, 69, 85)
      )

    res_age_d0 <- desc_cont(vc = c("age"),
                          .data = df,
                          format = "median (q1;q3)",
                          dig = 0)

    expect_equal(
      res_age_d0$value,
      "60 (53;72)"
    )

    res_age_d1 <- desc_cont(vc = c("age"),
                          .data = df,
                          format = "median (q1;q3)",
                          dig = 1)

    expect_equal(
      res_age_d1$value,
      "60.0 (53.0;72.0)"
    )
  }
)

test_that(
  "formatting works", {
    df <-
      data.frame(
        age = c(60, 50, 56, 49, 75, 69, 85)
      )

    res_age_f1 <- desc_cont(vc = c("age"),
                             .data = df,
                             format = "median [q1-q3)",
                             dig = 0)

    expect_equal(
      res_age_f1$value,
      "60 [53-72)"
    )

    res_age_f2 <- desc_cont(vc = c("age"),
                             .data = df,
                             format = "median yolooo (q1yepq3",
                             dig = 0)

    expect_equal(
      res_age_f2$value,
      "60 yolooo (53yep72"
    )

    res_age_f3 <- desc_cont(vc = c("age"),
                             .data = df,
                             format = "median; q1 to q3",
                             dig = 0)

    expect_equal(
      res_age_f3$value,
      "60; 53 to 72"
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
        age = c(60, 50, 56, 49, 75, 69, 85),
        bmi = c(18, 30, 25, 22, 23, 21, 22)
      )

    res <- desc_cont(vc = c("age", "bmi"),
                             .data = df,
                             format = "median (q1-q3)",
                             dig = 0)

    expect_equal(
      res$value,
      c("60 (53-72)", "22 (22-24)")
    )

    expect_equal(
      res$var,
      c("age", "bmi")
    )

  }
)

test_that(
  "missing data", {
    df <-
      data.frame(
        age = c(60, 50, 56, 49, 75, NA, 85)
      )

    res <- desc_cont(vc = c("age"),
                      .data = df,
                      format = "median (q1-q3)",
                      dig = 0)

    expect_equal(
      res$value,
      "58 (52-71)"
      )


    expect_equal(
      res$n_avail,
      6
    )

  }
)

test_that(
  "fully missing data", {
    df <-
      data.frame(
        age = c(NA_real_, NA_real_, NA_real_)
      )

    expect_message({
      res <<- desc_cont(vc = c("age"),
                       .data = df,
                       format = "median (q1-q3)",
                       dig = 0)
      },
      "var age is empty"
    )

    res_true <-
      data.frame(var = "age", level = NA_character_, value = "-", n_avail = 0)


    expect_equal(
      res,
      res_true
    )

    df <-
      data.frame(
        age = as.numeric(c(NA, NA, NA))
      )

    expect_message({
      res2 <<- desc_cont(vc = c("age"),
                        .data = df,
                        format = "median (q1-q3)",
                        dig = 0)
    },
    "var age is empty"
    )

    expect_equal(
      res2,
      res_true
    )

  }
)

test_that(
  "doesnt work with categorial columns", {
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
      desc_cont(vc = c("smoke_status"),
                 .data = df,
                 format = "median (q1-q3)",
                 dig = 0)
      ,
      "Non numeric or integer columns selected"
    )


  }
)


test_that(
  "doesnt work if format as two time min", {
    df <-
      data.frame(
        age = c(60, 50, 56, 49, 75, 69, 85)
      )

    expect_error(
      desc_cont(vc = c("age"),
                .data = df,
                format = "median admin (q1-q3) min",
                dig = 0)
      ,
      "format code `min` is present more than once in `format`."
    )


  }
)

test_that(
  "doesnt work with names out of .data names", {
    df <-
      data.frame(
        age = c(60, 50, 56, 49, 75, 69, 85)
      )

    expect_error(
      desc_cont(vc = c("bmi"),
                .data = df,
                format = "median (q1-q3)",
                dig = 0)
      ,
      "Column(s) bmi is(are) absent of .data",
      fixed = TRUE
    )

    expect_error(
      desc_cont(vc = c("bmi", "sex"),
                .data = df,
                format = "median (q1-q3)",
                dig = 0)
      ,
      "Column(s) bmi, sex is(are) absent of .data",
      fixed = TRUE
    )


  }
)

test_that(
  "doesnt work if format as none of min max, q1, q3 and median", {
    df <-
      data.frame(
        age = c(60, 50, 56, 49, 75, 69, 85)
      )

    expect_error(
      desc_cont(vc = c("age"),
                .data = df,
                format = "no formal arg",
                dig = 0)
      ,
      "format arg does not contain any of median, q1, q3, min or max. Please provide at least one."
    )


  }
)

test_that("exporting raw values works", {
  df <-
    data.frame(
      age = c(60, 50, 56, 49, 75, 69, 85)
    )

  res_age <- desc_cont(vc = c("age"),
                       .data = df,
                       format = "median (q1;q3) [min-max]",
                       dig = 0,
                       export_raw_values = TRUE)

  res_age_true <-
    data.frame(
      var = "age",
      level = NA_character_,
      value = "60 (53;72) [49-85]",
      n_avail = 7,
      median = 60,
      q1 = 53,
      q3 = 72,
      min = 49,
      max = 85
    )

  expect_equal(
    res_age,
    res_age_true
  )
})
