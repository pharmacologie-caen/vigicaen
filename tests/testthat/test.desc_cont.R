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
      res$n_missing,
      1
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


