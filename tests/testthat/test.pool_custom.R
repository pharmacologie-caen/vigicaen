context("pool_custom")

test_that("A standard Cox model can be extracted just as with the usual mice workflow", {

   df <-
      data.frame(
      event = c(0, 0, 0, 1, 1, 1, NA, NA, 0, 1, 0, 1, 1, 1),
      time = c(10, NA, 13, 25, 50, 4, 9, 25, 15, 13, 16, 25, NA, 40),
      param = c(1, 1, 0, 1, NA, 1, 0, 1, 1, 1, NA, NA, 0, 0)
      )

   imp_df <- mice::parlmice(df, m = 5)

   # survival analysis

   mod_surv <-
     imp_df %>%
     mice::complete("all") %>%
     map(function(imp_data)
       coxph(Surv(time = time,
                  event = event == 1) ~ param,
             data = imp_data))

   fit_df <-
     mod_surv %>%
     purrr::map(summary, type = "tidy", exponentiate = FALSE) %>%
     purrr::map("coefficients") %>%
     purrr::map_dfr(data.table, keep.rownames = TRUE)

   nparam <- 1 # only param at the moment

   dfcom <- mod_surv[[1]]$nevent - nparam # note that this is SPECIFIC to coxph

   r_pool_custom <-
     pool_custom(mod_surv,
      fit_df = fit_df,
      dfcom = dfcom,
      term = "rn",
      estimate = "coef",
      std.error = "se(coef)") %>% summary()

   r_pool_mice <-
     pool(mod_surv) %>%
     summary()

  expect_equal(r_pool_custom,
               r_pool_mice)

})

test_that("A standard logistic regression can be extracted just as with the usual mice workflow", {

  df <-
    data.frame(
      event = c(0, 0, 0, 1, 1, 1, NA, NA, 0, 1, 0, 1, 1, 1),
      time = c(10, NA, 13, 25, 50, 4, 9, 25, 15, 13, 16, 25, NA, 40),
      param = c(1, 1, 0, 1, NA, 1, 0, 1, 1, 1, NA, NA, 0, 0)
    )

  imp_df <- mice::parlmice(df, m = 5)

  # glm binomial analysis

  mod_glm <-
    imp_df %>%
    mice::complete("all") %>%
    map(function(imp_data)
      glm(event ~ param, family = "binomial",
            data = imp_data))

  fit_df <-
    mod_glm %>%
    purrr::map(summary, type = "tidy", exponentiate = FALSE) %>%
    purrr::map("coefficients") %>%
    purrr::map_dfr(data.table, keep.rownames = TRUE)


  dfcom <- mod_glm[[1]]$df.residual

  # or

  dfcom <- broom::glance(mod_glm[[1]])$df.residual

  r_pool_custom <-
    pool_custom(mod_glm,
                fit_df = fit_df,
                dfcom = dfcom,
                term = "rn",
                estimate = "Estimate",
                std.error = "Std. Error") %>%
    summary()

  r_pool_mice <-
    pool(mod_glm) %>%
    summary()

  expect_equal(r_pool_custom,
               r_pool_mice)

})
