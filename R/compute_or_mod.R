
#' Compute (r)OR from a model summary
#'
#' Helper in dplyr to compute and format OR based on `summary(glm)$coefficients`, or any equivalent in other modelling packages. Preferably, it is transformed into a data.table or data.frame before being evaluated in the function. Otherwise, `compute_ror` will transform it.
#'
#' Output is a data.table.
#' Actually, the function computes an Odds-Ratio, which is not necessarily a *reporting* Odds-Ratio.

#' @param x A `summary(glm)$coefficients`table.
#' @param estimate Quasiquoted name of estimate parameter.
#' @param std_er Quasiquoted name of standard error parameter.
#' @param alpha alpha risk.
#' @keywords ror
#' @export
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @examples
#' # Reporting Odds-Ratio of colitis with nivolumab among ICI cases.
#'
#' demo <-
#'   demo %>%
#'   add_drug(
#'     d_code = ex_$d_drecno,
#'     drug_data = drug_
#'   ) %>%
#'   add_adr(
#'     a_code = ex_$a_llt,
#'     adr_data = adr_
#'   )
#'
#' mod <- glm(colitis ~ nivolumab, data = demo, family = "binomial")
#'
#' compute_ror(x = summary(mod)$coefficients) # Preferably, you would transform x before passing it to the function.

compute_ror <-
  function(.mod,
           estimate = Estimate,
           std_er = Std..Error,
           alpha = .05) {

    if(!any(class(.mod) %in% c("data.table", "data.frame"))){
      .mod <- data.frame(rn = row.names(.mod), .mod)
    }

    zval <- qnorm(1 - alpha / 2)
    estimate_p <- paste0(rlang::ensym(estimate))
    std_er_p <- paste0(rlang::ensym(std_er))
    .mod %>%
      mutate(or = exp(.[[estimate_p]]),
             low_ci = exp(.[[estimate_p]] - zval * .[[std_er_p]]),
             up_ci = exp(.[[estimate_p]] + zval * .[[std_er_p]]),
             rorl = cff(ror, dig = 2),
             ci = cff(low_ci = lowci,
                      up_ci = upci,
                      dig = 2,
                      method = "ci"),
             ci_level = paste0((1 - alpha) * 100, "%")) %>%
      data.table()
  }
