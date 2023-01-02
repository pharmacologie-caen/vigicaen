
#' Compute (r)OR from a model summary
#'
#' Helper in dplyr to compute and format OR based on `summary(glm)$coefficients`, or any equivalent in other modelling packages. Preferably, it is transformed into a data.table or data.frame before being evaluated in the function. Otherwise, `compute_or_mod` will transform it.
#' Significant **R**-or column means low_ci is > 1.
#'
#' Output is a data.table.
#' Actually, the function computes an Odds-Ratio, which is not necessarily a *reporting* Odds-Ratio.

#' @param .coef_table A coefficient table, see details.
#' @param estimate Quasiquoted name of estimate parameter.
#' @param std_er Quasiquoted name of standard error parameter.
#' @param p_val Quasiquoted name of p-value parameter
#' @param alpha alpha risk.
#' @keywords ror
#' @export
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @examples
#' # Reporting Odds-Ratio of colitis with nivolumab among ICI cases.
#'
#' demo <-
#'   demo_ %>%
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
#' coef_table <-
#'  mod %>%
#'  summary() %>%
#'  .$coefficients
#'
#' coef_table %>%
#'   compute_or_mod(
#'   estimate = Estimate,
#'   std_er = Std..Error,
#'   p_val = Pr...z..) # Preferably, you would transform x before passing it to the function.
#'
#'   coef_table %>%
#'   compute_or_mod(
#'   estimate = Estimate,
#'   std_er = Std..Error)

compute_or_mod <-
  function(.coef_table,
           estimate = Estimate,
           std_er = Std..Error,
           p_val = NULL,
           alpha = .05) {

    if(!any(class(.coef_table) %in% c("data.table", "data.frame"))){
      .coef_table <- data.frame(rn = row.names(.coef_table), .coef_table)
    }

    zval <- qnorm(1 - alpha / 2)

    calcs <-
      .coef_table %>%
      mutate(or = exp({{ estimate }}),
             low_ci = exp({{ estimate }} - .env$zval * {{ std_er }}),
             up_ci  = exp({{ estimate }} + .env$zval * {{ std_er }}),
             orl = cff(or, dig = 2),
             ci =  cff(low_ci = low_ci,
                       up_ci = up_ci,
                       dig = 2,
                       method = "ci"),
             ci_level = paste0((1 - .env$alpha) * 100, "%"),
             signif_ror = if_else(low_ci > 1, 1, 0),
             p_val = nice_p({{ p_val }})
             ) %>%
      data.table()

    if(unique(calcs$p_val) == "p_val was NULL"){
      calcs$p_val <- NULL
    }

    # return

    calcs
  }
