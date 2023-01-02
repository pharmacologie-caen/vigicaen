
#' Compute (r)OR
#'
#' Compute bivariate OR and IC from contingency table extracted from a data.table.
#'
#' Output is a data.table, containing the ror, the boundaries of the `1 - alpha` confidence interval, the ic, a simple formatting, and whether the result is significant (i.e. `low_ci > 1`).
#' Actually, the function computes an Odds-Ratio, which is not necessarily a reporting Odds-Ratio.

#' @param y A character string, the variable to explain.
#' @param x A character string, the explaining variable.
#' @param alpha Alpha risk.
#' @param dataset The data.table to compute from.
#' @param na_format Character string to fill NA values in ror and ci legends.
#' @param dig Number of digits for rounding (this argument is passed to `cff`)
#' @keywords ror
#' @export
#' @importFrom dplyr %>%
#' @import data.table
#' @examples
#' ## not yet ready
#'

compute_ror_abcd <-
  function(y,
           x,
           alpha = .05,
           dataset,
           na_format = "-",
           dig = 2) {

  z_val <- qnorm(1 - alpha / 2)

  var <- c(y, x)

  eff_table <- dataset[, .(eff = as.numeric(.N)), by = var]

  lc <- function(x){
    if(length(x) == 0) {
      0
    } else {
      x
    }
  }

  a <-
    lc(eff_table[eff_table[[y]] == 1 & eff_table[[x]] == 1, eff])
  b <-
    lc(eff_table[eff_table[[y]] == 1 & eff_table[[x]] == 0, eff])
  c <-
    lc(eff_table[eff_table[[y]] == 0 & eff_table[[x]] == 1, eff])
  d <-
    lc(eff_table[eff_table[[y]] == 0 & eff_table[[x]] == 0, eff])

  n_exp <-
    (a + b) * # n drug
    (a + c) / # n event
    (a + b + c + d) # n pop

  std_er <- sqrt((1 / a) + (1 / b) + (1 / c) + (1 / d))

  table_ror <-
    data.frame(y = y, x = x, a, b, c, d) %>%
    dplyr::mutate(
      ror = a * d / (b * c),
      low_ci = ror * exp(- z_val * std_er),
      up_ci = ror * exp(+ z_val * std_er),
      rorl = ifelse(ror %in% c(0, Inf),
                    na_format,
                    charles::cff(num = ror, dig = dig, method = "num_only")),
      ror_ci = ifelse(
        low_ci %in% c(NaN, 0, Inf),
        na_format,
        charles::cff(low_ci = low_ci, up_ci = up_ci, dig = dig, method = "ci")
        ),
      ic = log((a + .5) / (n_exp + .5), base = 2),
      ic_tail = ic_tail(n_obs = a, n_exp = n_exp, p = alpha / 2),
      ci_level = paste0((1 - alpha) * 100, "%"),
      signif = ifelse(low_ci > 1, 1, 0),
      signif_ic = ifelse(ic_tail > 0, 1, 0)
    ) %>%
    data.table

  table_ror
  }
