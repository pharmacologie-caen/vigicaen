#' Nice printing of p-values
#'
#' @description `r lifecycle::badge('stable')` Formatting function
#' for consistent p-value reporting.
#'
#' You can choose to print the leading zero (e.g. `0.01`) or not
#' (e.g. `.01`) with `print_zero`.
#'
#' @param p_val A numeric. The p-value to format.
#' @param print_zero A logical. Should leading zero be printed? (see Details)
#' @keywords pvalue
#' @export
#' @examples
#' pvals <-
#'   c(0.056548, 0.0002654, 0.816546, 0.0493321)
#' nice_p(pvals)
#'
#' nice_p(pvals, print_zero = TRUE)

nice_p <- function(p_val,
                   print_zero = FALSE) {

  if(!is.null(p_val)) {
    format <-
    dplyr::case_when(
      p_val < .0001 ~ "<0.0001",
      p_val < .001 ~  "<0.001",
      p_val < .01 ~ "<0.01",
      p_val > .045 & p_val < .055 ~ format(round(p_val, 3), nsmall = 3),
      TRUE ~ format(round(p_val, 2), nsmall = 2)
    )

  if (!print_zero) {
    format <- gsub("0.", ".", format, fixed = TRUE)
  }

  format
  } else {
    "p_val was NULL"
  }
}
