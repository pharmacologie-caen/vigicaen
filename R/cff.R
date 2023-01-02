#' Fast formatting of numbers
#'
#' This is a formatting function for consistent number reporting.
#'
#' Set `method` according to the printing you like: a unique number with `num_only` (default), the number and its confidence interval with `num_ci`, a `ci` only (for example a range of time to onset)
#' The function properly returns `NA` when input is missing.
#'
#' @param num A numeric. The number to format.
#' @param low_ci A numeric. Lower end of a confidence interval
#' @param up_ci A numeric. Upper end of a confidence interval
#' @param dig A numeric. Number of digits
#' @param method What sort of printing do you need? (see Details)
#' @keywords number
#' @export
#' @examples
#' ## not yet ready
#' num <- c(0.1, 0.02, 1.658)
#'
#' cff(num)
#'
#' cff(num, dig = 2)
#'
#' cff(num = num[[1]],
#'      low_ci = num[[2]],
#'      up_ci = num[[3]],
#'      method = "num_ci",
#'      dig = 2)


cff <- function(num,
                low_ci,
                up_ci,
                dig = 0,
                method = c("num_only", "num_ci", "ci")) {
  method <- match.arg(method)

  cff_m <- function(n) {
    format(round(n, digits = dig),
           nsmall = dig,
           big.mark = ",",
           trim = TRUE)
  }

  na_detector <- if(method == "ci"){
    low_ci
  } else {
    num
  }

  format <-
    if (method == "num_only") {
      cff_m(num)
    } else {
      if (method == "num_ci") {
        paste0(cff_m(num), " (",
               cff_m(low_ci), "-",
               cff_m(up_ci), ")")
      } else {
        if (method == "ci") {
          paste0("(",
                 cff_m(low_ci), "-",
                 cff_m(up_ci), ")")
        }
      }
    }

  format[which(is.na(na_detector))] <- NA

  format

}

