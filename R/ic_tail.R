#' Credibility interval limits for the information component
#'
#' Compute the IC credibility interval, typically the lower end of the 95% CI, also known as the IC025.
#'
#' The ends of the credibility interval of the information componant are estimated with the gamma distribution.
#' `n_exp` is defined as `n_drug * n_event / n_total` for the basic IC (formula is different for interactions)
#' Do not add `+.5` to `n_obs` and `n_exp` as it is automatically down in the function.
#' By default, IC025 is computed. Change `p` for different ends.
#'
#' @param n_obs Number of observed cases
#' @param n_exp Number of expected cases (see Details)
#' @param p End of chosen credibility interval
#' @keywords ic
#' @export
#' @examples
#'
#' ic_tail(n_obs = 12,
#'         n_exp = 5)
#'

ic_tail <- function(n_obs = NULL,
                    n_exp = NULL,
                    p = .025) {
  if (is.null(n_obs) || is.null(n_exp)) {
    stop("you must supply n_obs AND n_exp (do not add + .5)")
  }
  log(stats::qgamma(p, shape = n_obs + .5, rate = n_exp + .5), 2)
}
