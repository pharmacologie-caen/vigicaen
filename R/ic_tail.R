#' Credibility interval limits for the information component
#'
#' @description `r lifecycle::badge('stable')` Compute
#' the Information Component credibility interval, typically the lower end of
#' the 95% CI, also known as the IC025.
#'
#' @details The ends of the credibility interval of the information component
#'  are estimated with the gamma distribution.
#' `n_exp` is defined as `n_drug * n_event / n_total` for the basic IC
#' (formula is different for interactions)
#' Do not add `+.5` to `n_obs` and `n_exp` as it is automatically done in the function.
#' By default, IC025 is computed. Change `p` for different ends.
#' It may be easier to use [compute_dispro()], which internally calls this function.
#'
#' @param n_obs Number of observed cases
#' @param n_exp Number of expected cases (see Details)
#' @param p End of chosen credibility interval
#' @keywords disproportionality ic
#' @seealso [compute_dispro()]
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
