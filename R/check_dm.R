#' Check binary variables
#'
#' @description `r lifecycle::badge('stable')` Quick check
#' that your data management steps through \code{\link{add_adr}}
#' or \code{\link{add_drug}} found cases.
#'
#' @details It is a simple wrapper around `dplyr::summarise()`.
#' Be careful not to supply factors with > 2 levels or continuous outcome
#' (the function does NOT have a checker for this, so that it is faster).
#' Also, the function WONT work with NAs. Use [desc_facvar()].
#' if you need more detailed description of your dataset.
#'
#' @param .data A data.frame to be checked
#' @param cols A character vector, name of columns to look at (usually will be `d_names`, `a_names`)
#' @keywords data_management
#' @export
#' @seealso [desc_facvar()], [add_adr()], [add_drug()]
#' @examples
#' # first create some new variables
#'
#' demo <- demo_
#'
#' demo <-
#'   demo |>
#'     add_adr(
#'       a_code = ex_$a_llt,
#'       adr_data = adr_
#'     )
#'
#'  # then check the number of reports with each feature
#'
#' demo |>
#'   check_dm(names(ex_$a_llt))

check_dm <-
  function(.data,
           cols){

    .data  |>
      dplyr::summarise(
        dplyr::across(dplyr::all_of(cols),
                      ~ sum(.x)
        )
      )  |>
      t()
  }
