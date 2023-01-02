#' Check binary variables
#'
#' After adding drugs and/or adrs/atc classes to a dataset, check the number of reports with the feature.
#'
#' It is a simple wrapper around `dplyr::summarise`. Be careful not to supply factors with > 2 levels or continuous outcome (the function does NOT have a checker for this, so that it is faster).
#'
#' @param .data A data.frame to be checked
#' @param cols A character vector, name of columns to look at (usually will be `d_names`, `a_names`)
#' @keywords check_dm
#' @export
#' @importFrom dplyr %>%
#' @examples
#' # first create some new variables
#'
#' demo <- demo_
#'
#' demo <-
#'   demo %>%
#'     add_adr(
#'       a_code = ex_$a_llt,
#'       adr_data = adr_
#'     )
#'
#'  # then check the number of reports with each feature
#'
#' demo %>%
#'   check_dm(names(ex_$a_llt))

check_dm <-
  function(.data,
           cols){

    .data %>%
      dplyr::summarise(
        dplyr::across(dplyr::all_of(cols),
                      ~ sum(.x)
        )
      ) %>%
      t()
  }
