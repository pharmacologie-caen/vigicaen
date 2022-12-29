#' Extract llts from soc classification
#'
#' Extracts a list of llt codes from a meddra data.table, given a specified level term
#'
#' The function extracts llt codes. It throws a warning when nothing was found, indicating that there is probably a mistake in the spelling of the term you were looking for. In meddra, terms are case sensitive and they are not tolower-ed by default (e.g. you may find "Colitis" but not "colitis" or "COLITIS").
#' In term_sel, all terms should come from the same hierarchical level, e.g. all preferred terms, all high level terms, etc.
#'
#' @param term_sel A named list of character vector. The terms to extract llts codes from. See details.
#' @param term_level A character vector. One of "hlgt", "hlt", "pt", or "llt"
#' @param meddra A data.table. Built from meddra_builders functions
#' @keywords meddra soc llt
#' @export
#' @examples
#'
#' ## Finding llt codes for colitis
#'
#' pt_sel <- rlang::list2(
#'   colitis = ex_$pt_sel$colitis,
#'   pneumonitis = ex_$pt_sel$pneumonitis
#'   )
#'
#' # Remember you can use more than one term to define each adverse reaction,
#' # but they should all be at the same hierarchical level in meddra.
#'
#' get_llt_soc(
#'   term_sel = pt_sel,
#'   term_level = "pt",
#'   meddra = ex_$meddra
#'   )

get_llt_soc <-
  function(
    term_sel,
    term_level = c("hlgt", "hlt", "pt", "llt"),
    meddra
  ) {
    term_level <- match.arg(term_level)

    term_sym <- rlang::sym(paste0(term_level, "_name"))

    get_one_term_llt <- function(one_term){
      term_request <- rlang::expr(!!term_sym %in% !!one_term)

      llt <- meddra[eval(term_request), unique(llt_code)]

      llt
    }

    llt_list <- purrr::map(term_sel, get_one_term_llt)

    length_llt_list_element <-
      purrr::map(llt_list, length)

    if(any(length_llt_list_element == 0)){
      zero_llt_element <-
        length_llt_list_element %>%
        purrr::keep(~ .x == 0)

      zero_llt_element_names <-
        paste0(names(zero_llt_element), collapse = ", ")

      warning(paste0("no llt code found for '",
                     zero_llt_element_names,
                     "' at ",
                     term_level,
                     " level. Check spelling.")
              )
    }

    if(length(llt_list) == 0){
      warning(paste0("no llt code found for ", term, " at ", term_level, " level. Check spelling."))
    }

    llt_list

  }
