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

    # check for unmatched terms

    get_unmatched_terms <- function(one_term){
      unmatch_request <-
        rlang::expr(one_term[!(!!one_term %in% meddra[, unique(!!term_sym)]
        )])

      unmatch <- eval(unmatch_request)

      unmatch
    }

    um_term <- purrr::map(term_sel, get_unmatched_terms) %>%
      purrr::compact()

    if(length(um_term) > 0)
      warning(paste0("In '",
      paste0(names(um_term), collapse = ", "),
      "', the following terms were not found at ",
      term_level,
      " level: ",
      paste0(unlist(um_term), collapse = ", "),
     ". Check spelling."))

    llt_list

  }
