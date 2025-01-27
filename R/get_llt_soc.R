#' Extract low level terms from soc classification
#'
#' @description `r lifecycle::badge('stable')` Collect llt codes from a `meddra`
#' data.table, given another term of the MedDRA SOC Hierarchy.
#'
#' @details `get_llt_soc()` is an *ID collector* function. The function extracts low level term codes.
#' `get_llt_soc()` is **case-sensitive**, and MedDRA terms always begin with a capital letter,
#' in their native version.
#' In `term_sel`, all terms should come from the same hierarchical level,
#' e.g. all preferred terms, all high level terms, etc.
#'
#' @param term_sel A named list of character vector(s). The terms to extract llts codes from. See details.
#' @param term_level A character string. One of "soc", "hlgt", "hlt", "pt", or "llt"
#' @param meddra A data.table. Built from meddra_builders functions
#' @keywords data_management meddra soc llt
#' @seealso [get_llt_smq()]
#' @export
#' @examples
#'
#' ## Finding llt codes for colitis
#'
#' pt_sel <- rlang::list2(
#'   colitis = c("Colitis",
#'               "Autoimmune colitis"),
#'   pneumonitis = c("Pneumonitis",
#'                   "Organising pneumonia")
#'   )
#'
#' hlt_sel <- rlang::list2(
#'   colitis = c("Gastrointestinal inflammatory disorders NEC"),
#'   pneumonitis = c("Pulmonary thrombotic and embolic conditions")
#'   )
#'
#' # Remember you can use more than one term to define each adverse reaction,
#' # but they should all be at the same hierarchical level in meddra.
#'
#' # with preferred terms
#'
#' get_llt_soc(
#'   term_sel = pt_sel,
#'   term_level = "pt",
#'   meddra = meddra_
#'   )
#'
#' # with high level terms
#'
#' get_llt_soc(
#'   term_sel = hlt_sel,
#'   term_level = "hlt",
#'   meddra = meddra_
#'   )

get_llt_soc <-
  function(
    term_sel,
    term_level = c("soc", "hlgt", "hlt", "pt", "llt"),
    meddra
  ) {
    term_level <- match.arg(term_level)

    term_sym <- rlang::sym(paste0(term_level, "_name"))

    if("Table"  %in% class(meddra)){
      # automatically collect meddra if out of memory
      # since it's a small table
      meddra <-
        dplyr::collect(meddra)
    }

    get_one_term_llt <- function(one_term,
                                 llt_code = {{ llt_code }}){
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

    um_term <- purrr::map(term_sel, get_unmatched_terms) |>
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
