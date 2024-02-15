#' Extract llts from SMQs
#'
#' Extracts from a smq_list_content a list of llts given an SMQ.
#'
#' !! You must use this function only for NON-algorithmic SMQs (this status is given in the smq_list table). There are 2 non-blocking checkers in the function. Be sure to add ` (SMQ)` at the end of the SMQ name. The other checker is when nothing is found. The function doesn't work with character vectors of length > 1.
#'
#' @param smq A named list of character vector of length 1.
#' @param smq_scope A character vector. One of "narrow" or "broad".
#' @param smq_list_content A data.table. A joint of smq_list and smq_content
#' @keywords meddra smq llt
#' @export
#' @examples
#' ## Finding llt codes for Embolism (SMQ)
#'
#' smq_sel <- rlang::list2(
#'   embolism = "Embolic and thrombotic events, venous (SMQ)"
#'  )
#' get_llt_smq(smq_sel,
#'                 smq_scope = "narrow",
#'                 smq_list_content = smq_list_content_
#'                 )

get_llt_smq <-
  function(
    smq,
    smq_scope = c("narrow", "broad"),
    smq_list_content
  ){
    smq <-
      purrr::map(smq, function(s_)
      if_else(
        stringr::str_detect(s_, "\\s\\(SMQ\\)$"),
        s_,
        paste0(s_, " (SMQ)")
      )
      )

    smq_scope <- match.arg(smq_scope)

    smq_scope_code <-
      dplyr::case_when(
        smq_scope == "narrow" ~ c("2"),
        smq_scope == "broad" ~ c("1", "2"),
        TRUE ~ "this is an error"
      )

    get_one_smq_llt <- function(one_smq){
      smq_list_content[smq_name == one_smq &
                         term_scope %in% smq_scope_code &
                         term_status == "A" &
                         # A pour terme actif
                         smq_algorithm == "N",
                       # N : not algorithmic

                       unique(term_code)
                       ]
    }

    llt_list <- purrr::map(smq, get_one_smq_llt)

    length_llt_list_element <-
      purrr::map(llt_list, length)

    # Handling 0 results issues ----

    if(any(length_llt_list_element == 0)){

      # debugging steps

      zero_llt_element <-
        length_llt_list_element %>%
        purrr::keep(~ .x == 0)

      zero_llt_element_names <-
        names(zero_llt_element)

      # step 1 : are there high level SMQs ?

      high_level_smq <-
        smq[zero_llt_element_names] %>%
        purrr::map(function(zero_s){
          # if you find zero_s in the list of smqs
          zero_s %in% unique(smq_list_content$smq_name) &&
            # and those smqs are not algorithmic
            smq_list_content[smq_name == zero_s, all(smq_algorithm == "N")]
        }) %>%
        purrr::keep(isTRUE)

       if(length(high_level_smq) > 0)
        warning(paste0("In '",
                       names(high_level_smq),
                       "', at least one of '",
                       paste0(smq[names(high_level_smq)], collapse = ", "),
                       "' is/are high level smq(s), you need to query lower level smq(s)"))

      # step 2 : are there algorithmic smq?

      non_hls_zerollt_element <-
        zero_llt_element_names[!zero_llt_element_names %in%
                                 names(high_level_smq)]


      smq_algorithm_not_n <-
        smq[non_hls_zerollt_element] %>%
        purrr::map(function(s_)
          s_ %in% unique(smq_list_content$smq_name) &&
          smq_list_content[smq_name == s_ &
                             term_status == "A",
                           all(!(smq_algorithm == "N"))
          ]
        ) %>%
        purrr::keep(isTRUE)

      if(length(smq_algorithm_not_n) > 0)
        # this should be an error, as it is out of the function scope
        stop(paste0("smq '",

                    paste0(names(smq_algorithm_not_n), collapse = ", "),
                    "' is/are algorithmic, they are not handled by get_llt_smq."))
    }

    # check for unmatched terms (its smarter than the previous, since is does
    # capture an unmatched term in the middle of good ones)

    get_unmatched_terms <- function(one_smq){
      unmatch_request <-
        rlang::expr(one_smq[!(!!one_smq %in% smq_list_content[, unique(smq_name)]
        )])

      unmatch <- eval(unmatch_request)

      unmatch
    }

    um_term <- purrr::map(smq, get_unmatched_terms) %>%
      purrr::compact()

    if(length(um_term) > 0)
      warning(paste0("In '",
                     paste0(names(um_term), collapse = ", "),
                     "', the following elements were not found: ",
                     paste0(unlist(um_term), collapse = ", "),
                     ". Check spelling."))

    llt_list

  }
