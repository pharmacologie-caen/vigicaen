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
#' @param verbose Logical. Allows you to see matching reactions in the console.
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
    meddra,
    verbose = TRUE
  ) {
    term_level <- rlang::arg_match(term_level)

    term_col  <- paste0(term_level, "_name")

    if("Table"  %in% class(meddra)){
      # automatically collect meddra if out of memory
      # since it's a small table
      meddra <-
        dplyr::collect(meddra)
    }

    check_data_meddra(meddra)

    # check for match and collect term codes

    llt_codes_dataset <-
      get_term_matching_and_codes(term_sel, term_col, meddra)

    # gather matching and non matching terms

    matching_terms <-
      llt_codes_dataset |>
      purrr::map(function(lcd){
        lcd |>
          dplyr::filter(!is.na(.data$match)) |>
          dplyr::distinct(.data$term) |>
          dplyr::pull(term)
      })

    unmatching_terms <-
      llt_codes_dataset |>
      purrr::map(function(lcd){
        lcd |>
          dplyr::filter(is.na(.data$match)) |>
          dplyr::distinct(.data$term) |>
          dplyr::pull(term)
      }) |>
      purrr::compact()

    # keep codes

    llt_list <-
      llt_codes_dataset |>
      purrr::map(function(lcd){
        lcd |>
          dplyr::filter(!is.na(.data$match)) |>
          dplyr::pull(.data$llt_code) |>
          unique()
      })

    if (length(unmatching_terms) > 0 | verbose == TRUE){
      cli_h1("get_llt_soc()")

      if(verbose == TRUE)
        msg_getlltsoc_match(llt_codes_dataset, term_level)

    if(length(unmatching_terms) > 0){
      cli_h2("{col_red({symbol$cross})} Unmatched reactions")
      msg_getlltsoc_no_match(unmatching_terms, term_level)
    }
}
    llt_list

  }

# Helpers -----------------------------------------------------

get_term_matching_and_codes <-
  function(term_sel, term_col, meddra){
    llt_codes_dataset <-
      term_sel |>
      purrr::map(function(t_)
        data.frame(
          term = t_
        ) |>
          dplyr::left_join(
            meddra |>
              dplyr::distinct(.data[[term_col]], .data$llt_code) |>
              dplyr::mutate(match = 1),
            by = c("term" = term_col)
          )
        )

    return(llt_codes_dataset)
  }

msg_getlltsoc_match <-
  function(llt_codes_dataset,
           term_level
  ){
    msg_match <- function() {

      lines <-
        llt_codes_dataset |>
        purrr::map(function(lcd)
          lcd |>
            dplyr::filter(!is.na(.data$match)) |>
            dplyr::group_by(.data$term) |>
            dplyr::summarise(lab =
                        paste0(unique(term), " (",dplyr::n(), ")")) |>
            dplyr::pull(.data$lab)
        )

      cli_par()

      cli_h2("{col_green({symbol$tick})} Matched reactions at {.code {term_level}} level (number of codes)")

      cli_end()
      cli_par()
      lines_cli <-
        lines |> purrr::imap(function(l_, n_){

          l_lab <- if(length(l_) == 0) "{symbol$cross} No match" else "{.val {l_}}"

          cli::cli_inform(
            c(">" = paste0(
              "{.code {n_}}: ",
              l_lab)
            )
          )
        })

      cli_end()
      cli_par()
      cli::cli_alert_info(
        "Set {.arg verbose} to FALSE to suppress this section."
      )
      cli_end()

    }
    msg_match()
  }


msg_getlltsoc_no_match <-
  function(res_list_no_match,
           term_level
  ){

    res_list_no_match_compact <-
      purrr::compact(res_list_no_match)

    no_capital_letter <-
      res_list_no_match_compact |>
      purrr::map(function(nm_)
        nm_ |>
          stringr::str_subset("^[a-z]")
        )

    other_unmatched_terms <-
      list(ini = res_list_no_match_compact,
           no_cap = no_capital_letter) |>
      purrr::pmap(function(ini, no_cap)
        ini[!ini %in% no_cap]
      )

    no_cap_compact <-
      purrr::compact(no_capital_letter)

    other_compact <-
      purrr::compact(other_unmatched_terms)

    msg_no_match <-
      function() {

        cli_par()

        if (length(other_compact) > 0) {
          cli_h3(paste0(
            col_yellow("!"),
            " Some reactions were not found at ",
            "{.code {term_level}} level"
          ))

          cli_end()
          cli_par()

          lid <- cli_ul()
          for (i in seq_along(other_compact)) {
            cli_li(
              paste0(
                'In {.code {names(other_compact)[i]}}:',
                col_red(' {symbol$cross} '),
                "{.val {other_compact[[i]]}}",
                ''
              )
            )

          }

          cli_end(lid)
        }

        if (length(no_cap_compact) > 0) {
          cli_h3(
            paste0(
              col_yellow("!"),
              " Some reactions did not start with a ",
              col_yellow(style_underline("C")),
              "apital letter"
            )
          )

          cli_end()
          cli_par()

          lid2 <- cli_ul()
          for (i in seq_along(no_cap_compact)) {
            cli_li(
              paste0(
                'In {.code {names(no_cap_compact)[i]}}:',
                col_red(' {symbol$cross} '),
                "{.val {no_cap_compact[[i]]}}",
                ''
              )
            )

          }

          cli_end(lid2)
        }
      }

    msg_no_match()
  }
