#' Screening of adverse drug reactions
#'
#' @description `r lifecycle::badge('experimental')` `screen_adr()` sort top seen terms,
#' according to a desired term level.
#'
#' @details If `freq_threshold` is set to 0.05, all adverse drug reactions found
#' at least in 5% of adrs in .data will be shown. That is not exactly the same as 5% of
#'  cases. Counts are provided at *case* level (not adr level).
#' The function uses an \code{\link{adr_}} data.table and a \code{\link{meddra_}} data.table.
#'
#' @param .data A data.table, an adr data
#' @param meddra A data.table, a meddra data
#' @param term_level A character string, one of "pt", "hlt", "hlgt", "soc".
#' @param freq_threshold A numeric, lowest frequency percentage of term to be
#' screened.
#'
#' @return A data.frame with counts of each term in .data.
#' @export
#' @keywords descriptive, adr
#' @examples
#' screen_adr(
#'   .data = adr_,
#'   meddra = meddra_,
#'   term_level = "pt"
#' )
#'
#' screen_adr(
#'   .data = adr_,
#'   meddra = meddra_,
#'   term_level = "hlt"
#' )

screen_adr <-
  function(
    .data,
    meddra,
    term_level =  c("soc", "hlgt", "hlt", "pt", "llt"),
    freq_threshold = 0.01
    ){

    term_level <- match.arg(term_level)

    term_level_name <- paste0(term_level, "_name")

    term_sym <- rlang::sym(term_level_name)

    # check unique MedDRA_Ids from .data
    # and apply the frequency threshold.
    create_mid_unique_table <-
      function(
    .data,
    MedDRA_Id = {{ MedDRA_Id }},
    N = {{ N }}){

      .data[, .N, by = MedDRA_Id][
        N > freq_threshold * nrow(.data)
      ]
      }

    m_id_unique <-
      create_mid_unique_table(.data)

    # summarise, create a term to meddra_id table

    create_term_to_mid_table <-
      function(
    meddra,
    m_id_unique,
    llt_code = {{ llt_code }}
    ){

      rlang::eval_tidy(rlang::expr(
        meddra[llt_code %in% m_id_unique$MedDRA_Id,
             list(!!term_sym, llt_code)]
      ))
      }

    t_to_mid <- create_term_to_mid_table(meddra, m_id_unique)

    # join that pt_names table to .data and count cases

    n_case_counts <-
      .data |>
      dplyr::left_join(t_to_mid, by = c("MedDRA_Id" = "llt_code")) |>
      dplyr::select(dplyr::all_of(c("UMCReportId", term_level_name))) |>
      dplyr::distinct() |>
      dplyr::summarise(n_cas = dplyr::n(), .by = .env$term_level_name) |>
      dplyr::arrange(dplyr::desc(.data$n_cas))

    n_case_counts
  }
