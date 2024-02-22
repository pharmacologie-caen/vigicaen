#' Screening of adverse drug reactions
#'
#' According to a desired term level, sort top seen terms.
#'
#' If `freq_threshold` is set to 0.05, all adverse drug reactions found at least in
#'  5% of adrs in adr_data will be shown. That is not exactly the same as 5% of
#'  cases. Counts are provided at *case* level (not adr level).
#'
#' @param adr_data A data.table, an adr data
#' @param meddra A data.table, a meddra data
#' @param term_level A character string, one of "pt", "hlt", "hlgt", "soc".
#' @param freq_threshold A numeric, lowest frequency percentage of term to be
#' screened.
#'
#' @return A data.frame with counts of each term in adr_data.
#' @export
#'
#' @examples
#' screen_adr(
#'   adr_data = adr_,
#'   meddra = meddra_,
#'   term_level = "pt"
#' )
#'
#' screen_adr(
#'   adr_data = adr_,
#'   meddra = meddra_,
#'   term_level = "hlt"
#' )

screen_adr <-
  function(
    adr_data,
    meddra,
    term_level =  c("soc", "hlgt", "hlt", "pt", "llt"),
    freq_threshold = 0.01
    ){

    term_level <- match.arg(term_level)

    term_level_name <- paste0(term_level, "_name")

    term_sym <- rlang::sym(term_level_name)

    # check unique MedDRA_Ids from adr_data
    # and apply the frequency threshold.

    m_id_unique <-
      adr_data[, .N, by = MedDRA_Id][
        N > freq_threshold * nrow(adr_data)
      ]

    umc_mid <-
      adr_data[, ]

    # summarise, create a term to meddra_id table

    t_to_mid <-
      rlang::eval_tidy(rlang::expr(
        meddra[llt_code %in% m_id_unique$MedDRA_Id,
             .(!!term_sym, llt_code)]
      ))

    # join that pt_names table to adr_data and count cases

    n_case_counts <-
      adr_data |>
      left_join(t_to_mid, by = c("MedDRA_Id" = "llt_code")) |>
      select(UMCReportId, all_of(term_level_name)) |>
      distinct() |>
      summarise(n_cas = n(), .by = .env$term_level_name) |>
      arrange(desc(n_cas))

    n_case_counts
  }
