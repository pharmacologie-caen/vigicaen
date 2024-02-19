#' Screening of adverse drug reactions
#'
#' According to a desired term level, sort top seen terms.
#'
#' If `freq_threshold` is set to 0.05, all adverse drug reactions found at least in
#'  5% of adrs in adr_data will be shown. That is not exactly the same as 5% of
#'  cases (that would be found in demo).
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
#'   meddra = meddra_
#' )

screen_adr <-
  function(
    adr_data,
    meddra,
    term_level = "pt",
    freq_threshold = 0.01
    ){

    # check unique MedDRA_Ids from adr_data
    # and apply the frequency threshold.

    m_id_unique <-
      adr_data[, .N, by = MedDRA_Id][
        N > freq_threshold * nrow(adr_data)
      ]

    # summarise, create a term to meddra_id table

    t_to_mid <-
      meddra[llt_code %in% m_id_unique$MedDRA_Id,
             .(pt_name, llt_code)]

    m_id_counts <-
      m_id_unique |>
      left_join(t_to_mid, by = c("MedDRA_Id" = "llt_code")) |>
      summarise(n_cas = sum(N), .by = "pt_name") |>
      arrange(desc(n_cas))

    m_id_counts
  }
