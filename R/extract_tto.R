#' Time to onset extraction
#'
#' drug-adr pair extraction of time to onset.
#'
#' Extraction of time (maximum available time) between drug initiation
#' and event onset. This runs at the drug-adr pair level.
#' You might want to run \code{\link{desc_tto}} to obtain summary statistics of
#' time to onset.
#'
#' @param luda_data A data.table. luda stands for Link with UmcreportId, Drug and Adr identifiers (see details).
#' @param adr_s A character string. The name of the adr column. Adr columns can be created with \code{\link{add_adr}} in a luda table.
#' @param drug_s A character string. The name of the drug column. Drug columns can be created with \code{\link{add_drug}} in a luda table.
#' @param tto_time_range Incertitude range of Time to onset, in days. Defaults to 1 as recommended by umc
#'
#' @return A data.table with
#' \itemize{
#'   \item All available time to onsets for this combination (column `tto_max`).
#' }
#' @export
#' @importFrom rlang .data
#' @importFrom rlang .env
#'
#' @examples
#' luda_ <-
#'   luda_ |>
#'   add_drug(
#'     d_code = ex_$d_groups_drecno,
#'     drug_data = drug_,
#'     data_type = "link"
#'   ) |>
#'   add_adr(
#'     a_code = ex_$a_llt,
#'     adr_data = adr_,
#'     data_type = "link"
#'   )
#'
#' extract_tto(luda_data = luda_,
#'          adr_s = "a_colitis",
#'          drug_s = "pd1")
#' extract_tto(luda_data = luda_,
#'          adr_s = c("a_colitis", "a_pneumonitis"),
#'          drug_s = c("pd1", "ctla4"))

extract_tto <-
  function(luda_data,
           adr_s = "a_colitis",
           drug_s = "pd1",
           tto_time_range = 1
  ){

    if(!all(c("tto_mean", "range") %in% names(luda_data))){

      stop("Either tto_mean or range columns are missing. See ?luda_")

    }

    # core extractor ----

    core_extract_tto <-
      function(one_adr,
               one_drug,
               UMCReportId = {{ UMCReportId }}
      ){
        # selection

        luda_sel <-
          luda_data |>
          dplyr::filter(
            dplyr::if_any(dplyr::all_of(.env$one_adr), ~ .x == 1) &
              dplyr::if_any(dplyr::all_of(.env$one_drug), ~ .x == 1) &
                   .data$range <= .env$tto_time_range &
                   .data$tto_mean >= 0
          )

        # TTO for each drug-adr pair - longest delay between drug introduction
        # and adr occurrence

        ttos <-
          luda_sel |>
          dplyr::summarise(
            tto_max = max(.data$tto_mean, na.rm = TRUE),
            .by = UMCReportId
            # its a bit ambiguous to use UMCReportId
            # but works since there is filtering on adr and drug of interest
            # at the previous step
          )

        res <-
          # if(nrow(ttos) == 0){
          #   warning(paste0(
          #     "there is no available time to onset for `",
          #     drug_s,
          #     "` with `", adr_s, "`."
          #   ))
          #
          #   NULL
          #
          # } else {
            ttos |>
              dplyr::mutate(
                .data$tto_max,
                adr_s = .env$one_adr,
                drug_s = .env$one_drug
              )
          # }

        res
      }

    purrr::map(
      adr_s,
      function(one_adr_)
        purrr::map(
          drug_s,
          function(one_drug_)
            core_extract_tto(
              one_adr = one_adr_,
              one_drug = one_drug_
              )
        ) |>
        purrr::list_rbind()
    ) |>
      purrr::list_rbind()

  }
