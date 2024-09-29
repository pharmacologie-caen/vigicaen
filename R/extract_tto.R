#' Time to onset extraction
#'
#' @description `r lifecycle::badge('stable')` `extract_tto()` collects
#' all available time to onsets for a set of drug-adr pairs.
#'
#' @details Extraction of (maximum available) time between drug initiation
#' and event onset. This runs at the drug-adr pair level.
#' You will need a `luda` data.table, see \code{\link{luda_}}, on which
#' you have added drugs and adrs with [add_drug()] and [add_adr()].
#' Uppsala Monitoring Centre recommends to use only cases where the incertitude
#' on time to onset is less than **1 day**. You can change this with `tto_time_range`.
#' You might want to use [desc_tto()] to obtain summary statistics of
#' time to onset, but `extract_tto()` is useful to get the raw data and plot it,
#' for instance with `ggplot2`.
#'
#' @param .data A \code{\link{luda_}} style data.table.
#' @param adr_s A character string. The name of the adr column. (see details)
#' @param drug_s A character string. The name of the drug column. (see details)
#' @param tto_time_range Incertitude range of Time to onset, in days. Defaults to 1 as recommended by umc
#'
#' @return A data.table with
#' \itemize{
#'   \item All available time to onsets for this combination (column `tto_max`).
#' }
#' @keywords drug-adr pair, descriptive
#' @export
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @seealso \code{\link{luda_}}, [desc_tto()], [add_drug()], [add_adr()], [desc_dch()], [desc_rch()]
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
#' extract_tto(.data = luda_,
#'          adr_s = "a_colitis",
#'          drug_s = "pd1")
#' extract_tto(.data = luda_,
#'          adr_s = c("a_colitis", "a_pneumonitis"),
#'          drug_s = c("pd1", "ctla4"))

extract_tto <-
  function(.data,
           adr_s = "a_colitis",
           drug_s = "pd1",
           tto_time_range = 1
  ){

    if(!all(c("tto_mean", "range") %in% names(.data))){

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
          .data |>
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
