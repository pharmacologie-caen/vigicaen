#' Time to onset extraction
#'
#' @description `r lifecycle::badge('stable')` `extract_tto()` collects
#' all available time to onsets for a set of drug-adr pairs.
#'
#' @details Extraction of (maximum available) time between drug initiation
#' and event onset. This runs at the drug-adr pair level.
#' You will need a `link` data.table, see \code{\link{link_}}, on which
#' you have added drugs and adrs with [add_drug()] and [add_adr()].
#' Uppsala Monitoring Centre recommends to use only cases where the incertitude
#' on time to onset is less than **1 day**. You can change this with `tto_time_range`.
#' You might want to use [desc_tto()] to obtain summary statistics of
#' time to onset, but `extract_tto()` is useful to get the raw data and plot it,
#' for instance with `ggplot2`.
#'
#' @param .data A `link` data.table. See \code{\link{link_}}.
#' @param adr_s A character string. The name of the adr column. (see details)
#' @param drug_s A character string. The name of the drug column. (see details)
#' @param tto_time_range Incertitude range of Time to onset, in days. Defaults to 1 as recommended by umc
#'
#' @return A data.frame with
#' \itemize{
#'   \item All available time to onsets for this combination (column `tto_max`).
#'   \item `adr_s` and `drug_s`, same as input.
#'   \item `UMCReportId`, the unique identifier of the case.
#' }
#' @keywords drug-adr pair, descriptive
#' @export
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @seealso \code{\link{link_}}, [desc_tto()], [add_drug()], [add_adr()], [desc_dch()], [desc_rch()]
#'
#' @examples
#' link_ <-
#'   link_ |>
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
#' extract_tto(.data = link_,
#'          adr_s = "a_colitis",
#'          drug_s = "pd1")
#' extract_tto(.data = link_,
#'          adr_s = c("a_colitis", "a_pneumonitis"),
#'          drug_s = c("pd1", "ctla4"))

extract_tto <-
  function (.data,
            adr_s,
            drug_s,
            tto_time_range = 1)
  {
    check_data_link(.data)

    # if (!all(c("tto_mean", "range") %in% names(.data))) {
    #   stop("Either tto_mean or range columns are missing. See ?link_")
    # }
    core_extract_tto <- function(one_adr, one_drug, UMCReportId = {
      {
        UMCReportId
      }
    }) {

      renamer <-
        c(one_adr_col = one_adr,
          one_drug_col = one_drug)

      link_sel <-
        .data |>
        dplyr::rename(dplyr::all_of(renamer)) |>
        dplyr::filter(
          .data$one_adr_col == 1 &
            .data$one_drug_col == 1 &
            .data$range <= .env$tto_time_range &
            .data$tto_mean >= 0
        )

      ttos <- dplyr::summarise(link_sel,
                               tto_max = max(.data$tto_mean, na.rm = TRUE),
                               .by = UMCReportId)
      res <- dplyr::mutate(ttos,
                           adr_s = .env$one_adr,
                           drug_s = .env$one_drug) |>
        dplyr::collect()

      res
    }
    purrr::list_rbind(purrr::map(adr_s, function(one_adr_)
      purrr::list_rbind(
        purrr::map(drug_s, function(one_drug_)
          core_extract_tto(one_adr = one_adr_, one_drug = one_drug_))
      )))
  }

