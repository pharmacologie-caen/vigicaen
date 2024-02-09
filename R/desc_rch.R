#' Rechallenge and TTO description
#'
#' drug-adr pair description of rechallenge cases and time to onsets.
#'
#' Description span from rechallenge cases to __informative__ rechallenge cases (those cases where the outcome is known). Drug and Adr identifiers refer to DrecNo and MedDRA_Id, respectively.
#' Terminology
#' \itemize{
#'   \item `Overall` as opposed to `rch` for rechallenged (`rch` + `no_rch` = `overall`).
#'   \item Among `rch`, `inf` (informative) as opposed to `non_inf` (`inf` + `non_inf` = `rch`)
#'   \item Among `inf`, `rec` (recurring) as opposed to `non_rec` (`rec` + `non_rec` = `inf`)
#' }
#'
#' @param luda_data A data.table. luda stands for Link with UmcreportId, Drug and Adr identifiers (see details).
#' @param demo_data A data.table. demo should have a serious and death columns.
#' @param drug_s A character string. The name of the drug column. Drug columns can be created with \code{\link{add_drug}} in a luda table.
#' @param adr_s A character string. The name of the adr column. Adr columns can be created with \code{\link{add_adr}} in a luda table.
#' @param tto_time_range Incertitude range of Time to onset, in days. Defaults to 1 as recommended by umc
#'
#' @return A one-row data.table with
#' \itemize{
#'   \item Counts of __overall__, __rch__, __inf__, and __rec__ cases (see details).
#'   \item Number of cases with a time to onset `tto` available in all settings.
#'   \item Median (interquartile range) time to onset in all settings.
#' }
#' @export
#' @import dplyr data.table
#'
#' @examples
#'
#' desc_rch(luda_data = luda_,
#'          demo_data = demo_,
#'          drug_s = "pd1",
#'          adr_s = "a_colitis")
#'
#' # You can vectorize over drugs and adrs
#'
#' desc_rch(luda_data = luda_,
#'          demo_data = demo_,
#'          adr_s = c("a_colitis", "a_pneumonitis"),
#'          drug_s = c("pd1", "pdl1")
#'          )

desc_rch <- function(luda_data,
                     demo_data,
                     drug_s = "pd1",
                     adr_s = "a_colitis",
                     tto_time_range = 1
){


  core_desc_rch <-
    function(one_drug,
             one_adr
    ){
      luda_sel <- # selection
        luda_data %>%
        dplyr::filter(.data[[one_drug]] == 1 &
                 .data[[one_adr]] == 1
               )

      demo_sel <- demo_data[UMCReportId %in% luda_sel[, UMCReportId]]

      luda_sel_rch <-
        luda_sel %>%
        dplyr::filter(Rechallenge1 == "1")

      demo_sel_rch <- demo_sel[UMCReportId %in% luda_sel_rch[, UMCReportId]]

      luda_sel_inf <-
        luda_sel %>%
        dplyr::filter(Rechallenge2 %in% c("1", "2"))

      demo_sel_inf <- demo_sel[UMCReportId %in% luda_sel_inf[, UMCReportId]]

      luda_sel_rec <-
        luda_sel %>%
        dplyr::filter(Rechallenge2 %in% c("1"))

      demo_sel_rec <- demo_sel[UMCReportId %in% luda_sel_rec[, UMCReportId]]

      n_overall <- demo_sel[, .N]

      # ---- Counting rechal cases ---- #

      # ++++ Any ++++ #
      n_rch <- demo_sel_rch[, .N]

      # ++++ Inf ++++ #
      n_inf <- demo_sel_inf[, .N]

      # ++++ Recurring cases ++++ #
      n_rec <- demo_sel_rec[, .N]

      # Output of results
      data.table::data.table(
        drug = one_drug,
        adr = one_adr,

        n_overall,

        n_rch,
        n_inf,
        n_rec
        )
    }

  purrr::map(
    adr_s, function(one_adr_)
      purrr::map(
        drug_s, function(one_drug_)
          core_desc_rch(
            one_drug = one_drug_,
            one_adr = one_adr_
          )
      ) %>%
      purrr::list_rbind()
  ) %>%
    purrr::list_rbind()

}
