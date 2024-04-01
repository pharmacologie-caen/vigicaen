#' Time to onset descriptive
#'
#' drug-adr pair description of time to onset.
#'
#' Description of time (maximum available time) between drug initiation
#' and event onset. This runs at the drug-adr pair level.
#' Internally, this runs \code{\link{extract_tto}} and \code{\link{desc_cont}},
#' you can supply extra arguments to `desc_cont` with `...`.
#'
#' @param luda_data A data.table. luda stands for Link with UmcreportId, Drug and Adr identifiers (see details).
#' @param adr_s A character string. The name of the adr column. Adr columns can be created with \code{\link{add_adr}} in a luda table.
#' @param drug_s A character string. The name of the drug column. Drug columns can be created with \code{\link{add_drug}} in a luda table.
#' @param tto_time_range Incertitude range of Time to onset, in days. Defaults to 1 as recommended by umc
#' @param ... Additional parameters to be passed to \code{\link{desc_cont}}. E.g. `format`, `digits`...
#'
#' @return A one row data.table with
#' \itemize{
#'   \item A descriptive of time to onsets for this combination (column `tto_max`).
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
#' desc_tto(luda_data = luda_,
#'          adr_s = "a_colitis",
#'          drug_s = "pd1")
#'
#'
#' desc_tto(luda_data = luda_,
#'          adr_s = c("a_colitis", "a_pneumonitis"),
#'          drug_s = c("pd1", "ctla4"))

desc_tto <-
  function(luda_data,
           adr_s = "a_colitis",
           drug_s = "pd1",
           tto_time_range = 1,
           ...
           ){

    # extract ttos

    ttos <-
      pharmacocaen::extract_tto(
        luda_data = luda_data,
        drug_s = drug_s,
        adr_s = adr_s,
        tto_time_range = tto_time_range
      )

    # pass to desc_cont

    core_desc_cont <-
      function(one_adr,
               one_drug){
        res <-
          ttos |>
          dplyr::filter(
            adr_s == .env$one_adr,
            drug_s == .env$one_drug
          ) |>
          pharmacocaen::desc_cont(
            vc = "tto_max",
            ...
          ) |>
          dplyr::mutate(
            adr_s = .env$one_adr,
            drug_s = .env$one_drug
          ) |>
          dplyr::relocate(
            drug_s,
            adr_s
          )
      }

    purrr::map(
      adr_s, function(one_adr_)
        purrr::map(
          drug_s, function(one_drug_)
            core_desc_cont(
              one_adr = one_adr_,
              one_drug = one_drug_
            )
        ) |>
        purrr::list_rbind()
    ) |>
      purrr::list_rbind()

}
