#' Time to onset descriptive
#'
#' @description `r lifecycle::badge('stable')` `desc_tto()` provides a
#' drug-adr pair description of time to onset.
#'
#' @details Description of time (maximum available time) between drug initiation
#' and event onset. This runs at the drug-adr pair level.
#' Internally, it uses [extract_tto()] and [desc_cont()],
#' You will need a `link` data.table, see \code{\link{link_}}, on which
#' you have added drugs and adrs with [add_drug()] and [add_adr()].
#' you can supply extra arguments to [desc_cont()] with `...`.
#' Uppsala Monitoring Centre recommends to use only cases where the incertitude
#' on time to onset is less than **1 day**. You can change this with `tto_time_range`.
#'
#' @param .data A `link` data.table. See \code{\link{link_}}.
#' @param adr_s A character string. The name of the adr column. (see details)
#' @param drug_s A character string. The name of the drug column. (see details)
#' @param tto_time_range Incertitude range of Time to onset, in days. Defaults to 1 as recommended by umc
#' @param ... Additional parameters to be passed to [desc_cont()]. E.g. `format`, `digits`...
#'
#' @return A data.table with one row per drug-adr pair
#' \itemize{
#'   \item A descriptive of time to onsets for this combination (column `tto_max`).
#' }
#' @keywords drug-adr pair, descriptive
#' @export
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @seealso \code{\link{link_}}, [extract_tto()], [add_drug()], [add_adr()], [desc_dch()], [desc_rch()]
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
#' desc_tto(.data = link_,
#'          adr_s = "a_colitis",
#'          drug_s = "pd1")
#'
#'
#' desc_tto(.data = link_,
#'          adr_s = c("a_colitis", "a_pneumonitis"),
#'          drug_s = c("pd1", "ctla4"))

desc_tto <-
  function(.data,
           adr_s,
           drug_s,
           tto_time_range = 1,
           ...
           ){

    # extract ttos

    ttos <-
      extract_tto(
        .data = .data,
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
          desc_cont(
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
