#' Outcome descriptive
#'
#' @description `r lifecycle::badge('experimental')` Compute
#' outcome description over a set of adr and drugs.
#'
#' @details You need an `adr` data.table.
#' Be careful that you cannot directly filter `adr` data.table on drugs!
#' You first have to add drug columns to `adr`, with [add_drug()].
#' The function reports the worst outcome into consideration for a given case,
#' if many are reported.
#' Outcomes, from best to worst are:
#' \itemize{
#'   \item Recovered/resolved
#'   \item Recovering/resolving
#'   \item Recovered/resolved with sequelae
#'   \item Not recovered/not resolved
#'   \item Fatal
#'   \item Died- unrelated to reaction
#'   \item Died- reaction may be contributory
#' }
#' See `vignette("descriptive")` for more details.
#' @keywords drug-adr pair, descriptive
#' @param .data, An `adr` data.table. See \code{\link{adr_}}
#' @param drug_s A character vector, the drug column(s)
#' @param adr_s A character vector, the adverse drug reaction column(s).
#'
#' @return A data.table with one row per drug-adr pair.
#' \itemize{
#' \item `drug_s` and `adr_s`, same as input
#' \item `n_cas`, number of cases for each category
#' \item `out_label`, the worst outcome for this drug-adr pair
#'}
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @seealso \code{\link{adr_}}, [add_drug()], [add_adr()]
#' @export
#' @examples
#'
#' adr_ <-
#'   adr_ |>
#'   add_drug(
#'     d_code = ex_$d_groups_drecno,
#'     drug_data = drug_,
#'     data_type = "adr"
#'   ) |>
#'   add_adr(
#'     a_code = ex_$a_llt,
#'     adr_data = adr_,
#'     data_type = "adr"
#'   )
#'
#'
#' desc_outcome(
#'   adr_,
#'   drug_s = "pd1",
#'   adr_s = "a_colitis"
#'   )
#'
#'
#' # you can vectorize over multiple adrs and drugs
#'
#' desc_outcome(
#'   adr_,
#'   drug_s = c("pd1", "pdl1"),
#'   adr_s = c("a_colitis", "a_pneumonitis")
#'   )


desc_outcome <-
  function(.data,
           drug_s = "drug1",
           adr_s = "adr1"
           ) {

    # 1Recovered/resolved
    # 2Recovering/resolving
    # 3Not recovered/not resolved
    # 4Recovered/resolved with sequelae
    # 5Fatal
    # 6Unknown
    # 7Died- reaction may be contributory
    # 8Died- unrelated to reaction

    out_worst_mask <-
      data.frame(
        out_worst =
          c(1:7,
            0 # 0 is for NA, see below
            ),
            out_label =
              c(
                "Recovered/resolved",
                "Recovering/resolving",
                "Recovered/resolved with sequelae",
                "Not recovered/not resolved",
                "Fatal",
                "Died- unrelated to reaction",
                "Died- reaction may be contributory",
                "Unknown"
              )
          )

    out_core <-
      function(one_drug,
               one_adr) {

        grouping_variables <- c(one_drug, one_adr, "UMCReportId")

        names(grouping_variables) <- NULL

        data_subset <-
          .data[.data[[one_drug]] == 1 &
                     .data[[one_adr]] == 1
          ]


        out <-
          data_subset |>
          dplyr::mutate(
            out_rank =
              dplyr::case_when(
                # rank outcomes from best to worst, numerically
                Outcome == 1 # Recovered/resolved
                ~ 1,
                Outcome == 2 # Recovering/resolving
                ~ 2,
                Outcome == 4 # Recovered/resolved with sequelae
                ~ 3,
                Outcome == 3 # Not recovered/not resolved
                ~ 4,
                Outcome == 5 # Fatal,
                ~ 5,
                Outcome == 8 # Died- unrelated to reaction
                ~ 6,
                Outcome == 7 # Died- reaction may be contributory
                ~ 7,
                TRUE # ignoring 6, Unknown
                ~ 0 # that's a bit dangerous, coding NA to 0, to
                # silent warnings
              )
          ) |>
          dplyr::summarise(out_worst = max(.data$out_rank),
                    .by = dplyr::all_of(grouping_variables))

        out |>
          dplyr::group_by(.data$out_worst) |>
          dplyr::summarise(
            drug_s = .env$one_drug,
            adr_s = .env$one_adr,
            n_cas =
              dplyr::n()
          ) |>
          dplyr::left_join(out_worst_mask,
                    by = "out_worst") |>
          dplyr::arrange(.data$drug_s, .data$adr_s, .data$out_worst) |>
          dplyr::select(-dplyr::all_of("out_worst")) |>
          dplyr::relocate(drug_s, adr_s)
      }

    purrr::map(
      adr_s,
      function(one_adr_)
        purrr::map(
          drug_s,
          function(one_drug_)
            out_core(
              one_drug = one_drug_,
              one_adr = one_adr_
            )
        ) |>
        purrr::list_rbind()
    ) |>
      purrr::list_rbind()

  }
