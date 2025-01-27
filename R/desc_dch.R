#' Dechallenge descriptive
#'
#' @description `r lifecycle::badge('stable')` Computes positive
#' dechallenge counts over a set of adr and drug pairs.
#'
#' @details Counts are provided at the **case** level (not the drug-adr pair level).
#' Positive dechallenge refers to cases where drug was withdrawn or
#' dose-reduced and reaction abated (in part or in full).
#' You will need a `link` data.table, see \code{\link{link_}}, on which
#' you have added drugs and adrs with [add_drug()] and [add_adr()].
#'
#' @param .data A `link` data.table.
#' @param drug_s A character vector, the drug column(s)
#' @param adr_s A character vector, the adverse drug reaction column(s).
#' @return A data.table with one row per drug-adr pair.
#' \itemize{
#'  \item `drug_s` and `adr_s`, same as input
#'  \item `pos_dch`, number of positive dechallenge cases
#'  }
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @keywords drug-adr pair, descriptive
#' @export
#' @seealso \code{\link{link_}}, [add_drug()], [add_adr()], [desc_tto()], [desc_rch()]
#' @examples
#'
#' link_ <-
#'   link_ |>
#'   add_drug(
#'     d_code = ex_$d_groups_drecno,
#'     drug_data = drug_
#'   ) |>
#'   add_adr(
#'     a_code = ex_$a_llt,
#'     adr_data = adr_
#'   )
#'
#'
#' desc_dch(link_,
#'          drug_s = "pd1",
#'          adr_s = "a_colitis")
#'
#'
#' # you can vectorize over multiple adrs and drugs
#'
#' desc_dch(link_,
#'          drug_s = c("pd1", "pdl1"),
#'          adr_s = c("a_colitis", "a_pneumonitis"))


desc_dch <-
  function(.data,
           drug_s = "drug1",
           adr_s = "adr1"
           ) {

    check_data_link(.data)

    # dechallenge1_label <- data.frame(
    #   Dechallenge1 = as.character(1:6),
    #   drug_action = factor(c(
    #     "Drug withdrawn",
    #     "Dose reduced",
    #     "Dose increased",
    #     "Dose not changed",
    #     "Unknown",
    #     "Not applicable"
    #   ),
    #   # so as to order the output in a meaningful way
    #   levels = c(
    #     "Drug withdrawn",
    #     "Dose reduced",
    #     "Dose not changed",
    #     "Dose increased",
    #     "Not applicable",
    #     "Unknown"
    #   ))
    # )
    #
    # dechallenge2_label <-
    #   data.frame(
    #     Dechallenge2 = as.character(1:5),
    #     adr_outcome = factor(c(
    #       "Reaction abated",
    #       "Fatal",
    #       "No effect observed",
    #       "Not applicable",
    #       "Effect unknown"
    #     ),
    #     # so as to order the output in a meaningful way
    #     levels = c(
    #       "No effect observed",
    #       "Reaction abated",
    #       "Fatal",
    #       "Not applicable",
    #       "Effect unknown"
    #     ))
    #   )

    dch_core <-
      function(one_drug,
               one_adr) {

        grouping_variables <- c(one_drug, one_adr, "UMCReportId")

        names(grouping_variables) <- NULL

        data_subset <-
          .data[.data[[one_drug]] == 1 &
                  .data[[one_adr]] == 1
          ]


        dch <-
          data_subset |>
          dplyr::mutate(
            pos_dch =
              ifelse(
                .data$Dechallenge1 %in% c("1", "2") &
                  .data$Dechallenge2 %in% c("1"),
                1,
                # be careful with the 0, only use instead of
                # NA to shut warning in max, below
                0
              )
          ) |>
          dplyr::summarise(max_pos_dch = max(.data$pos_dch),
                    .by = dplyr::all_of(grouping_variables))

        dch |>
          dplyr::summarise(
            drug_s = .env$one_drug,
            adr_s = .env$one_adr,
            pos_dch = sum(.data$max_pos_dch)
          ) |>
          dplyr::relocate(drug_s, adr_s)
      }

    purrr::map(
      adr_s,
      function(one_adr_)
        purrr::map(
          drug_s,
          function(one_drug_)
            dch_core(
              one_drug = one_drug_,
              one_adr = one_adr_
            )
        ) |>
        purrr::list_rbind()
    ) |>
      purrr::list_rbind()

  }
