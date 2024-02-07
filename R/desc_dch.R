#' Dechallenge descriptive
#'
#' Compute dechallenge counts over a set of adr and drugs. You would need a
#' `luda` data.table, that is a `link` data.table, joined for `UMCReportId`
#' (via `drug` or `adr` tables), and with appropriate adrs and drugs columns,
#' as adjuncted with `add_drug` and `add_adr`. See \code{\link{luda_}}.
#' If `display_all_levels` is TRUE, Dechallenge actions are factored and you may
#' see combinations that did not happen, with 0 counts
#'
#' @param .data A luda style data.table.
#' @param drug_s A character vector, the drug column(s)
#' @param adr_s A character vector, the adverse drug reaction column(s).
#' @param display_all_levels A logical, do you want to see counts for all combinations, even those with 0?
#' @return Dechallenge descriptive, which is best depicted as a matrix with drug_action as rows and adr_outcome as columns.
#' @export
#' @example
#'
#' luda_dch <- data.table(
#'   UMCReportId = 1:10,
#'   Drug_Id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#'   Adr_Id = c(101, 102, 103, 104, 105, 106, 107, 108, 109, 110),
#'   drug1        = c(1, 0, 1, 1, 0, 1, 0, 1, 1,   1, 1, 1, 1),
#'   drug2        = c(0, 1, 0, 1, 1, 0, 1, 0, 1,   0, 0, 0, 0),
#'   adr1         = c(0, 1, 1, 0, 1, 0, 1, 1, 1,   0, 1, 1, 1),
#'   adr2         = c(1, 0, 1, 0, 1, 0, 1, 0, 0,   1, 0, 0, 0),
#'   Dechallenge1 =
#'   as.character(c(  3, 1, 5, 2, 4, 6, 1, 3, 2,   4, 1, 1, 2)),
#'   Dechallenge2 =
#'   as.character(c(  4, 2, 1, 5, 3, 2, 4, 1, "-", 3, 2, 2, 3))
#' )
#'
#' desc_dch(luda_dch,
#'          drug_s = "drug1",
#'          adr_s = "adr1")
#'
#' # do you want to see only combinations with n >0 ?
#'
#' desc_dch(luda_dch,
#'          drug_s = "drug1",
#'          adr_s = "adr1",
#'          display_all_levels = FALSE)
#'
#' # you can vectorize over multiple adrs and drugs
#'
#' desc_dch(luda_dch,
#'          drug_s = c("drug1", "drug2"),
#'          adr_s = c("adr1", "adr2"),
#'          display_all_levels = FALSE)
#'
#' # You may want to depict it as a matrix
#'
#' desc_dch(luda_dch,
#'          drug_s = c("drug1"),
#'          adr_s = c("adr1")
#'          ) %>%
#'   tidyr::pivot_wider(
#'     id_cols = c( drug_s, adr_s, drug_action),
#'     names_from = adr_outcome,
#'     values_from = n
#'   )


desc_dch <-
  function(.data,
           drug_s = "drug1",
           adr_s = "adr1",
           display_all_levels = TRUE
           ) {

    grouping_variables <- c(adr_s, drug_s, "UMCReportId")

    dechallenge1_label <- data.frame(
      Dechallenge1 = as.character(1:6),
      drug_action = factor(c(
        "Drug withdrawn",
        "Dose reduced",
        "Dose increased",
        "Dose not changed",
        "Unknown",
        "Not applicable"
      ),
      # so as to order the output in a meaningful way
      levels = c(
        "Drug withdrawn",
        "Dose reduced",
        "Dose not changed",
        "Dose increased",
        "Not applicable",
        "Unknown"
      ))
    )

    dechallenge2_label <-
      data.frame(
        Dechallenge2 = as.character(1:5),
        adr_outcome = factor(c(
          "Reaction abated",
          "Fatal",
          "No effect observed",
          "Not applicable",
          "Effect unknown"
        ),
        # so as to order the output in a meaningful way
        levels = c(
          "No effect observed",
          "Reaction abated",
          "Fatal",
          "Not applicable",
          "Effect unknown"
        ))
      )

    dch_core <-
      function(one_drug,
               one_adr) {

        data_subset <-
          .data[.data[[one_drug]] == 1 &
                  .data[[one_adr]] == 1
          ]


        dch <-
          data_subset %>%
          summarise(across(c(Dechallenge1, Dechallenge2),
                           # max here is arbitral, had to make a choice
                           ~ max(.x, na.rm = TRUE)),
                    .by = all_of(grouping_variables)) %>%
          # arrange(desc(Dechallenge1), desc(Dechallenge2)) %>%
          left_join(dechallenge1_label,
                    by = "Dechallenge1") %>%
          left_join(dechallenge2_label,
                    by = "Dechallenge2") %>%
          data.table()

        dch %>%
          group_by(drug_action,
                   adr_outcome,
                   .drop = !display_all_levels) %>%
          arrange(drug_action, adr_outcome) %>%
          summarise(
            drug_s = .env$one_drug,
            adr_s = .env$one_adr,
            n = n()
          ) %>%
          ungroup() %>%
          mutate(
            across(c(drug_action, adr_outcome),
                   ~ as.character(.x))
          ) %>%
          relocate(drug_s, adr_s)
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
        ) %>%
        purrr::list_rbind()
    ) %>%
      purrr::list_rbind()

  }
