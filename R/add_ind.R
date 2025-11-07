#' Add indication column(s) to a dataset
#'
#' @description `r lifecycle::badge('experimental')` Creates indication columns.
#' in vigibase datasets (demo, link, adr, drug, or ind).
#'
#' @details Indication terms are issued from either MedDRA or International
#' Classification of Diseases (ICD) - you need to use *both* dictionaries, should
#' you wish to capture all terms related to a specific disease.
#' Indication terms are not translated into codes in VigiBase ECL,
#' unlike drug or adr terms. Therefore, there is no `get_*` step to collect
#' such codes. The terms are passed directly to `i_list`, which should still be
#' a *named* list containing indication terms.
#'
#' @param .data   The dataset used to identify individual reports (usually, it is `demo`)
#' @param i_list  A named list of indication terms. See Details.
#' @param i_names A character vector. Names for indication columns (must be the same length as i_list), default to `names(i_list)`
#' @param drug_data A data.frame containing the drug data (usually, it is `drug`)
#' @param ind_data  A data.frame containing the indication data (usually, it is `ind`)
#'
#' @returns A dataset with the new indication columns.
#' Each element of `i_names` will add a column with the same name in `.data`.
#' The value can be
#' \itemize{
#'  \item 0 The corresponding indication is absent.
#'  \item 1 The indication is present in the case if `.data` is `demo` or `adr`,
#' or "this row correspond to this indication",
#' if `.data` is `drug`, `link` or `ind`).
#'  \item NA There is no indication data for this case / drug.
#'  }
#' @export
#'
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @seealso [add_adr()], [add_drug()]
#'
#' @examples
#'
#' # Set up a list of indication terms
#'
#' i_list <-
#'   list(
#'     melanoma = c("Malignant melanoma", "Metastatic malignant melanoma"),
#'     lung_cancer = c("Non-small cell lung cancer", "Lung adenocarcinoma")
#'     )
#'
#'  demo <-
#'    demo_ |>
#'    add_ind(i_list,
#'            drug_data = drug_,
#'            ind_data  = ind_)
#'
#'  demo |> desc_facvar(names(i_list))

add_ind <-
  function(.data,
           i_list,
           i_names = names(i_list),
           drug_data,
           ind_data){

    # 0. arrow options

    # keep original user option, then set it

    original_user_option <- options("arrow.pull_as_vector")

    options(arrow.pull_as_vector = FALSE)

    # 1. checkers

    check_data_drug(drug_data, "drug_data")
    check_data_ind(ind_data, "ind_data")

    data_type <-
      query_data_type(.data, ".data")

    # 2. identify table_ids to collect

    t_id <-
      switch(data_type,
             demo = "UMCReportId",
             adr  = "UMCReportId",
             link = "Drug_Id",
             drug = "Drug_Id",
             ind  = "Drug_Id"
      )

    renamer_tid <-
      c("t_id" = t_id)

    dd <-
      drug_data |>
      dplyr::rename(dplyr::all_of(renamer_tid))

    # 3. collect drug_ids from ind_data

    d_ids <-
      purrr::map(i_list, function(i_list_batch) {
        ind_data |>
          dplyr::filter(.data$Indication %in% i_list_batch) |>
          dplyr::pull(.data$Drug_Id)

      })

    # 3.1 collect t_ids if different from d_ids

    t_ids <-
      # if .data is demo or adr, collect t_id
      if (data_type %in% c("demo", "adr")) {
        purrr::map(d_ids, function(d_id_batch) {
          dd |>
            dplyr::filter(.data$Drug_Id %in% d_id_batch) |>
            dplyr::pull(.data$t_id)

        })
      } else {
        # otherwise, t_id = d_id
        d_ids
      }

    # 4 collect entries from .data with no data in ind_data

    # 4.1 collect all Drug_Ids from ind_data

    full_ind_ids <-
      ind_data |>
      dplyr::distinct(.data$Drug_Id) |>
      dplyr::pull(.data$Drug_Id)

    # 4.2 match it to .data

    t_id_not_in_ind_data <-
      if (data_type %in% c("demo", "adr")){
        # case level datasets
        # you need first to match full_ind_ids to drug

        full_ind_related_umc_ids <-
          drug_data |>
          dplyr::filter(.data$Drug_Id %in% full_ind_ids) |>
          dplyr::distinct(.data$UMCReportId) |>
          dplyr::pull(.data$UMCReportId)

        .data |>
          dplyr::filter(!(.data[[t_id]] %in% .env$full_ind_related_umc_ids)) |>
          dplyr::pull(.data[[t_id]])


      } else {
        # drug level datasets

        .data |>
          dplyr::filter(!(.data[[t_id]] %in% .env$full_ind_ids)) |>
          dplyr::pull(.data[[t_id]])

      }

    # 4. prepare expressions for data management

    e_l <-
      t_ids |>
      purrr::map(function(t_id_subset){

        rlang::quo(
          # case_when not implemented in arrow
          ifelse(
            .data$t_id %in% t_id_not_in_ind_data,
            NA_real_,
            ifelse(.data$t_id %in% t_id_subset,
                   1,
                   0
            )
        ))
      }) |>
      rlang::set_names(i_names)

    # 5. prepare destination table

    dest_data <-
      .data |>
      dplyr::rename(dplyr::all_of(renamer_tid))

    # 6. add_cols

    dest_data_withcols <-
      dest_data |>
      dplyr::mutate(
        !!!e_l
        )

    # 7. back rename table id to original name

    back_renamer <-
      c("t_id") |>
      rlang::set_names(t_id)

    final_data <-
      dest_data_withcols |>
      dplyr::rename(dplyr::all_of(back_renamer))

    # 8. restore user option

    options(arrow.pull_as_vector = original_user_option)

    # 9. compute everything (this is strictly required only for arrow objects)

    if(any(c("Table", "Dataset") %in% class(.data))){
      final_data |>
        dplyr::compute()
    } else {
      final_data
    }


  }
