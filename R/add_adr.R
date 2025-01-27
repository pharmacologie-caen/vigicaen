#' Add ADR column(s) to a dataset
#'
#' @description `r lifecycle::badge('stable')`Creates adr columns
#' in vigibase datasets (demo, link, drug, but also adr).
#'
#' @details Low-level term codes are the preferred level of requesting in
#' Vigibase extract case level since they capture all possible codes for a given
#' Preferred Term. Collect low-level terms with [get_llt_soc()] and
#' [get_llt_smq()]. You can add adr identification to a `demo`, a `link`, `drug`
#' or even an `adr` dataset (in this latter case, you must provide `adr` twice,
#' as `.data` and `adr_data`). Column names of these dataset should not have been
#' modified from the original vigibase dataset (as created with [tb_vigibase()]).
#'
#' @param .data The dataset to update (demo, link, drug, adr).
#' @param a_code A named list of low level terms codes (llt_codes).
#' @param a_names A character vector. Names for adr columns (must be the
#' same length as adr_list), default to `names(a_code)`
#' @param adr_data A data.frame containing the adr data (usually, it is `adr`)
#' @param data_type `r lifecycle::badge('deprecated')`. Data_type is now detected
#' internally.
#' @returns A dataset with the new adr columns.
#' Each value of `a_names` will add a column with the same name in `.data`.
#' @export
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @keywords data_management adr
#' @seealso [add_drug()], [get_llt_soc()], [get_llt_smq()]
#' @examples
#' # create adr_colitis, adr_embolism and adr_pneumonitis columns in demo
#'
#' # be careful, this example may overwrite your own demo dataset
#' demo <- demo_
#'
#' a_pt_sel <- ex_$pt_sel
#'
#'
#' adr <- adr_
#'
#' a_llt <-
#'   get_llt_soc(
#'   term_sel = a_pt_sel,
#'   term_level = "pt",
#'   meddra = meddra_
#'   )
#'
#' demo <-
#'   demo |>
#'     add_adr(
#'       a_code = a_llt,
#'       adr_data = adr
#'     )
#'
#' demo |>
#'   check_dm(names(a_pt_sel))

add_adr <-
  function(.data,
           a_code,
           a_names = names(a_code),
           adr_data,

           data_type = deprecated()
           ){

    check_id_list_numeric(a_code)

    check_data_adr(adr_data)

    # Check if user has supplied `data_type`.
    if (lifecycle::is_present(data_type)) {

      # Signal the deprecation to the user
      lifecycle::deprecate_soft(
        when = "0.14.1",
        what = "add_adr(data_type)",
        details = c("i" = "data_type is now internally detected")
      )
    }

    data_type <-
      query_data_type(.data, ".data")

    # identify table_ids to collect

    t_id <-
      switch(data_type,
             demo = "UMCReportId",
             adr  = "Adr_Id",
             link = "Adr_Id",
             drug = "UMCReportId"
      )


    renamer_tid <-
      c("t_id" = t_id)

    # adr data renamed
    ad_rn <-
      adr_data |>
      dplyr::rename(dplyr::all_of(renamer_tid))

    # collect table_ids

    t_ids <-
      purrr::map(a_code, function(a_code_batch){
        if(any(c("Table", "Dataset") %in% class(.data))){
          ad_rn |>
            dplyr::filter(.data$MedDRA_Id %in% a_code_batch) |>
            dplyr::pull(.data$t_id, as_vector = FALSE)
        } else {
          ad_rn |>
            dplyr::filter(.data$MedDRA_Id %in% a_code_batch) |>
            dplyr::pull(.data$t_id)
        }
      })

    e_l <-
      t_ids |>
      purrr::map(function(t_id_subset){

        rlang::quo(ifelse(
          .data$t_id %in% t_id_subset,
          1, 0
        ))
      }
      ) |>
      rlang::set_names(a_names)

    # prepare destination table

    dest_data <-
      .data |>
      dplyr::rename(dplyr::all_of(renamer_tid))

    # add_cols

    dest_data_withcols <-
      dest_data |>
      dplyr::mutate(
        !!!e_l
      )

    # back rename table id to original name

    back_renamer <-
      c("t_id") |>
      rlang::set_names(t_id)

    final_data <-
      dest_data_withcols |>
      dplyr::rename(dplyr::all_of(back_renamer))

    # compute everything (this is strictly required only for arrow objects)

    if(any(c("Table", "Dataset") %in% class(.data))){
      final_data |>
        dplyr::compute()
    } else {
      final_data
    }
  }
