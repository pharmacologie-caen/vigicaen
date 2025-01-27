#' Add DRUG column(s) to a dataset (tidyverse syntax)
#'
#' @description `r lifecycle::badge('stable')` Creates drug columns.
#' in vigibase datasets (demo, link, adr, but also drug).
#'
#' @details `d_code` is a named list containing drug codes.
#' Either drug record numbers (e.g., from [get_drecno()]), or
#' medicinalprod_ids (e.g., from [get_atc_code()]). Default method is to DrecNos.
#'
#' @section Argument `repbasis`:
#' Drugs can be reported according to one of three reputation bases:
#' \itemize{
#'  \item `s` for suspect
#'  \item `c` for concomitant
#'  \item `i` for interacting
#'  }
#' in the occurrence of the adverse drug reaction. To study only one of these
#' reputation basis, type only the corresponding letter in `repbasis`,
#' e.g. "s" for suspects, or "si" for suspect **or** interacting.
#'
#' You can add drug identification to a `demo`, `link`, `adr` or even `drug`
#' dataset.(in this latter case, you must provide `adr` twice,
#' as `.data` and `drug_data`)
#'
#' @param .data The dataset used to identify individual reports (usually, it is `demo`)
#' @param d_code A named list of drug codes (DrecNos or MPI). See Details.
#' @param d_names A character vector. Names for drug columns (must be the same length as d_code), default to `names(d_code)`
#' @param method A character string. The type of drug code (DrecNo or MedicinalProd_Id). See details.
#' @param repbasis Suspect, interacting and/or concomitant.
#' Type initial of those you wish to select ("s" for suspect, "c" for concomitant
#' and "i" for interacting ; default to all, e.g. "sci").
#' @param drug_data A data.frame containing the drug data (usually, it is `drug`)
#' @param data_type `r lifecycle::badge('deprecated')`. Data_type is now detected
#' internally.
#' @keywords data_management drug
#' @export
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @seealso [add_adr()], [get_drecno()], [get_atc_code()]
#' @examples
#' # create a nivolumab column in demo_
#'
#' d_sel_names <- list(nivolumab = "nivolumab")
#'
#' d_drecno <- get_drecno(d_sel_names,
#'                         mp = mp_)
#' demo_ <-
#'   add_drug(
#'     .data = demo_,
#'     d_code = d_drecno,
#'     method = "DrecNo",
#'     repbasis = "sci",
#'     drug_data = drug_
#'   )
#'
#' # remember to assign the result to your actual demo dataset
#'
#' # do you want to work only with cases where nivolumab was a "suspected" drug?
#' # change argument repbasis to "s"
#'
#' demo_ <-
#'   add_drug(
#'     .data = demo_,
#'     d_code = d_drecno,
#'     d_names = "nivolumab_suspected",
#'     method = "DrecNo",
#'     repbasis = "s",
#'     drug_data = drug_
#'   )
#'
#' check_dm(demo_, cols = c("nivolumab", "nivolumab_suspected"))

add_drug <-
  function(.data,
           d_code,
           d_names = names(d_code),
           repbasis = "sci",
           method = c("DrecNo", "MedicinalProd_Id"),
           drug_data,
           data_type = deprecated()
  )
  {

    check_id_list_numeric(d_code)

    method <- rlang::arg_match(method)

    check_data_drug(drug_data, "drug_data")

    # Check if user has supplied `data_type`.
    if (lifecycle::is_present(data_type)) {

      # Signal the deprecation to the user
      lifecycle::deprecate_soft(
        when = "0.14.1",
        what = "add_drug(data_type)",
        details = c("i" = "data_type is now internally detected")
      )
    }

    data_type <-
      query_data_type(.data, ".data")

    basis_sel <-
      c(
        if(grepl("s", repbasis)){ 1 },
        # subsidiary_files / Repbasis_Lx
        if(grepl("c", repbasis)){ 2 },
        if(grepl("i", repbasis)){ 3 }
      )

    dd_rb <-
      drug_data |>
      dplyr::filter(.data$Basis %in% basis_sel)

    # match id_col to method

    renamer_did <- c("did_col" = method)

    dd_rb <-
      dd_rb |>
      dplyr::rename(dplyr::all_of(renamer_did))

    # identify table_ids to collect

    t_id <-
      switch(data_type,
             demo = "UMCReportId",
             adr  = "UMCReportId",
             link = "Drug_Id",
             drug = "Drug_Id"
      )

    renamer_tid <-
      c("t_id" = t_id)

    dd_rb <-
      dd_rb |>
      dplyr::rename(dplyr::all_of(renamer_tid))

    # collect table_ids

    t_ids <-
      purrr::map(d_code, function(d_code_batch){
        if(any(c("Table", "Dataset") %in% class(.data))){
          dd_rb |>
            dplyr::filter(.data$did_col %in% d_code_batch) |>
            dplyr::pull(.data$t_id, as_vector = FALSE)
          } else {
            dd_rb |>
              dplyr::filter(.data$did_col %in% d_code_batch) |>
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
      rlang::set_names(d_names)

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
