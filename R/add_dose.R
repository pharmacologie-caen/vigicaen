#' Add Dose in mg to a dataset
#'
#' @description `r lifecycle::badge('experimental')`
#' `add_dose()` creates drug dose columns in vigibase
#' datasets (demo, link, adr, drug, ind)
#' for specified drugs in a dataset. It calculates daily dose values
#' based on dose amount, frequency, and their corresponding units.
#' The function is compatible with `demo`, `link`, `adr`, `drug` and `ind`
#'  datasets.
#'
#' @details
#' Currently, only drug doses in **mg per day** are handled.
#' The function identifies drug doses in a dataset by cross-referencing
#' with a drug data table. Drugs may be filtered based on reputation
#' bases (suspect, concomitant, or interacting).
#' Either drug record numbers (e.g., from [get_drecno()]), or
#' medicinalprod_ids (e.g., from [get_atc_code()]) can be used to
#' identify drugs. Default method is to DrecNos.
#'
#'
#' **Note:** It is very important to check the results obtained, as coding problems
#' are very frequent for dose data, and some results might appear unreliable.
#'
#' @param .data The dataset used to identify individual reports (usually, it is `demo`)
#' @param d_code A named list of drug codes (DrecNos or MPI). See Details.
#' @param d_dose_names A character vector. Names for drug dose
#' columns (must be the same length as d_code), default to `names(d_code)`.
#' Will be followed by a fixed suffix "_dose_mg_per_day".
#' @param repbasis Suspect, interacting and/or concomitant.
#' Type initial of those you wish to select ("s" for suspect, "c" for concomitant
#' and "i" for interacting ; default to all, e.g. "sci").
#' @param method A character string.
#' The type of drug code (DrecNo or MedicinalProd_Id). See details.
#' @param drug_data A data.frame containing the drug data (usually, it is `drug`)
#'
#'
#' @keywords data_management drug doses
#' @export
#' @importFrom rlang .data .env
#' @seealso [add_drug()], [get_drecno()], [get_atc_code()]
#' @examples
#' # Example: Adding doses for paracetamol
#' d_code <- list(paracetamol = c(97818920, 97409107))
#' demo <-
#'   add_dose(
#'     .data = demo_,
#'     d_code = d_code,
#'     d_dose_names = "paracetamol",
#'     drug_data = drug_
#'     )
#'
#'  desc_facvar(demo, "paracetamol_dose_mg_per_day")
#'
#' # Use only drug dose where paracetamol had a "suspect" reputation base.
#' demo <-
#'   add_dose(
#'     .data = demo_,
#'     d_code = d_code,
#'     d_dose_names = "para_susp",
#'     repbasis = "s",
#'     drug_data = drug_
#'   )
#'
#'   desc_facvar(demo, "para_susp_dose_mg_per_day")


###########

add_dose <-
  function(.data,
           d_code,
           d_dose_names = names(d_code),
           repbasis = "sci",
           method = c("DrecNo", "MedicinalProd_Id"),
           drug_data
  )
  {

    check_id_list_numeric(d_code)
    method <- rlang::arg_match(method)
    check_data_drug(drug_data, "drug_data")

    data_type <-
      query_data_type(.data, ".data")

    basis_sel <- c(
      if (grepl("s", repbasis)) { 1 },
      if (grepl("c", repbasis)) { 2 },
      if (grepl("i", repbasis)) { 3 }
    )

    d_d_names_full <-
      paste0(d_dose_names, "_dose_mg_per_day")


    dd_rb <- drug_data |>
      dplyr::filter(.data$Basis %in% basis_sel)

    renamer_did <- c("did_col" = method)
    dd_rb <- dd_rb |> dplyr::rename(dplyr::all_of(renamer_did))

    # identify table_ids to collect

    t_id <-
      switch(data_type,
             demo = "UMCReportId",
             adr  = "UMCReportId",
             link = "Drug_Id",
             drug = "Drug_Id",
             ind  = "Drug_Id"
      )

    renamer_tid <- c("t_id" = t_id)
    dd_rb <- dd_rb |> dplyr::rename(dplyr::all_of(renamer_tid))

    # Collect table_ids with doses
    t_ids <- purrr::map(d_code, function(d_code_batch) {
      dd_rb |>
        dplyr::filter(.data$did_col %in% d_code_batch) |>
        dplyr::select(t_id, Amount, AmountU, Frequency, FrequencyU) |>
        dplyr::mutate(
          AmountU = gsub("\\s+", "", AmountU),
          FrequencyU = gsub("\\s+", "", FrequencyU),
          Amount = gsub("\\s+", "", Amount),
          Frequency = gsub("\\s+", "", Frequency)
        ) |>
        dplyr::filter(Amount != "-") |>
        dplyr::filter(trimws(AmountU) %in% c("1", "2", "3", "4", "5", "6")) |>
        dplyr::filter(Frequency != "-") |>
        dplyr::filter(trimws(FrequencyU) %in% c("801", "802", "803", "804", "805", "806")) |>
        dplyr::filter(Frequency != 0) |>
        dplyr::mutate(
          Amount = as.numeric(Amount),
          Frequency = as.numeric(Frequency),
          multiplicator_amount = dplyr::case_when(
            AmountU == "1" ~ 1000000,
            AmountU == "2" ~ 1000,
            AmountU == "3" ~ 1,
            AmountU == "4" ~ 1 / 1000,
            AmountU == "5" ~ 1 / 1000000,
            AmountU == "6" ~ 1 / 1000000000,
            TRUE ~ NA_real_
          ),
          multiplicator_frequency = dplyr::case_when(
            FrequencyU == "806" ~ 1440,
            FrequencyU == "805" ~ 24,
            FrequencyU == "804" ~ 1,
            FrequencyU == "803" ~ 1 / 7,
            FrequencyU == "802" ~ 1 / 30,
            FrequencyU == "801" ~ 1 / 365.25,
            TRUE ~ NA_real_
          ),
          dose_mg_per_day = (Amount * multiplicator_amount * multiplicator_frequency * Frequency
          )) |>
        dplyr::filter(!is.na(dose_mg_per_day)) |>
        dplyr::group_by(t_id) |>
        dplyr::slice_max(dose_mg_per_day, with_ties = FALSE) |>
        dplyr::ungroup()|>
        dplyr::select(t_id, dose_mg_per_day) # Only keep relevant columns
    })


    e_l <-
      t_ids |>
      purrr::map(function(t_id_subset) {
        # Create a lookup vector for t_id to dose
        lookup <- setNames(t_id_subset$dose_mg_per_day, t_id_subset$t_id)
        # Return a quosure that performs the lookup
        rlang::quo(
          lookup[as.character(t_id)]
        ) |>
          rlang::quo_set_env(
            new.env(parent = rlang::caller_env()) |>
              { \(e) { e$lookup <- lookup; e } }()
          )
      }) |>
      rlang::set_names(d_d_names_full)

    # Prepare destination table with renamed columns
    dest_data <- .data |> dplyr::rename(dplyr::all_of(renamer_tid))

    # Add new columns using the quosures
    dest_data_withcols <- dest_data |> dplyr::mutate(!!!e_l)

    # Count the number of rows with a valid dose in mg/day for each drug
    dose_counts <-
      d_dose_names |>
      rlang::set_names() |>
      purrr::map( ~ {
        drug_col <- paste0( .x, "_dose_mg_per_day")
        sum(!is.na(dest_data_withcols[[drug_col]]))
        })

    drug_with_dose_data <-
      dose_counts |>
      purrr::keep(~ .x > 0)

    drug_without_dose_data <-
      dose_counts |>
      purrr::discard(~ .x > 0)

    # booleans to check if there is any of each cases (drugs with/without data)
    any_with_dose <-
      drug_with_dose_data |> purrr::map(function(x)
        ! is.null(x)) |>
      unlist() |> any()

    any_no_dose <-
      drug_without_dose_data |> purrr::map(function(x)
        ! is.null(x)) |>
      unlist() |> any()



    # Display results
    if (any_no_dose) {
      msg_addind_no_match(drug_without_dose_data)
    }

    if(any_with_dose) {
      msg_addind_match(drug_with_dose_data)
    }

    # Check if any of the columns have non-NA values
    if (any_with_dose) {
      cli::cli_alert_info("Summary of added dose columns:")
      print(desc_cont(dest_data_withcols,
                      paste0(names(drug_with_dose_data), "_dose_mg_per_day"))
      )
    }

    # compute everything (this is strictly required only for arrow objects)

    if(any(c("Table", "Dataset") %in% class(.data))){
      return(dest_data_withcols |>
        dplyr::compute()
      )
    } else {
      return(dest_data_withcols)
    }

  }


# Helpers --------------------


msg_addind_no_match <-
  function(drug_without_dose_data
  ){

    res_list_no_match_compact <-
      purrr::compact(drug_without_dose_data)

    msg_no_match <-
      function() {

        cli_par()

        cli_h3(paste0(col_red("x"), " No drug dose found in mg/day"))

        cli_end()
        cli_par()

        lid <- cli_ul()
        for (i in seq_along(res_list_no_match_compact)) {
          cli_li(paste0(
            '{.code {names(res_list_no_match_compact)[i]}}',
            ''
          ))

        }

        cli_end(lid)

        cli_par()

        cli_alert_info(
          "Other posology schemas are not supported in add_dose())."
        )
        cli_end()
      }

    msg_no_match()
  }

msg_addind_match <-
  function(drug_with_dose_data){

    res_list_match_compact <-
      purrr::compact(drug_with_dose_data)

    msg_match <-
      function() {

        cli_par()

        cli_h3(paste0(col_green("{symbol$tick}"), " Drug dose found in mg/day"))

        cli_end()
        cli_par()

        lid <- cli_ul()
        for (i in seq_along(res_list_match_compact)) {
          cli_li(paste0(
            '{.code {names(res_list_match_compact)[i]}}: ',
            '{res_list_match_compact[i]} rows',
            ''
          ))

        }

        cli_end(lid)

        # Display a message about checking results and trimming
        cli::cli_alert_info("Important: Please check the results for posology,
                        as coding issues are common.
                        Some results may seem unreliable.")
      }

    msg_match()
  }
