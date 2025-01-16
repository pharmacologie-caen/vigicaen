#' Add Dose Information to a Dataset (Tidyverse Syntax)
#'
#' @description `r lifecycle::badge('stable')`
#' `add_dose()` creates dynamic columns representing drug doses (in mg)
#' for specified drugs in a dataset. It calculates daily dose values
#' based on dose amount, frequency, and their corresponding units.
#'
#' @details
#' The function identifies drug doses in a dataset by cross-referencing
#' with a drug data table. Drug codes can be specified using either
#' DrecNos or MedicinalProd_Id. Doses are filtered based on reputation
#' bases (suspect, concomitant, or interacting). The function is
#' compatible with `demo`, `link`, and `adr` datasets.
#'
#' **Important:** Ensure the dataset's structure aligns with the `data_type`
#' argument to avoid errors.
#'
#' @param .data A dataset used to identify individual reports (typically `demo`).
#' @param d_code A named list of drug codes (DrecNos or MedicinalProd_Id).
#' Each list element should correspond to a drug.
#' @param d_names A character vector of names for the drug columns
#' (defaults to `names(d_code)`).
#' @param repbasis A character string indicating the reputation bases to include:
#' - "s" for suspect
#' - "c" for concomitant
#' - "i" for interacting
#' Defaults to "sci" (all reputation bases).
#' @param method The type of drug code provided. Either "DrecNo" or
#' "MedicinalProd_Id".
#' @param drug_data A data frame containing drug data (typically `drug`).
#' @param data_type A character string indicating the dataset type:
#' - "demo" for a demographics dataset
#' - "link" for a linkage dataset
#' - "adr" for an adverse drug reaction dataset
#'
#' Defaults to "demo".
#'
#' @keywords data_management drug doses
#' @export
#' @importFrom rlang .data .env
#' @seealso [add_drug()], [get_drecno()], [get_atc_code()]
#' @examples
#' # Example: Adding doses for paracetamol
#' d_code <- list(paracetamol = c("DRECNO_001", "DRECNO_002"))
#' demo_updated <- add_dose(
#'   .data = demo,
#'   d_code = d_code,
#'   d_names = "paracetamol",
#'   repbasis = "sci",
#'   method = "DrecNo",
#'   drug_data = drug,
#'   data_type = "demo"
#' )
#'
#' # Example: Restricting to "suspect" reputation base
#' demo_suspect <- add_dose(
#'   .data = demo,
#'   d_code = d_code,
#'   d_names = "paracetamol_suspected",
#'   repbasis = "s",
#'   method = "DrecNo",
#'   drug_data = drug,
#'   data_type = "demo"
#' )


###########

add_dose <-
  function(.data,
           d_code,
           d_names = names(d_code),
           repbasis = "sci",
           method = c("DrecNo", "MedicinalProd_Id"),
           drug_data,
           data_type = c("demo", "link", "adr")
  )
  {
    method <- match.arg(method)
    data_type <- match.arg(data_type)

    # Validate data type and dataset structure
    if(data_type == "demo" &&
       any(c("Drug_Id", "Adr_Id") %in% names(.data))){
      stop("The dataset has Drug_Id or Adr_Id columns (like a `link` dataset). Yet data_type is set to `demo`. Please set data_type to `link` or use a `demo` dataset")
    } else if(data_type == "link" &&
              !all(c("Drug_Id", "Adr_Id") %in% names(.data))){
      stop("The dataset does not have Drug_Id and Adr_Id columns, (as a `link` dataset would). Yet data_type is set to `link`. Please set data_type to `demo` or use a `link` dataset")
    } else if(data_type == "adr" &&
              !all(c("Adr_Id", "MedDRA_Id", "Outcome") %in% names(.data))){
      stop("The dataset does not have Adr_Id, MedDRA_Id, and/or Outcome columns, (as an `adr` dataset would). Yet data_type is set to `adr`. Please set data_type accordingly.")
    }

    basis_sel <- c(
      if (grepl("s", repbasis)) { 1 },
      if (grepl("c", repbasis)) { 2 },
      if (grepl("i", repbasis)) { 3 }
    )

    dd_rb <- drug_data |>
      dplyr::filter(.data$Basis %in% basis_sel)

    renamer_did <- c("did_col" = method)
    dd_rb <- dd_rb |> dplyr::rename(dplyr::all_of(renamer_did))

    t_id <- switch(data_type,
                   demo = "UMCReportId",
                   adr  = "UMCReportId",
                   link = "Drug_Id"
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
          daily_dose_in_mg = Amount * multiplicator_amount * Frequency * multiplicator_frequency
        ) |>
        dplyr::filter(!is.na(daily_dose_in_mg)) |>
        dplyr::group_by(t_id) |>
        dplyr::slice_max(daily_dose_in_mg, with_ties = FALSE) |>
        dplyr::ungroup()|>
        dplyr::select(t_id, daily_dose_in_mg) # Only keep relevant columns
    })

    # Add dynamic dose columns to .data
    for (i in seq_along(d_code)) {
      drug_data_t <- t_ids[[i]]
      drug_name <- paste0("doses_", names(d_code)[i], "_in_mg")
      .data <- .data |>
        dplyr::left_join(drug_data_t, by = c("UMCReportId" = "t_id")) |>
        dplyr::mutate(!!drug_name := coalesce(daily_dose_in_mg, 0)) |>
        dplyr::select(-daily_dose_in_mg) # Remove intermediate column
    }

    # Return final data
    .data
  }
