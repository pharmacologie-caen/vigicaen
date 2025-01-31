#' Add Dose in mg to a dataset
#'
#' @description `r lifecycle::badge('experimental')`
#' `add_dose()` creates dynamic columns representing drug doses (in mg)
#' for specified drugs in a dataset. It calculates daily dose values
#' based on dose amount, frequency, and their corresponding units.
#' The function is #' compatible with `demo`, `link`, and `adr` datasets.
#'
#' @details
#' The function identifies drug doses in a dataset by cross-referencing
#' with a drug data table. Drug codes can be specified using either
#' DrecNos or MedicinalProd_Id. Doses are filtered based on reputation
#' bases (suspect, concomitant, or interacting).
#' `d_code` is a named list containing drug codes.
#' Either drug record numbers (e.g., from [get_drecno()]), or
#' medicinalprod_ids (e.g., from [get_atc_code()]). Default method is to DrecNos.
#'
#' **Important:** Ensure the dataset's structure aligns with the `data_type`
#' argument to avoid errors.
#'
#' **Note:** It is very important to check the results obtained, as coding problems
#' are very frequent for posology data. Some results might appear unbelievable due
#' to these issues and should be carefully reviewed and trimmed accordingly.
#'
#' @param .data The dataset used to identify individual reports (usually, it is `demo`)
#' @param d_code A named list of drug codes (DrecNos or MPI). See Details.
#' @param d_names A character vector. Names for drug columns (must be the same length as d_code), default to `names(d_code)`
#' @param repbasis Suspect, interacting and/or concomitant.
#' Type initial of those you wish to select ("s" for suspect, "c" for concomitant
#' and "i" for interacting ; default to all, e.g. "sci").
#' @param method A character string. The type of drug code (DrecNo or MedicinalProd_Id). See details.
#' @param drug_data A data.frame containing the drug data (usually, it is `drug`)
#'
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
#'   drug_data = drug
#' )
#'
#' # Example: Restricting to "suspect" reputation base
#' demo_suspect <- add_dose(
#'   .data = demo,
#'   d_code = d_code,
#'   d_names = "paracetamol_suspected",
#'   repbasis = "s",
#'   method = "DrecNo",
#'   drug_data = drug
#' )


###########

add_dose <-
  function(.data,
           d_code,
           d_names = names(d_code),
           repbasis = "sci",
           method = c("DrecNo", "MedicinalProd_Id"),
           drug_data
  )
  {
    method <- rlang::arg_match(method)

    check_data_drug(drug_data, "drug_data")

    data_type <-
      query_data_type(.data, ".data")

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


    # Add dynamic dose columns to .data
    for (i in seq_along(d_code)) {
      drug_data_t <- t_ids[[i]]
      drug_name <- paste0(d_names[i], "_dose_mg_per_day")
      .data <- .data |>
        dplyr::left_join(drug_data_t, by = c("UMCReportId" = "t_id")) |>
        dplyr::mutate(!!drug_name := ifelse(is.na(dose_mg_per_day), NA_real_, dose_mg_per_day)) |>
        dplyr::select(-dose_mg_per_day) # Remove intermediate column
    }

    # Count the number of rows with a valid dose in mg/day for each drug
    dose_counts <- purrr::map_dfr(d_names, ~ {
      drug_col <- paste0( .x, "_dose_mg_per_day")
      count <- sum(!is.na(.data[[drug_col]]))
      data.frame(drug = .x, count = count)
    })

    # Display results
    if (sum(dose_counts$count) == 0) {
      cli::cli_alert_danger("No dose data in mg/day were found for any drug (other schemas not supported in add_dose()).")
    } else {
      cli::cli_alert_info("Number of lines with a posology in mg/day found per drug:")
      # Display each drug with its corresponding count
      purrr::walk(dose_counts$drug, ~{
        count <- dose_counts$count[dose_counts$drug == .]
        cli::cli_alert_info("{.x}: {count} lines with a posology in mg/day")
      })
    }

    # Display a message about checking results and trimming
    cli::cli_alert_info("Important: Please check the results for posology, as coding issues are common. Some results may seem unbelievable and should be carefully reviewed and trimmed.")

    dose_cols <- .data %>% dplyr::select(contains("dose_mg_per_day"))

    # Check if any of the columns have non-NA values
    if (any(purrr::map_lgl(dose_cols, ~ any(!is.na(.))))) {
      cli::cli_alert_info("Summary of added dose columns:")
      print(summary(dose_cols))
    }

    # Return final data
    .data
  }
