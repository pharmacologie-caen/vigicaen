#' @title Add DRUG and ADR columns
#'
#' @description
#' `add_drug_adr()` adds both drug and adverse drug reaction (ADR) columns to a dataset.
#'
#' @details
#' This function combines the functionalities of `add_drug()` and `add_adr()` to create columns for drug and ADR information in a single operation. It allows users to add drug-related columns using drug codes (e.g., `DrecNos` or `MedicinalProd_Id`) and ADR-related columns using low-level term codes (e.g., `llt_codes`). It performs a left join to merge the resulting drug and ADR columns based on a common identifier (e.g., `UMCReportId`).
#'
#' The function first uses `add_drug()` to add the drug columns, then filters the resulting dataset to retain only relevant columns for merging. It then applies `add_adr()` to add the ADR columns, before merging the two datasets using the common `UMCReportId` column.
#'
#' @param .data The dataset used to identify individual reports (usually, it is `demo`).
#' @param d_code A named list of drug codes (e.g., `DrecNos` or `MedicinalProd_Id`).
#' @param d_names A character vector specifying names for drug columns (must be the same length as `d_code`). Defaults to `names(d_code)`.
#' @param drug_data A data.frame containing the drug data (usually, it is `drug`).
#' @param a_code A named list of low-level term codes (e.g., `llt_codes`) for ADRs.
#' @param a_names A character vector specifying names for ADR columns (must be the same length as `a_code`). Defaults to `names(a_code)`.
#' @param adr_data A data.frame containing the ADR data (usually, it is `adr`).
#' @param repbasis A character string indicating the type of drug report basis to include: "s" for suspect, "c" for concomitant, and "i" for interacting drugs. Defaults to "sci" (all types).
#' @param method A character string specifying the type of drug code, either "DrecNo" or "MedicinalProd_Id". Defaults to "DrecNo".
#' @param data_type A character string indicating the type of data to add columns to. Can be either "demo", "link", or "adr". Defaults to "demo".
#' @export
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @keywords data_management drug adr
#' @seealso [add_drug()], [add_adr()], [get_drecno()], [get_llt_soc()]
#' @examples
#' # Example usage:
#' # Combine both drug and ADR columns into a dataset `demo_`
#'
#' d_names <- rlang::list2(
#' nivolumab = "nivolumab",
#' pembrolizumab = "pembrolizumab"
#' )
#'
#' d_drecno <-
#'  d_names |>
#'  get_drecno(
#'    mp_short = mp_short_
#'  )
#'
#' a_llt <- ex_$a_llt
#'
#' demo_ <- add_drug_adr(
#'   .data = demo_,
#'   d_code = d_drecno,
#'   d_names = "nivolumab",
#'   drug_data = drug_,
#'   a_code = a_llt,
#'   a_names = "colitis",
#'   adr_data = adr_,
#'   repbasis = "sci",
#'   method = "DrecNo",
#'   data_type = "demo"
#' )
#'
#' # Check the merged dataset
#' check_dm(demo_, cols = c("nivolumab", "colitis"))
#' @name add_drug_adr

utils::globalVariables("UMCReportId")

add_drug_adr  <- function(.data,
                          d_code,
                          d_names = names(d_code),
                          drug_data,
                          a_code,
                          a_names = names(a_code),
                          adr_data,
                          repbasis = "sci",
                          method = c("DrecNo", "MedicinalProd_Id"),
                          data_type = c("demo", "link", "adr")) {

  final_data_drug  <- #just use add_drug
    add_drug(.data = .data,
             d_code = d_code,
             d_names = d_names,
             drug_data = drug_data,
             repbasis = repbasis,
             method = method,
             data_type = data_type)


  final_data_drug_filtered <- final_data_drug |> #filter to keep only the columns that will be useful when merging
    dplyr::select(UMCReportId, dplyr::all_of(as.character(d_names)))


  final_data_adr <- #just use add_adr
    add_adr(.data = .data,
            a_code = a_code,
            a_names = a_names,
            adr_data = adr_data,
            data_type = data_type
    )


  # Merge final_data_drug and final_data_adr
  final_data <-  final_data_adr |>
    dplyr::left_join(final_data_drug_filtered, by = "UMCReportId") # or use the appropriate column to join

}

tmp_file <- tempfile()
on.exit(unlink(tmp_file), add = TRUE)
