#' Extract of subset of Vigibase
#'
#' This is a subsetting function
#'
#' You must select a subset variable with `subset_var` and provide an appropriate list according to this variable in `sv_selection`.
#' Available `subset_var` :
#' \itemize{
#'   \item `drecno` will use Drug Record Number (DrecNo), from WHO Drug, and will subset from `drug`. See \code{\link{get_drecno}}.
#'   \item `medprod_id` will use MedicinalProd_Id, also from `drug`. May be useful if requesting from ATC classes. See \code{\link{get_atc_code}}.
#'   \item `meddra_id` will use MedDRA_Id, subset from `adr`. See \code{\link{get_llt_soc}} or \code{\link{get_llt_smq}}.
#'   \item `age` will use AgeGroup from `demo`. See below.
#' }
#' Age groups are as follows:
#' \itemize{
#'   \item 1 0 - 27 days
#'   \item 2 28 days to 23 months
#'   \item 3 2 - 11 years
#'   \item 4 12 - 17 years
#'   \item 5 18 - 44 years
#'   \item 6 45 - 64 years
#'   \item 7 65 - 74 years
#'   \item 8 >= 75 years
#'   \item 9 Unknown
#' }
#'
#' @param wd_in Source directory pathway (character)
#' @param wd_out Output directory pathway (character)
#' @param subset_var One of `"drecno"`, `"medprod_id"`, `"meddra_id"`, `"age"`
#' @param sv_selection A vector containing the appropriate type of data (according to the method, see details)
#' @param rm_suspdup A logical. Should suspected duplicates be removed? TRUE by default
#' @keywords vigibase subset custom
#' @export
#' @examples
#' ## Extract all colitis cases
#'
#' # Locate your input directory
#' # wd_in <- "/your_main_vigibase_files/"
#' # Choose a (close) directory to export to (it doesnt have to exist before you call it)
#' # wd_out <- "/your_main_vigibase_files/this_place_is_fine/"
#'
#' # For the example, we create tables in a directory that should be replaced by
#' # your own directory containing the entire database.
#'
#' wd_in <- tempdir()
#'
#' # --- technical steps ---- #
#' suspdup <-
#'   data.table(
#'     UMCReportId = c(17658707, 17658774, 17658793, 17658806),
#'     SuspectedduplicateReportId = c(17658708, 17658775, 17658794, 17658807)
#'   )
#'
#'  datasets <- rlang::list2(
#'    demo = demo_,
#'    drug = drug_,
#'    adr  = adr_,
#'    link = link_,
#'    srce = srce_,
#'    ind  = ind_,
#'    out  = out_,
#'    followup = followup_,
#'    suspectedduplicates = suspdup
#' )
#'
#'    purrr::iwalk(
#'      datasets,
#'      function(dataset, name)
#'        fst::write_fst(
#'          dataset,
#'          path = paste0(wd_in, "\\", name, ".fst")
#'        )
#'
#'    )

#' # back to tb_custom, you should select a subset_var and corresponding data
#'
#' # Subset on adr colitis codes
#' sv_selection <-
#'   ex_$a_llt$a_colitis
#'
#' wd_out <- paste0(wd_in, "\\", "colitis_subset", "\\")
#'
#' tb_custom(wd_in, wd_out,
#'           subset_var = "meddra_id",
#'           sv_selection = sv_selection)
#'
#'  # Subset on drug codes
#'
#'  sv_selection <-
#'     ex_$d_drecno$nivolumab
#'
#' wd_out <- paste0(wd_in, "\\", "nivolumab_subset", "\\")
#'
#' tb_custom(wd_in, wd_out,
#'           subset_var = "drecno",
#'           sv_selection = sv_selection)
#'
#'  # Subset on age > 65 year-old
#'
#'  sv_selection <-
#'     c(7, 8)
#'
#' wd_out <- paste0(wd_in, "\\", "more_than_65_subset", "\\")
#'
#' tb_custom(wd_in, wd_out,
#'           subset_var = "age",
#'           sv_selection = sv_selection)

tb_custom <-
  function(wd_in,
           wd_out,

           subset_var = c("drecno", "medprod_id", "meddra_id", "age"),
           sv_selection, # > 65 yo

           rm_suspdup = TRUE
  ){

    # supress notes for no visible binding for these variables
    UMCReportId <- NULL
    Drug_Id <- NULL

    subset_var <- match.arg(subset_var)

    if(!dir.exists(wd_in)){
      stop(paste0(wd_in, " was not found, check spelling and availability."))
    }


    if(!dir.exists(wd_out)){
      dir.create(wd_out)
    }

    # Subset variable

    sv <- switch(
      subset_var,
      drecno = "DrecNo",
      medprod_id = "MedicinalProd_Id",
      meddra_id = "MedDRA_Id",
      age = "AgeGroup"
    )

    sv_sym <- rlang::sym(sv)

    # Subset data.frame

    sv_df_name <- switch(
      subset_var,
      drecno = "drug",
      medprod_id = "drug",
      meddra_id = "adr",
      age = "demo"
    )

    sv_df_sym <- rlang::sym(sv_df_name)

    # rm_suspdup condition
    suspdup_list <-
      if(rm_suspdup){
        dt_fst(wd_in, "suspectedduplicates")[["SuspectedduplicateReportId"]]
      }

    # First step import

    drug <- dt_fst(wd_in, "drug")
    adr  <- dt_fst(wd_in, "adr")
    demo <- dt_fst(wd_in, "demo")

    # Creer le subset a partir de drug ou adr (ou demo pour age) et obtenir UMCreportID et DrugId

    subset_expr <-
      rlang::expr(`[`(
        !!sv_df_sym,
        !!sv_sym %in% sv_selection &
          !(UMCReportId %in% suspdup_list)
        )
      )

    sv_df <- eval(subset_expr) # 20211021 shall NOT be used as export, as it misses non of-interest drug or adr lines (as compared to drug or adr restricted to umc list)

    umc_subset <-
      sv_df[, unique(UMCReportId)]

    drug_id_subset <- # No need to extract Adr_Id, you need drug_id for table ind
      drug[UMCReportId %in% umc_subset, unique(Drug_Id)]


    # UMCReportId restricted datasets

    urd <- function(fst_file, restrict_list, wd_in, wd_out) {

      # supress notes for no visible binding for these variables
      UMCReportId <- NULL
      Drug_Id <- NULL

      df <-
        if (grepl("adr", fst_file)) {
          adr
        } else {
          dt_fst(wd_in, fst_file)
        }

      df <- df[UMCReportId %in% restrict_list]

      nr <- nrow(df)

      writeLines(
        paste0(fst_file, " subset has ", nr, " rows.")
      )

      write_fst(df, path = paste0(wd_out, fst_file))
    }


    urd_list <-
      paste0(
        c("demo", "adr", "out", "srce", "followup",
          if(!rm_suspdup) {"suspectedduplicates"}),
        ".fst")

    purrr::walk(urd_list, urd, restrict_list = umc_subset, wd_in, wd_out)

    # Drug_Id restricted datasets

    drd <- function(fst_file, restrict_list, wd_in, wd_out) {

      # supress notes for no visible binding for these variables
      UMCReportId <- NULL
      Drug_Id <- NULL

      df <-
        if (grepl("drug", fst_file)) {
          drug
        } else {
          dt_fst(wd_in, fst_file)
        }

      df <- df[Drug_Id %in% restrict_list]

      nr <- nrow(df)

      writeLines(
        paste0(fst_file, " subset has ", nr, " rows.")
      )

      write_fst(df, path = paste0(wd_out, fst_file))
    }

    drd_list <-
      paste0(
        c("drug", "link", "ind"),
        ".fst")

    purrr::walk(drd_list, drd, restrict_list = drug_id_subset, wd_in, wd_out)

  }
