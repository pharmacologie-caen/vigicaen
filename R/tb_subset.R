#' Extract of subset of Vigibase
#'
#' @description `r lifecycle::badge('stable')` Create a subset of the VigiBase
#' ECL datasets
#'
#' @details You must select a subset variable with `subset_var` and provide
#' an appropriate list according to this variable in `sv_selection`.
#' Available `subset_var` :
#' \itemize{
#'   \item `drecno` will use Drug Record Number (DrecNo), from WHO Drug, and will subset from `drug` (see [get_drecno()]).
#'   \item `medprod_id` will use MedicinalProd_Id, also from `drug`. May be useful if requesting from ATC classes. (see [get_atc_code()]).
#'   \item `meddra_id` will use MedDRA_Id, subset from `adr`. (see [get_llt_soc()] or See [get_llt_smq()]).
#'   \item `age` will use AgeGroup from `demo`. See below.
#' }
#' Age groups ids are as follows:
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
#' Example: To work with patients aged 18 to 74, provide `c(5, 6, 7)`
#' as `sv_selection`.
#'
#' Use [dt_parquet()] to load the tables afterward.
#'
#' @param wd_in Source directory pathway (character)
#' @param wd_out Output directory pathway (character)
#' @param subset_var One of `"drecno"`, `"medprod_id"`, `"meddra_id"`, `"age"`
#' @param sv_selection A named list or a vector containing the appropriate ids (according to the method, see details)
#' @param rm_suspdup A logical. Should suspected duplicates be removed? TRUE by default
#' @returns Parquet files in the output directory. All files from a
#' vigibase ECL main folder are returned subsetted
#' (including suspectedduplicates).
#'
#' @keywords dataset subset custom
#' @export
#' @seealso [get_drecno()], [get_atc_code()], [get_llt_soc()], [get_llt_smq()], [dt_parquet()]
#' @examples
#' ## Extract all colitis cases
#'
#' # Locate your input directory
#' # wd_in <- "../vigibase_ecl/main/"
#' # Choose a (close) directory to export to (it doesnt have to exist before you call it)
#' # wd_out <- "../vigibase_ecl/main/subset/"
#'
#' # For the example, we create tables in a directory that should be replaced by
#' # your own directory containing the entire database.
#'
#' wd_in <- tempdir()
#'
#' # --- technical steps ---- #
#'
#' library(data.table)
#'
#' mini_data <- rlang::list2(
#'   demo =
#'     data.table(
#'       UMCReportId = c(1, 2, 3, 4),
#'       AgeGroup = c(1, 2, 7, 9)
#'     ),
#'   drug =
#'     data.table(
#'       UMCReportId = c(1, 2, 3, 4),
#'       Drug_Id = c("d1", "d2", "d3", "d4"),
#'       DrecNo = c(133138448, 133138448, 111841511, 111841511),
#'       MedicinalProd_Id = c(25027716, 97354576, 104264760, 37484408)
#'     ),
#'   adr  =
#'     data.table(
#'       UMCReportId = c(1, 2, 3, 4),
#'       Adr_Id = c("a1", "a2", "a3", "a4"),
#'       MedDRA_Id = c(110049083, 31672047, 146319904, 72535511)
#'     ),
#'   link =
#'     data.table(
#'       Drug_Id = c("d1", "d2", "d3", "d4"),
#'       Adr_Id = c("a1", "a2", "a3", "a4")
#'     ),
#'   srce =
#'     data.table(
#'       UMCReportId = c(1, 2, 3, 4)
#'     ),
#'   ind  =
#'     data.table(
#'       Drug_Id = c("d1", "d2", "d3", "d4")
#'     ),
#'   out  =
#'     data.table(
#'       UMCReportId = c(1, 2, 3, 4)
#'     ),
#'   followup =
#'     data.table(
#'       UMCReportId = c(1, 2, 3, 4)
#'     ),
#'   suspdup =
#'     data.table(
#'       UMCReportId = c(3),
#'       SuspectedduplicateReportId = c(4)
#'     )
#' )
#'
#'   purrr::iwalk(
#'     mini_data,
#'     function(dataset, name)
#'       arrow::write_parquet(
#'         dataset |>
#'           arrow::as_arrow_table(),
#'         sink = paste0(wd_in, "/", name, ".parquet")
#'       )
#'
#'   )
#'
#' # back to tb_subset, you should select a subset_var and corresponding data
#'
#' # Subset on adr colitis codes
#'
#' adr_llt <-
#'  list(
#'    colitis = "Colitis"
#'    ) |>
#'    get_llt_soc(term_level = "pt", meddra_)
#'
#'
#' wd_out <- paste0(wd_in, "/", "colitis_subset", "/")
#'
#' tb_subset(wd_in, wd_out,
#'           subset_var = "meddra_id",
#'           sv_selection = adr_llt)
#'
#' # Subset on drug codes
#'
#'  d_drecno <-
#'    list(
#'     ipi = "ipilimumab") |>
#'     get_drecno(mp = mp_)
#'
#' wd_out <- paste0(wd_in, "/", "nivolumab_subset", "/")
#'
#' tb_subset(wd_in, wd_out,
#'           subset_var = "drecno",
#'           sv_selection = d_drecno)
#'
#'  # Subset on age > 65 year-old
#'
#'  sv_selection <-
#'     c(7, 8)
#'
#' wd_out <- paste0(wd_in, "/", "more_than_65_subset", "/")
#'
#' tb_subset(wd_in, wd_out,
#'           subset_var = "age",
#'           sv_selection = sv_selection)

tb_subset <-
  function(wd_in,
           wd_out,

           subset_var = c("drecno", "medprod_id", "meddra_id", "age"),
           sv_selection, # > 65 yo

           rm_suspdup = TRUE
  ){

    # supress notes for no visible binding for these variables
    UMCReportId <- NULL
    Drug_Id <- NULL

    subset_var <- rlang::arg_match(subset_var)

    # check integer lists for meddra_id, medprod_id, and drecno

    if(subset_var %in% c("drecno", "medprod_id", "meddra_id")){
      check_id_list(sv_selection)

      check_id_list_numeric(sv_selection)
    }

    # in all cases, flatten the list
    sv_selection <-
      sv_selection |> unlist()

    if(!dir.exists(wd_in)){
      stop(paste0(wd_in, " was not found, check spelling and availability."))
    }

    if(!dir.exists(wd_out)){
      dir.create(wd_out)
    }

    # helps working with the "here" package, or tempdir

    if(!grepl("(/|\\\\)$", wd_in, perl = TRUE)){
      wd_in <-
        paste0(wd_in, "/")
    }

    if(!grepl("(/|\\\\)$", wd_out, perl = TRUE)){
      wd_out <-
        paste0(wd_out, "/")
    }


    # Subset variable

    sv <- switch(
      subset_var,
      drecno     = "DrecNo",
      medprod_id = "MedicinalProd_Id",
      meddra_id  = "MedDRA_Id",
      age        = "AgeGroup"
    )

    sv_sym <- rlang::sym(sv)

    sv_col <-
      sv |>
      rlang::set_names("sv_col")

    # Subset data.frame

    sv_df_name <- switch(
      subset_var,
      drecno     = "drug",
      medprod_id = "drug",
      meddra_id  = "adr",
      age        = "demo"
    )

    sv_df_sym <- rlang::sym(sv_df_name)

    # rm_suspdup condition
    suspdup_list <-
      if(rm_suspdup){
        arrow::read_parquet(
          paste0(wd_in, "suspdup.parquet"),
          as_data_frame = FALSE
        ) |>
          dplyr::pull(.data$SuspectedduplicateReportId,
                      as_vector = FALSE)
      }

    # First step import

    drug <- arrow::read_parquet(paste0(wd_in, "drug.parquet"), as_data_frame = FALSE)
    adr  <- arrow::read_parquet(paste0(wd_in, "adr.parquet"),  as_data_frame = FALSE)
    demo <- arrow::read_parquet(paste0(wd_in, "demo.parquet"), as_data_frame = FALSE)

    # Creer le subset a partir de drug ou adr (ou demo pour age) et obtenir UMCreportID et DrugId

    # first step, create the sv data

    sv_df <- rlang::eval_tidy(
      rlang::expr(!!sv_df_sym)
    ) |>
    # then rename subset var
      dplyr::rename(dplyr::all_of(sv_col)) |>
    # and use it to subset.
      dplyr::filter(.data$sv_col %in% .env$sv_selection &
                      !(.data$UMCReportId %in% suspdup_list)
      ) |>
      dplyr::compute()


    # subset_expr <-
    #   rlang::expr(`[`(
    #     !!sv_df_sym,
    #     !!sv_sym %in% sv_selection &
    #       !(UMCReportId %in% suspdup_list)
    #     )
    #   )
    #
    # sv_df <- eval(subset_expr) # 20211021 shall NOT be used as export, as it misses non of-interest drug or adr lines (as compared to drug or adr restricted to umc list)

    umc_subset <-
      sv_df |>
      dplyr::pull(unique(.data$UMCReportId),
                  as_vector = FALSE)

    drug_id_subset <- # No need to extract Adr_Id, you need drug_id for table ind
      drug |>
      dplyr::filter(.data$UMCReportId %in% .env$umc_subset) |>
      dplyr::pull(unique(.data$Drug_Id),
                  as_vector = FALSE)


    # UMCReportId restricted datasets

    urd <- function(parquet_file, restrict_list, wd_in, wd_out) {

      # supress notes for no visible binding for these variables
      UMCReportId <- NULL
      Drug_Id <- NULL

      df <-
        if (grepl("adr", parquet_file)) {
          adr
        } else {
          arrow::read_parquet(paste0(wd_in, parquet_file),
                              as_data_frame = FALSE)
        }

      df <-
        df |>
        dplyr::filter(.data$UMCReportId %in% .env$restrict_list) |>
        dplyr::compute()

      nr <- nrow(df)

      writeLines(
        paste0(parquet_file, " subset has ", nr, " rows.")
      )

      arrow::write_parquet(df, sink = paste0(wd_out, parquet_file))
    }


    urd_list <-
      paste0(
        c("demo", "adr", "out", "srce", "followup",
          if(!rm_suspdup) {"suspdup"}),
        ".parquet")

    purrr::walk(urd_list, urd, restrict_list = umc_subset, wd_in, wd_out)

    # Drug_Id restricted datasets

    drd <- function(parquet_file, restrict_list, wd_in, wd_out) {

      # supress notes for no visible binding for these variables
      UMCReportId <- NULL
      Drug_Id <- NULL

      df <-
        if (grepl("drug", parquet_file)) {
          drug
        } else {
          arrow::read_parquet(paste0(wd_in, parquet_file),
                              as_data_frame = FALSE)
        }

      df <-
        df |>
        dplyr::filter(.data$Drug_Id %in% .env$restrict_list) |>
        dplyr::compute()

      nr <- nrow(df)

      writeLines(
        paste0(parquet_file, " subset has ", nr, " rows.")
      )

      arrow::write_parquet(df, sink = paste0(wd_out, parquet_file))
    }

    drd_list <-
      paste0(
        c("drug", "link", "ind"),
        ".parquet")

    purrr::walk(drd_list, drd, restrict_list = drug_id_subset, wd_in, wd_out)

  }
