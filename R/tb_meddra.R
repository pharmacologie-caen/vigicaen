#' Create MedDRA tables
#'
#' @description `r lifecycle::badge('stable')` Transform MedDRA .ascii
#' files to .parquet files
#'
#' MedDRA is delivered as ascii files, that you should
#' transform to a more efficient format. Parquet format from arrow has many advantages:
#' It works with out-of-memory data, which makes it possible to process tables on
#' a computer with not-so-much RAM. It is also lightweighted and standard across different
#' langages.
#' The function also creates variables in each table.
#' You should note that NOT all MedDRA tables are processed with this function.
#' Three tables are created: `meddra_hierarchy`, that respects the System Organ
#' Class hierarchic classification. `smq_list` and `smq_content` for Standardized
#' MedDRA Queries.
#' **Caution** There tends to be small variations in the MedDRA ascii files structure.
#' Last verified version on which this function is working is **26.1**.
#' Use [dt_parquet()] to load the tables afterward.
#'
#' @param path_meddra Character string, a directory containing MedDRA ascii tables.
#' It is also the output directory.
#'
#' @importFrom utils read.table
#' @keywords import meddra
#' @seealso [tb_vigibase()], [tb_who()], [tb_subset()], [dt_parquet()]
#'
#' @export
#'
#' @return .parquet files into the `path_meddra` directory.
#' Three tables: `meddra_hierarchy`, `smq_list`, and `smq_content`.
#' Some columns are returned as `integer` (all `*_code` columns).
#' All other columns are `character`.
#'
#' @examples
#'
#' # Use the examples from tb_main if you want to see these functions in action.
#'
#' path_meddra <- "/meddra_directory/"
#'
#' ## NOT RUN ##
#' # tb_meddra(path_meddra = path_meddra)
#'


tb_meddra <-
  function(path_meddra){

    cli::cli_h1(
      "tb_meddra()"
    )
    cli::cli_alert_info(
      "Creating MedDRA tables.")

    msg_tb_onceperdatabase()

    cli_progress_bar(
      "Creating MedDRA",
      format = "{cli::pb_bar} {cli::pb_percent} | {cli::pb_elapsed} | {cli::pb_status}",
      total = 100
    )

    # ---- llt.asc ---- ####
    cli_progress_update(force = TRUE,
                        status = "Read llt.asc",
                        set = 15)

    llt_table <- read.table(paste0(path_meddra, "llt.asc"),
                            sep = "$",
                            quote = "",
                            fill = FALSE,
                            col.names = c("llt_code",
                                          "llt_name",
                                          "pt_code",
                                          "llt_whoart_code",
                                          "llt_harts_code",
                                          "llt_costart_sym",
                                          "llt_icd9_code",
                                          "llt_icd9cm_code",
                                          "llt_icd10_code",
                                          "llt_currency",
                                          "llt_jart_code",
                                          "empty_col"))

    # ---- mdhier.asc ---- ####
    cli_progress_update(force = TRUE,
                        status = "Read mdhier.asc",
                        set = 30)

    med_hierarchy <- read.table(paste0(path_meddra, "mdhier.asc"),
                                sep = "$",
                                quote = "",
                                fill = FALSE,
                                col.names = c("pt_code",
                                              "hlt_code",
                                              "hlgt_code",
                                              "soc_code",
                                              "pt_name",
                                              "hlt_name",
                                              "hlgt_name",
                                              "soc_name",
                                              "soc_abbrev",
                                              "null_field",
                                              "pt_soc_code",
                                              "primary_soc_fg",
                                              "empty_col"
                                ))


    med_hierarchy_llt <-
      llt_table |>
      dplyr::select(dplyr::all_of(c("llt_code", "llt_name", "pt_code"))) |>
      dplyr::left_join(med_hierarchy, by = "pt_code")

    # ---- write
    cli_progress_update(force = TRUE,
                        status = "Write meddra_hierarchy.parquet",
                        set = 45)

    arrow::write_parquet(med_hierarchy_llt,
                         sink = paste0(path_meddra, "meddra_hierarchy.parquet")
    )

    # SMQ #### ####

    # ---- smq_list.asc ---- ####
    cli_progress_update(force = TRUE,
                        status = "Read smq_list.asc",
                        set = 60)

    smq_list <- read.table(paste0(path_meddra, "smq_list.asc"),
                           sep = "$",
                           quote = "",
                           comment.char = "",
                           fill = FALSE,
                           col.names = c("smq_code",
                                         "smq_name",
                                         "smq_level",
                                         "smq_description",
                                         "smq_source",
                                         "smq_note",
                                         "MedDRA_version",
                                         "status",
                                         "smq_algorithm",
                                         "empty_col"))


    # ---- write
    cli_progress_update(force = TRUE,
                        status = "Write smq_list.parquet",
                        set = 70)

    arrow::write_parquet(smq_list,
                         sink = paste0(path_meddra, "smq_list.parquet")
    )

    # ---- smq_content.asc ---- ####
    cli_progress_update(force = TRUE,
                        status = "Read smq_content.asc",
                        set = 80)

    smq_content <- read.table(paste0(path_meddra, "smq_content.asc"),
                              sep = "$",
                              quote = "",
                              comment.char = "",
                              fill = FALSE,
                              col.names = c("smq_code",
                                            "term_code",
                                            "term_level",
                                            "term_scope",
                                            "term_category",
                                            "term_weight",
                                            "term_status",
                                            "term_addition_version",
                                            "term_last_modified_version",
                                            "empty_col"))

    # ---- write
    cli_progress_update(force = TRUE,
                        status = "Write smq_content.parquet",
                        set = 90)

    arrow::write_parquet(smq_content,
                         sink = paste0(path_meddra, "smq_content.parquet")
    )

    cli_progress_update(force = TRUE,
                        status = "Done",
                        set = 100)

    cli_progress_done()

  }

# Helpers ------------------------------


msg_tb_onceperdatabase <-
  function(){
    cli::cli_inform(
      "This process must only be done {.strong {cli::col_yellow('once')}} per database version.")
  }
