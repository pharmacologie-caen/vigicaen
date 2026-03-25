#' Create VigiBase ECL tables
#'
#' @description `r lifecycle::badge('stable')` Transform VigiBase .txt
#' files to .parquet files.
#'
#' @details Vigibase Extract Case Level is delivered as zipped csv files, that you should
#' transform to a more efficient format. Parquet format from `arrow` has many advantages:
#' It works with out-of-memory data, which makes it possible to process Vigibase tables on
#' a computer with not-so-much RAM. It is also lightweighted and standard across different
#' langages.
#' The function also creates variables in each table.
#' The `suspectedduplicates` table will be added to the base directory.
#' Use [dt_parquet()] to load the tables afterward.
#'
#' The argument `overwrite_existing_tables` is especially useful if the function crashes or is interrupted:
#' it allows you to resume the process without rebuilding tables that were already successfully created.
#' If set to FALSE (the default), the function will skip the construction of any .parquet tables that already exist,
#' so you do not have to start from scratch after a failure. Set to TRUE to force rebuilding all tables.
#'
#' @param path_base Character string, a directory containing vigibase csv tables. It is also the
#' output directory.
#' @param path_sub  Character string, a directory containing subsidiary tables.
#' @param force Logical, to be passed to `cli::cli_progress_update()`. Used for internal
#' purposes.
#' @param rm_suspdup Logical, should suspected duplicates (from SUSPECTEDDUPLICATES.txt) be removed from main tables? Default is TRUE. Set to FALSE to keep all cases, including suspected duplicates.
#' @param overwrite_existing_tables Logical, should existing parquet tables be overwritten? Default is FALSE.
#'
#' @keywords import
#' @importFrom stringr str_sub str_trim
#' @importFrom cli cli_progress_bar cli_progress_update
#' @importFrom cli cli_progress_done
#'
#' @returns
#' \itemize{
#'   \item .parquet files of all main tables into the `path_base`
#'   directory: demo, adr, drug, link, ind, out, srce,
#'   followup, and the suspdup (suspected duplicates) table.
#'   Check `?demo_` for more information on the tables.
#'   \item The link table is augmented with `tto_mean` and `range`, to analyze
#'   time to onset according to WHo's recommendations (see `vignette("descriptive")`.
#'   \item .parquet files of all other subsidiary tables into the `path_sub`
#'   directory: AgeGroup, Dechallenge, Dechallenge2, Frequency,
#'   Gender, Notifier, Outcome, Rechallenge, Rechallenge2, Region,
#'   RepBasis, ReportType, RouteOfAdm, Seriousness, and SizeUnit.
#'   }
#'
#' @export
#'
#' @seealso [tb_who()], [tb_meddra()], [tb_subset()], [dt_parquet()]
#'
#' @return .parquet files into the `path_base` directory (**including suspected duplicates tables**).
#' Some columns are returned as `integer` (UMCReportId, Drug_Id, Record_Id, Adr_Id, MedDRA_Id),
#' and some columns as `numeric` (TimeToOnsetMin, TimeToOnsetMax)
#' All other columns are `character`.
#'
#' @examplesIf interactive()
#'
#' # --- Set up example source files ---- ####
#'
#' path_base <- paste0(tempdir(), "/", "main", "/")
#'
#' path_sub  <- paste0(tempdir(), "/", "sub",  "/")
#'
#' dir.create(path_base)
#' dir.create(path_sub)
#'
#' create_ex_main_csv(path_base)
#' create_ex_sub_csv(path_sub)
#'
#'  # ---- Running tb_vigibase
#'
#'  tb_vigibase(path_base = path_base,
#'         path_sub  = path_sub)
#'
#'  # Clear temporary files when you're done
#'  unlink(path_base, recursive = TRUE)
#'  unlink(path_sub, recursive = TRUE)

tb_vigibase <-
  function(path_base,
           path_sub,
           force = FALSE,
           rm_suspdup = TRUE,
           overwrite_existing_tables = FALSE
           ){

    path_base <- fix_path_endslash(path_base)
    path_sub <- fix_path_endslash(path_sub)
    check_dir_exists(path_base)
    check_dir_exists(path_sub)

    cli::cli_h1(
      "tb_vigibase()"
    )

    screen_csv_main <- tb_screen_main(path_base, ext = ".csv")
    screen_csv_sub  <- tb_screen_sub( path_sub, ext = ".csv")

    screen_csv <- c(screen_csv_main, screen_csv_sub)

    expected_csv_files <-
      paste0(
        c("ADR", "DEMO", "DRUG", "FOLLOWUP", "IND", "LINK", "OUT", "SRCE",
          "AgeGroup_Lx", "Dechallenge_Lx", "Dechallenge2_Lx", "Frequency_Lx",
          "Gender_Lx", "Notifier_Lx", "Outcome_Lx", "Rechallenge_Lx", "Rechallenge2_Lx",
          "Region_Lx", "ReportType_Lx", "RouteOfAdm_Lx", "Seriousness_Lx",
          "SizeUnit_Lx", "SUSPECTEDDUPLICATES"),
             ".csv")

    if(all(expected_csv_files %in% screen_csv)) {
      cli::cli_alert_success("All expected csv files found in {.arg path_base} and {.arg path_sub}")
    } else {
      missing_csv_files <-
        expected_csv_files[!expected_csv_files %in% screen_csv]

      if(length(missing_csv_files) == 8) { # 8 = number of tables in main
        cli::cli_abort(
          c(
            "All csv files must be present in {.arg path_base} and {.arg path_sub}.",
            "i" = "As of vigicaen 1.1.0, input tables must be in .csv format."
          )
        )
          } else {
        cli::cli_abort(
          c(
            "All csv files must be present in {.arg path_base} and {.arg path_sub}.",
            "x" = "Missing file{?s}: {missing_csv_files}."
          ))
      }
    }

    # Scan for existing parquet tables ONLY if overwrite_existing_tables = FALSE

    if (!overwrite_existing_tables) {
      cli::cli_alert_info("Checking for existing tables.")

      main_parquet_tables <- tb_screen_main(path_base, ext = ".parquet")
      sub_parquet_tables <- tb_screen_sub(path_sub, ext = ".parquet")

      if(!length(main_parquet_tables) == 0 |
         !length(sub_parquet_tables) == 0) {
        cli::cli_alert_info("These tables won't be built again.")
        cli::cli_inform(c(">" = "Set {.arg overwrite_existing_tables} to TRUE to rewrite them."))
      }
    } else {
      main_parquet_tables <- character(0)
      sub_parquet_tables <- character(0)
    }

    cli::cli_alert_info(
    "Creating vigibase tables.")

    msg_tb_onceperdatabase()
    cli::cli_inform("It can take up to 30minutes.")

    cli_progress_bar(
      "Creating vigibase",
      format = "{cli::pb_bar} {cli::pb_percent} | {cli::pb_elapsed} | {cli::pb_status}",
      total = 100
    )

    # ---- suspectedduplicates (always created first) ---- ####
    if (overwrite_existing_tables || !("suspdup.parquet" %in% main_parquet_tables)) {
      cli_progress_update(force = force, status = "Read SUSPECTEDDUPLICATES.csv", set = 1)
      suspdup <-
        arrow::read_csv_arrow(
          paste0(path_sub, "SUSPECTEDDUPLICATES.csv"),
          col_names = FALSE,
          as_data_frame = FALSE,
          schema =
            arrow::schema(
              UMCReportId  = arrow::int32(),
              SuspectedduplicateReportId = arrow::int32()
            )
        )

      cli_progress_update(force = force, status = "Write suspdup.parquet", set = 3)
      arrow::write_parquet(suspdup, sink = paste0(path_base, "suspdup.parquet"))

      # If rm_suspdup, prepare the list of duplicates
      if (rm_suspdup) {
        duplicates <- suspdup |> dplyr::pull(.data$SuspectedduplicateReportId, as_vector = FALSE)
      } else {
        duplicates <- NULL
      }
      rm(suspdup)
      gc()
    } else {
      # If not re-reading, need to load duplicates if rm_suspdup
      if (rm_suspdup) {
        suspdup <- dt_parquet(path_base, "suspdup", in_memory = FALSE)
        duplicates <- suspdup |> dplyr::pull(.data$SuspectedduplicateReportId, as_vector = TRUE)
        rm(suspdup)
        gc()
      }
    }

    # ---- demo ---- ####
    if (overwrite_existing_tables || !("demo.parquet" %in% main_parquet_tables)) {
      cli_progress_update(force = force, status = "Read DEMO.csv", set = 4)
      demo <-
        arrow::read_csv_arrow(
          paste0(path_base, "DEMO.csv"),
          col_names = FALSE,
          as_data_frame = FALSE,
          schema =
            arrow::schema(
              UMCReportId  = arrow::int32(),
              AgeGroup     = arrow::string(),
              Gender       = arrow::string(),
              DateDatabase = arrow::string(),
              Type         = arrow::string(),
              Region       = arrow::string(),
              FirstDateDatabase = arrow::string()
            )
        )

      if (rm_suspdup) {
        cli_progress_update(force = force, status = "Remove duplicates", set = 7)
        demo <- demo |> dplyr::filter(!.data$UMCReportId %in% duplicates) |>
          dplyr::compute()
      }
      cli_progress_update(force = force, status = "Write demo.parquet", set = 8)
      arrow::write_parquet(demo, sink = paste0(path_base, "demo.parquet"))
      rm(demo)
      gc()
    }
    # ---- drug ---- ####
    if (overwrite_existing_tables || !("drug.parquet" %in% main_parquet_tables)) {
      cli_progress_update(force = force, status = "Read DRUG.csv", set = 10)
      drug <- arrow::read_csv_arrow(
        paste0(path_base, "DRUG.csv"),
        col_names = FALSE,
        as_data_frame = FALSE,
        schema =
          arrow::schema(
            UMCReportId = arrow::int32(),
            Drug_Id     = arrow::int32(),
            Record_Id   = arrow::int32(),
            DrecNo      = arrow::int32(),
            Seq1        = arrow::string(),
            Seq2        = arrow::string(),
            Route       = arrow::string(),
            Basis       = arrow::string(),
            Amount      = arrow::string(),
            AmountU     = arrow::string(),
            Frequency   = arrow::string(),
            FrequencyU  = arrow::string()
          )
      )
      # Remove suspected duplicates if requested
      if (rm_suspdup) {
        cli_progress_update(force = force, status = "Remove duplicates", set = 13)
        drug <- drug |> dplyr::filter(!.data$UMCReportId %in% duplicates) |>
          dplyr::compute()
        drug_ids <- dplyr::pull(drug, .data$Drug_Id, as_vector = TRUE)
      } else {
        drug_ids <- NULL
      }
      cli_progress_update(force = force, status = "Write drug.parquet", set = 14)
      arrow::write_parquet(drug, sink = paste0(path_base, "drug.parquet"))
      rm(drug)
      gc()
    } else {
      # If not re-reading drug, but drug_ids are needed for ind
      if (rm_suspdup && !("ind.parquet" %in% main_parquet_tables)) {
        drug <- dt_parquet(path_base, "drug", in_memory = FALSE)
        drug_ids <- drug |> dplyr::pull(.data$Drug_Id, as_vector = FALSE)
        rm(drug)
        gc()
      }
    }
    # ---- followup ---- ####
    if (overwrite_existing_tables || !("followup.parquet" %in% main_parquet_tables)) {
      cli_progress_update(force = force, status = "Read FOLLOWUP.csv", set = 16)
      followup <- arrow::read_csv_arrow(
        paste0(path_base, "FOLLOWUP.csv"),
        col_names = FALSE,
        as_data_frame = FALSE,
        schema =
          arrow::schema(
            UMCReportId         = arrow::int32(),
            ReplacedUMCReportId = arrow::int32()
          )
      )
      if (rm_suspdup) {
        cli_progress_update(force = force, status = "Remove duplicates", set = 19)
        followup <- followup |> dplyr::filter(!.data$UMCReportId %in% duplicates) |>
          dplyr::compute()
      }
      cli_progress_update(force = force, status = "Write followup.parquet", set = 20)
      arrow::write_parquet(followup, sink = paste0(path_base, "followup.parquet"))
      rm(followup)
      gc()
    }
    # ---- adr ---- ####
    if (overwrite_existing_tables || !("adr.parquet" %in% main_parquet_tables)) {
      cli_progress_update(force = force, status = "Read ADR.csv", set = 22)
      adr <- arrow::read_csv_arrow(
        paste0(path_base, "ADR.csv"),
        col_names = FALSE,
        as_data_frame = FALSE,
        schema =
          arrow::schema(
            UMCReportId = arrow::int32(),
            Adr_Id      = arrow::int32(),
            MedDRA_Id   = arrow::int32(),
            Outcome     = arrow::string()
          )
      )

      if (rm_suspdup) {
        cli_progress_update(force = force, status = "Remove duplicates", set = 25)
        adr <- adr |> dplyr::filter(!.data$UMCReportId %in% duplicates) |>
          dplyr::compute()
      }
      cli_progress_update(force = force, status = "Write adr.parquet", set = 26)
      arrow::write_parquet(adr, sink = paste0(path_base, "adr.parquet"))
    } else {
      # If not re-reading adr, but adr is needed for link
      if (!("link.parquet" %in% main_parquet_tables)) {
        adr <- dt_parquet(path_base, "adr", in_memory = FALSE)
      }
    }
    # ---- out ---- ####
    if (overwrite_existing_tables || !("out.parquet" %in% main_parquet_tables)) {
      cli_progress_update(force = force, status = "Read OUT.csv", set = 27)
      out <- arrow::read_csv_arrow(
        paste0(path_base, "OUT.csv"),
        col_names = FALSE,
        as_data_frame = FALSE,
        schema =
          arrow::schema(
            UMCReportId = arrow::int32(),
            Seriousness = arrow::string(),
            Serious     = arrow::string()
          )
      )

      if (rm_suspdup) {
        cli_progress_update(force = force, status = "Remove duplicates", set = 29)
        out <- out |> dplyr::filter(!.data$UMCReportId %in% duplicates) |>
          dplyr::compute()
      }
      cli_progress_update(force = force, status = "Write out.parquet", set = 30)
      arrow::write_parquet(out, sink = paste0(path_base, "out.parquet"))
      rm(out)
      gc()
    }
    # ---- srce ---- ####
    if (overwrite_existing_tables || !("srce.parquet" %in% main_parquet_tables)) {
      cli_progress_update(force = force, status = "Read SRCE.csv", set = 31)
      srce <- arrow::read_csv_arrow(
        paste0(path_base, "SRCE.csv"),
        col_names = FALSE,
        as_data_frame = FALSE,
        schema =
          arrow::schema(
            UMCReportId = arrow::int32(),
            Type        = arrow::string()
          )
      )

      if (rm_suspdup) {
        cli_progress_update(force = force, status = "Remove duplicates", set = 33)
        srce <- srce |> dplyr::filter(!.data$UMCReportId %in% duplicates) |>
          dplyr::compute()
      }
      cli_progress_update(force = force, status = "Write srce.parquet", set = 34)
      arrow::write_parquet(srce, sink = paste0(path_base, "srce.parquet"))
      rm(srce)
      gc()
    }
    # ---- link ---- ####
    if (overwrite_existing_tables || !("link.parquet" %in% main_parquet_tables)) {
      cli_progress_update(force = force, status = "Read LINK.csv", set = 35)
      link <- arrow::read_csv_arrow(
        paste0(path_base, "LINK.csv"),
        col_names = FALSE,
        as_data_frame = FALSE,
        schema =
          arrow::schema(
            Drug_Id        = arrow::int32(),
            Adr_Id         = arrow::int32(),
            Dechallenge1   = arrow::string(),
            Dechallenge2   = arrow::string(),
            Rechallenge1   = arrow::string(),
            Rechallenge2   = arrow::string(),
            TimeToOnsetMin = arrow::string(),
            TimeToOnsetMax = arrow::string()
          )
      )
      cli_progress_update(force = force, status = "Process link (longest step)", set = 36)
      link <- link |>
        dplyr::mutate(
          dplyr::across(dplyr::all_of(c("TimeToOnsetMin", "TimeToOnsetMax")),
                        ~
                          .x |>
                          # str_trim() |>
                          stringr::str_replace("^-$", "1568459784.65489") |>
                          as.numeric()
          ),
          dplyr::across(dplyr::all_of(c("TimeToOnsetMin", "TimeToOnsetMax")),
                        ~ dplyr::if_else(.x == 1568459784.65489, NA_real_, .x)),

          tto_mean = (.data$TimeToOnsetMax + .data$TimeToOnsetMin) / 2,
          range = (.data$TimeToOnsetMax + .data$TimeToOnsetMin) / 2 - .data$TimeToOnsetMin
          )|>
        dplyr::left_join(
          adr |>
            dplyr::select(dplyr::all_of(c("UMCReportId", 'Adr_Id'))),
          by = "Adr_Id"
        ) |>
        dplyr::compute()
      if (rm_suspdup) {
        cli_progress_update(force = force, status = "Remove duplicates", set = 72)
        link <- link |> dplyr::filter(!is.na(.data$UMCReportId)) |>
          dplyr::compute()
      }
      cli_progress_update(force = force, status = "Write link.parquet", set = 73)
      arrow::write_parquet(link, sink = paste0(path_base, "link.parquet"))
      rm(adr, link)
      gc()
    }
    # ---- ind ---- ####
    if (overwrite_existing_tables ||
        !("ind.parquet" %in% main_parquet_tables)) {
      cli_progress_update(force = force,
                          status = "Read IND.csv",
                          set = 74)
      ind <- arrow::read_csv_arrow(
        paste0(path_base, "IND.csv"),
        col_names = FALSE,
        as_data_frame = FALSE,
        schema =
          arrow::schema(
            Drug_Id        = arrow::int32(),
            Indication     = arrow::string()
          )
        )
      # arrow::csv_read_options(encoding = "ANSI_X3.4-1986") # seems to be
      # outdated as of csv export of VigiBase

      if (rm_suspdup) {
        cli_progress_update(force = force,
                            status = "Remove duplicates",
                            set = 76)
        ind <- ind |> dplyr::filter(.data$Drug_Id %in% drug_ids) |>
          dplyr::compute()
      }
      cli_progress_update(force = force,
                          status = "Write ind.parquet",
                          set = 98)
      arrow::write_parquet(ind, sink = paste0(path_base, "ind.parquet"))
      rm(ind)
      gc()
    }
    # ---- Unique block for subsidiary tables ---- ####

    secondary_tables <- c(
      "AgeGroup", "Dechallenge", "Dechallenge2", "Frequency",
      "Gender", "Notifier", "Outcome", "Rechallenge",
      "Rechallenge2", "Region", "RepBasis", "ReportType",
      "RouteOfAdm", "Seriousness", "SizeUnit"
    )

    secondary_tables_parquet <- paste0(secondary_tables, ".parquet")

    if (overwrite_existing_tables || !all(secondary_tables_parquet %in% sub_parquet_tables)) {
      cli_progress_update(force = force,
        status = "Process Subsidiary files",
        set = 99)

      subs <-
        secondary_tables |>
        rlang::set_names(secondary_tables_parquet) |>
        purrr::imap(
          function(t_, name_){
          full_path <- paste0(path_sub, t_, "_Lx.csv")

          table <-
            arrow::read_csv_arrow(
            full_path,
            col_names = FALSE,
            as_data_frame = FALSE,
            schema =
              arrow::schema(
                Code = arrow::string(),
                Text = arrow::string()
              )
            )

            arrow::write_parquet(table, sink = paste0(path_sub, name_))
        })

      cli_progress_update(force = force,
        status = "Done",
        set = 100)
      cli_progress_done()
  }
  }

# Helper: scan for main parquet tables
#' @param path_base character string, folder with base files
#' @param ext character string, either ".parquet", ".csv", or ".txt"
#' @keywords internal
#' @noRd
tb_screen_main <- function(path_base,
                                   ext = c(".parquet", ".csv", ".txt")) {

  ext <- rlang::arg_match(ext)

  main_tables <- c("demo",
                   "adr",
                   "drug",
                   "link",
                   "ind",
                   "out",
                   "srce",
                   "followup",
                   "suspdup")
  pattern <- paste0("^(", paste(main_tables, collapse = "|"),
                    ")\\",
                    ext,
                    "$")
  files <- list.files(path_base, pattern = pattern, full.names = FALSE,
                      ignore.case = TRUE)
  if (length(files) > 0 && ext == ".parquet") {
    # only displayed if parquet checking - part of user info linked to
    # overwrite_existing_tables arg.
    cli::cli_inform(
      "The following tables were found as {ext} files in {.arg path_base}: {.val {files}}"
    )
  }
  return(files)
}

# Helper: scan for subsidiary parquet tables
#' @keywords internal
#' @noRd

tb_screen_sub <- function(path_sub,
                                  ext = c(".parquet", ".csv", ".txt")) {

  ext <- rlang::arg_match(ext)

  pattern_tables <- c(
    "AgeGroup_Lx",
    "Dechallenge_Lx",
    "Dechallenge2_Lx",
    "Frequency_Lx",
    "Gender_Lx",
    "Notifier_Lx",
    "Outcome_Lx",
    "Rechallenge_Lx",
    "Rechallenge2_Lx",
    "Region_Lx",
    "RepBasis_Lx",
    "ReportType_Lx",
    "RouteOfAdm_Lx",
    "Seriousness_Lx",
    "SizeUnit_Lx",
    "SUSPECTEDDUPLICATES"
  )

  if(ext == ".parquet"){
    pattern_tables <-
      pattern_tables[!pattern_tables == "SUSPECTEDDUPLICATES"] |>
      stringr::str_replace("_Lx$", "")
  }

  pattern <- paste0("^(", paste(pattern_tables, collapse = "|"),
                    ")\\",
                    ext,
                    "$")

  files <- list.files(path_sub, pattern = pattern, full.names = FALSE)
  if (length(files) == length(pattern_tables) && ext == ".parquet") {
    cli::cli_inform("Subsidiary files were found as {ext} files.")
  }
  return(files)
}
