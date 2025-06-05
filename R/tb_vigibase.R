#' Create main VigiBase ECL tables
#'
#' @description `r lifecycle::badge('stable')` Transform VigiBase .txt
#' files to .parquet files.
#'
#' @details Vigibase Extract Case Level is delivered as zipped text files, that you should
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
#' @param path_base Character string, a directory containing vigibase txt tables. It is also the
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
#' Some columns are returned as `integer` (UMCReportId, Drug_Id, MedicinalProd_Id, Adr_Id, MedDRA_Id),
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
#' create_ex_main_txt(path_base)
#' create_ex_sub_txt(path_sub)
#'
#'  # ---- Running tb_vigibase
#'
#'  tb_vigibase(path_base = path_base,
#'         path_sub  = path_sub)
#'
#'  # Clear temporary files when you're done
#'  unlink(path_base, recursive = TRUE)
#'  unlink(path_sub, recursive = TRUE)


# Helper: scan for main parquet tables
#' @keywords internal
#' @noRd
tb_screen_main_parquet <- function(path_base) {
  main_tables <- c("demo", "adr", "drug", "link", "ind", "out", "srce", "followup", "suspdup")
  pattern <- paste0("^(", paste(main_tables, collapse = "|"), ")\\.parquet$")
  files <- list.files(path_base, pattern = pattern, full.names = FALSE)
  if (length(files) > 0) {
    cli::cli_inform("The following tables were already found as parquet files in path_base: {.val {files}}")
    cli::cli_alert_info("These tables won't be build again.")
    cli::cli_alert_info("Set {.arg `overwrite_existing_tables`} to TRUE to rewrite those tables.")
  }
  return(files)
}

# Helper: scan for subsidiary parquet tables
#' @keywords internal
#' @noRd
tb_screen_sub_parquet <- function(path_sub) {
  pattern_tables <- c(
    "AgeGroup", "Dechallenge", "Dechallenge2", "Frequency", "Gender", "Notifier", "Outcome",
    "Rechallenge", "Rechallenge2", "Region", "RepBasis", "ReportType", "RouteOfAdm", "Seriousness", "SizeUnit"
  )
  pattern <- paste0("^(", paste(pattern_tables, collapse = "|"), ")\\.parquet$")
  files <- list.files(path_sub, pattern = pattern, full.names = FALSE)
  if (length(files) == length(pattern_tables)) {
    cli::cli_inform("Subsidiary files were already found as parquet files")
    cli::cli_alert_info("These tables won't be build again.")
    cli::cli_alert_info("Set {.arg overwrite_existing_tables} to TRUE to rewrite those tables")
  }
  return(files)
}

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

    # Scan for existing parquet tables ONLY if overwrite_existing_tables = FALSE
    if (!overwrite_existing_tables) {
      main_parquet_tables <- tb_screen_main_parquet(path_base)
      sub_parquet_tables <- tb_screen_sub_parquet(path_sub)
    }

    cli::cli_h1(
      "tb_vigibase()"
    )
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
      cli_progress_update(force = force, status = "Read SUSPECTEDDUPLICATES.txt", set = 1)
      suspdup <- reader("SUSPECTEDDUPLICATES.txt", folder = path_sub)
      cli_progress_update(force = force, status = "Split suspdup", set = 2)
      suspdup <- suspdup |>
        dplyr::transmute(
          UMCReportId                = str_sub(.data$f0, start = 1L,  end = 11L),
          SuspectedduplicateReportId = str_sub(.data$f0, start = 12L, end = 22L)
        ) |>
        dplyr::mutate(
          dplyr::across(dplyr::all_of(c("UMCReportId", "SuspectedduplicateReportId")),
                        ~ .x |> str_trim() |> as.integer())
        ) |>
        dplyr::compute()
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
        duplicates <- suspdup$SuspectedduplicateReportId
        rm(suspdup)
        gc()
      }
    }

    # ---- demo ---- ####
    if (overwrite_existing_tables || !("demo.parquet" %in% main_parquet_tables)) {
      cli_progress_update(force = force, status = "Read DEMO.txt", set = 4)
      demo <- reader("DEMO.txt", path_base)
      cli_progress_update(force = force,
        status = "Split demo",
        set = 6)
      demo <-
        demo |>
        dplyr::transmute(
          UMCReportId  = str_sub(.data$f0, start = 1L,  end = 11L),
          AgeGroup     = str_sub(.data$f0, start = 12L, end = 12L),
          Gender       = str_sub(.data$f0, start = 13L, end = 13L),
          DateDatabase = str_sub(.data$f0, start = 14L, end = 21L),
          Type         = str_sub(.data$f0, start = 22L, end = 22L),
          Region       = str_sub(.data$f0, start = 23L, end = 23L),
          FirstDateDatabase = str_sub(.data$f0, start = 24L, end = 31L)
        ) |>
        dplyr::mutate(
          UMCReportId = .data$UMCReportId |>
            str_trim() |>
            as.integer()
          ) |>
        dplyr::compute()
      if (rm_suspdup) {
        cli_progress_update(force = force, status = "Remove duplicates", set = 7)
        demo <- demo |> dplyr::filter(!.data$UMCReportId %in% duplicates)
      }
      cli_progress_update(force = force, status = "Write demo.parquet", set = 8)
      arrow::write_parquet(demo, sink = paste0(path_base, "demo.parquet"))
      rm(demo)
      gc()
    }
    # ---- drug ---- ####
    if (overwrite_existing_tables || !("drug.parquet" %in% main_parquet_tables)) {
      cli_progress_update(force = force, status = "Read DRUG.txt", set = 10)
      drug <- reader("DRUG.txt", path_base)
      cli_progress_update(force = force, status = "Split drug", set = 12)
      drug <- drug |>
        dplyr::transmute(
          UMCReportId = str_sub(.data$f0, start = 1L, end = 11L),
          Drug_Id     = str_sub(.data$f0, start = 12L, end = 22L),
          MedicinalProd_Id = str_sub(.data$f0, start = 23L, end = 33L),
          DrecNo      = str_sub(.data$f0, start = 34L, end = 39L),
          Seq1        = str_sub(.data$f0, start = 40L, end = 41L),
          Seq2        = str_sub(.data$f0, start = 42L, end = 44L),
          Route       = str_sub(.data$f0, start = 45L, end = 46L),
          Basis       = str_sub(.data$f0, start = 47L, end = 47L),
          Amount      = str_sub(.data$f0, start = 48L, end = 52L),
          AmountU     = str_sub(.data$f0, start = 53L, end = 54L),
          Frequency   = str_sub(.data$f0, start = 55L, end = 56L),
          FrequencyU  = str_sub(.data$f0, start = 57L, end = 59L)
          ) |>
        dplyr::mutate(
          dplyr::across(dplyr::all_of(c("UMCReportId", "Drug_Id",
                                        "MedicinalProd_Id",
                                        "DrecNo")),
                 ~ .x |>
            str_trim() |>
            as.integer()
          )
        ) |>
        dplyr::compute()
      # Remove suspected duplicates if requested
      if (rm_suspdup) {
        cli_progress_update(force = force, status = "Remove duplicates", set = 13)
        drug <- drug |> dplyr::filter(!.data$UMCReportId %in% duplicates)
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
      cli_progress_update(force = force, status = "Read FOLLOWUP.txt", set = 16)
      followup <- reader("FOLLOWUP.txt", path_base)
      cli_progress_update(force = force, status = "Split followup", set = 18)
      followup <- followup |>
        dplyr::transmute(
          UMCReportId = str_sub(.data$f0, start = 1L, end = 11L),
          ReplacedUMCReportId = str_sub(.data$f0, start = 12L, end = 22L)
          ) |>
        dplyr::mutate(
          dplyr::across(dplyr::all_of(c("UMCReportId", "ReplacedUMCReportId")),
                 ~ .x |>
                   str_trim() |>
                   as.integer()
          )
        ) |>
        dplyr::compute()
      if (rm_suspdup) {
        cli_progress_update(force = force, status = "Remove duplicates", set = 19)
        followup <- followup |> dplyr::filter(!.data$UMCReportId %in% duplicates)
      }
      cli_progress_update(force = force, status = "Write followup.parquet", set = 20)
      arrow::write_parquet(followup, sink = paste0(path_base, "followup.parquet"))
      rm(followup)
      gc()
    }
    # ---- adr ---- ####
    if (overwrite_existing_tables || !("adr.parquet" %in% main_parquet_tables)) {
      cli_progress_update(force = force, status = "Read ADR.txt", set = 22)
      adr <- reader("ADR.txt", path_base)
      cli_progress_update(force = force, status = "Split adr", set = 24)
      adr <- adr |>
        dplyr::transmute(
          UMCReportId = str_sub(.data$f0, start = 1L, end = 11L),
          Adr_Id      = str_sub(.data$f0, start = 12L, end = 22L),
          MedDRA_Id   = str_sub(.data$f0, start = 23L, end = 30L),
          Outcome     = str_sub(.data$f0, start = 31L, end = 31L)
        ) |>
        dplyr::mutate(dplyr::across(dplyr::all_of(c("UMCReportId", "Adr_Id", "MedDRA_Id")), ~ .x |> str_trim() |> as.integer())) |>
        dplyr::compute()
      if (rm_suspdup) {
        cli_progress_update(force = force, status = "Remove duplicates", set = 25)
        adr <- adr |> dplyr::filter(!.data$UMCReportId %in% duplicates)
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
      cli_progress_update(force = force, status = "Read OUT.txt", set = 28)
      out <- reader("OUT.txt", path_base)
      cli_progress_update(force = force, status = "Split out", set = 30)
      out <- out |>
        dplyr::transmute(
          UMCReportId = str_sub(.data$f0, start = 1L, end = 11L),
          Seriousness = str_trim(str_sub(.data$f0, start = 12L, end = 13L)),
          Serious     = str_sub(.data$f0, start = 14L, end = 14L)
        ) |>
        dplyr::mutate(dplyr::across(dplyr::all_of(c("UMCReportId")), ~ .x |> str_trim() |> as.integer())) |>
        dplyr::compute()
      if (rm_suspdup) {
        cli_progress_update(force = force, status = "Remove duplicates", set = 31)
        out <- out |> dplyr::filter(!.data$UMCReportId %in% duplicates)
      }
      cli_progress_update(force = force, status = "Write out.parquet", set = 32)
      arrow::write_parquet(out, sink = paste0(path_base, "out.parquet"))
      rm(out)
      gc()
    }
    # ---- srce ---- ####
    if (overwrite_existing_tables || !("srce.parquet" %in% main_parquet_tables)) {
      cli_progress_update(force = force, status = "Read SRCE.txt", set = 34)
      srce <- reader("SRCE.txt", path_base)
      cli_progress_update(force = force, status = "Split srce", set = 36)
      srce <- srce |>
        dplyr::transmute(
          UMCReportId = str_sub(.data$f0, start = 1L, end = 11L),
          Type        = str_trim(str_sub(.data$f0, start = 12L, end = 13L))
        ) |>
        dplyr::mutate(dplyr::across(dplyr::all_of(c("UMCReportId")), ~ .x |> str_trim() |> as.integer())) |>
        dplyr::compute()
      if (rm_suspdup) {
        cli_progress_update(force = force, status = "Remove duplicates", set = 37)
        srce <- srce |> dplyr::filter(!.data$UMCReportId %in% duplicates)
      }
      cli_progress_update(force = force, status = "Write srce.parquet", set = 38)
      arrow::write_parquet(srce, sink = paste0(path_base, "srce.parquet"))
      rm(srce)
      gc()
    }
    # ---- link ---- ####
    if (overwrite_existing_tables || !("link.parquet" %in% main_parquet_tables)) {
      cli_progress_update(force = force, status = "Read LINK.txt", set = 40)
      link <- reader("LINK.txt", path_base)
      cli_progress_update(force = force, status = "Split link (longest step)", set = 42)
      link <- link |>
        dplyr::transmute(
          Drug_Id        = str_sub(.data$f0, start = 1L,  end = 11L),
          Adr_Id         = str_sub(.data$f0, start = 12L, end = 22L),
          Dechallenge1   = str_sub(.data$f0, start = 23L, end = 23L),
          Dechallenge2   = str_sub(.data$f0, start = 24L, end = 24L),
          Rechallenge1   = str_sub(.data$f0, start = 25L, end = 25L),
          Rechallenge2   = str_sub(.data$f0, start = 26L, end = 26L),
          TimeToOnsetMin = str_sub(.data$f0, start = 27L, end = 37L),
          TimeToOnsetMax = str_sub(.data$f0, start = 38L, end = 48L)
        ) |>
        dplyr::mutate(
          dplyr::across(dplyr::all_of(c("Drug_Id", "Adr_Id")),
                        ~ .x |>
                          str_trim() |>
                          as.integer()
                        ),
          dplyr::across(dplyr::all_of(c("TimeToOnsetMin", "TimeToOnsetMax")),
                        ~
                          .x |>
                          str_trim() |>
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
        # since joining UMC from adr. If adr duplicates are removed, there will be
        # unmatched lines in link. Those are the duplicates of link.
        cli_progress_update(force = force, status = "Remove duplicates", set = 43)
        link <- link |> dplyr::filter(!is.na(.data$UMCReportId))
      }
      cli_progress_update(force = force, status = "Write link.parquet", set = 44)
      arrow::write_parquet(link, sink = paste0(path_base, "link.parquet"))
      rm(adr, link)
      gc()
    }
    # ---- ind ---- ####
    if (overwrite_existing_tables || !("ind.parquet" %in% main_parquet_tables)) {
      cli_progress_update(force = force, status = "Read IND.txt", set = 46)
      ind <- arrow::read_delim_arrow(paste0(path_base, "IND.txt"), col_names = FALSE, as_data_frame = FALSE, delim = "\t", read_options = arrow::csv_read_options(column_names = "f0", encoding = "ANSI_X3.4-1986"))
      cli_progress_update(force = force, status = "Split ind", set = 48)
      ind <- ind |>
        dplyr::transmute(
          Drug_Id    = str_sub(.data$f0, start = 1L,  end = 11L),
          Indication = str_trim(str_sub(.data$f0, start = 12L, end = 266L))
        ) |>
        dplyr::mutate(dplyr::across(dplyr::all_of(c("Drug_Id")), ~ .x |> str_trim() |> as.integer())) |>
        dplyr::compute()
      if (rm_suspdup) {
        cli_progress_update(force = force, status = "Remove duplicates", set = 49)
        ind <- ind |> dplyr::filter(.data$Drug_Id %in% drug_ids)
      }
      cli_progress_update(force = force, status = "Write ind.parquet", set = 50)
      arrow::write_parquet(ind, sink = paste0(path_base, "ind.parquet"))
      rm(ind)
      gc()
    }
    # ---- Unique block for subsidiary tables ---- ####
    secondary_tables <- c(
      "AgeGroup.parquet", "Dechallenge.parquet", "Dechallenge2.parquet", "Frequency.parquet",
      "Gender.parquet", "Notifier.parquet", "Outcome.parquet", "Rechallenge.parquet",
      "Rechallenge2.parquet", "Region.parquet", "RepBasis.parquet", "ReportType.parquet",
      "RouteOfAdm.parquet", "Seriousness.parquet", "SizeUnit.parquet"
    )
    if (overwrite_existing_tables || !all(secondary_tables %in% sub_parquet_tables)) {
      cli_progress_update(force = force,
        status = "Process Subsidiary files",
        set = 95)

      AgeGroup <- reader("AgeGroup_Lx.txt", path_sub)
      AgeGroup <-
        AgeGroup |>
        dplyr::transmute(
          AgeGroup = substr(.data$f0, start = 1, stop = 1),
          Code = substr(.data$f0, start = 2, stop = 26) |> str_trim()
        ) |>
        dplyr::compute()
      arrow::write_parquet(AgeGroup, sink = paste0(path_sub, "AgeGroup.parquet"))

      # Dechallenge
      Dechallenge <- reader("Dechallenge_Lx.txt", path_sub)
      Dechallenge <-
        Dechallenge |>
        dplyr::transmute(
          Dechallenge1 = substr(.data$f0, start = 1, stop = 1),
          Code = substr(.data$f0, start = 2, stop = 257) |> str_trim()
        ) |>
        dplyr::compute()
      arrow::write_parquet(Dechallenge, sink = paste0(path_sub, "Dechallenge.parquet"))

      # Dechallenge2

      Dechallenge2 <- reader("Dechallenge2_Lx.txt", path_sub)
      Dechallenge2 <-
        Dechallenge2 |>
        dplyr::transmute(
          Dechallenge2 = substr(.data$f0, start = 1, stop = 1),
          Code = substr(.data$f0, start = 2, stop = 257) |> str_trim()
        ) |>
        dplyr::compute()
      arrow::write_parquet(Dechallenge2, sink = paste0(path_sub, "Dechallenge2.parquet"))

      # FrequencyU

      Frequency <- reader("Frequency_Lx.txt", path_sub)
      Frequency <-
        Frequency |>
        dplyr::transmute(
          FrequencyU = substr(.data$f0, start = 1, stop = 3),
          Code = substr(.data$f0, start = 4, stop = 259) |> str_trim()
        ) |>
        dplyr::compute()
      arrow::write_parquet(Frequency, sink = paste0(path_sub, "Frequency.parquet"))

      # Gender

      Gender <- reader("Gender_Lx.txt", path_sub)
      Gender <-
        Gender |>
        dplyr::transmute(
          Gender = substr(.data$f0, start = 1, stop = 1),
          Code = substr(.data$f0, start = 2, stop = 257) |> str_trim()
        ) |>
        dplyr::compute()
      arrow::write_parquet(Gender, sink = paste0(path_sub, "Gender.parquet"))

      # Notifier

      Notifier <- reader("Notifier_Lx.txt", path_sub)
      Notifier <-
        Notifier |>
        dplyr::transmute(
          Type = substr(.data$f0, start = 1, stop = 2) |> str_trim() |> as.integer(),
          Code = substr(.data$f0, start = 3, stop = 258) |> str_trim()
        ) |>
        dplyr::compute()
      arrow::write_parquet(Notifier, sink = paste0(path_sub, "Notifier.parquet"))

      # Outcome

      Outcome <- reader("Outcome_Lx.txt", path_sub)
      Outcome <-
        Outcome |>
        dplyr::transmute(
          Outcome = substr(.data$f0, start = 1, stop = 1),
          Code = substr(.data$f0, start = 2, stop = 257) |> str_trim()
        ) |>
        dplyr::compute()
      arrow::write_parquet(Outcome, sink = paste0(path_sub, "Outcome.parquet"))

      # Rechallenge

      Rechallenge <- reader("Rechallenge_Lx.txt", path_sub)
      Rechallenge <-
        Rechallenge |>
        dplyr::transmute(
          Rechallenge1 = substr(.data$f0, start = 1, stop = 1),
          Code = substr(.data$f0, start = 2, stop = 81) |> str_trim()
        ) |>
        dplyr::compute()
      arrow::write_parquet(Rechallenge, sink = paste0(path_sub, "Rechallenge.parquet"))

      # Rechallenge2

      Rechallenge2 <- reader("Rechallenge2_Lx.txt", path_sub)
      Rechallenge2 <-
        Rechallenge2 |>
        dplyr::transmute(
          Rechallenge2 = substr(.data$f0, start = 1, stop = 1),
          Code = substr(.data$f0, start = 2, stop = 81) |> str_trim()
        ) |>
        dplyr::compute()
      arrow::write_parquet(Rechallenge2, sink = paste0(path_sub, "Rechallenge2.parquet"))

      # Region

      Region <- reader("Region_Lx.txt", path_sub)
      Region <-
        Region |>
        dplyr::transmute(
          Region = substr(.data$f0, start = 1, stop = 1),
          Code = substr(.data$f0, start = 2, stop = 51) |> str_trim()
        ) |>
        dplyr::compute()
      arrow::write_parquet(Region, sink = paste0(path_sub, "Region.parquet"))

      # RepBasis

      RepBasis <- reader("RepBasis_Lx.txt", path_sub)
      RepBasis <-
        RepBasis |>
        dplyr::transmute(
          Basis = substr(.data$f0, start = 1, stop = 1),
          Code = substr(.data$f0, start = 2, stop = 51) |> str_trim()
        ) |>
        dplyr::compute()
      arrow::write_parquet(RepBasis, sink = paste0(path_sub, "RepBasis.parquet"))

      # ReportType

      ReportType <- reader("ReportType_Lx.txt", path_sub)
      ReportType <-
        ReportType |>
        dplyr::transmute(
          ReportType = substr(.data$f0, start = 1, stop = 1),
          Code = substr(.data$f0, start = 2, stop = 257) |> str_trim()
        ) |>
        dplyr::compute()
      arrow::write_parquet(ReportType, sink = paste0(path_sub, "ReportType.parquet"))

      # RouteOfAdm

      RouteOfAdm <- reader("RouteOfAdm_Lx.txt", path_sub)
      RouteOfAdm <-
        RouteOfAdm |>
        dplyr::transmute(
          Route = substr(.data$f0, start = 1, stop = 2) |> str_trim() |> as.integer(),
          Code = substr(.data$f0, start = 3, stop = 82) |> str_trim()
        ) |>
        dplyr::compute()
      arrow::write_parquet(RouteOfAdm, sink = paste0(path_sub, "RouteOfAdm.parquet"))

      # Seriousness

      Seriousness <- reader("Seriousness_Lx.txt", path_sub)
      Seriousness <-
        Seriousness |>
        dplyr::transmute(
          Seriousness = substr(.data$f0, start = 1, stop = 2) |>
          str_trim() |>
          as.integer(),
          Code = substr(.data$f0, start = 3, stop = 258) |> str_trim()
        ) |>
        dplyr::compute()
      arrow::write_parquet(Seriousness, sink = paste0(path_sub, "Seriousness.parquet"))

      # SizeUnit

      SizeUnit <-
        read.table(
          file = paste0(path_sub, "SizeUnit_Lx.txt"),
          header = FALSE,
          sep = "\t",
          quote = "",
          dec = ".",
          fill = TRUE,
          comment.char = "",
          stringsAsFactors = FALSE,
          col.names = "f0",
          colClasses = "character",
          nrows = 1000)

      SizeUnit$f0 <- iconv(SizeUnit$f0, from = "ANSI_X3.4-1986", to = "UTF8")

      SizeUnit <-
        SizeUnit |>
        dplyr::transmute(
          AmountU = substr(.data$f0, start = 1, stop = 2) |>
          str_trim() |>
          as.integer(),
          Code = substr(.data$f0, start = 3, stop = 82) |> str_trim()
        ) |>
        dplyr::compute()
      arrow::write_parquet(SizeUnit, sink = paste0(path_sub, "SizeUnit.parquet"))

      cli_progress_update(force = force,
        status = "Done",
        set = 100)
      cli_progress_done()
  }
           }
