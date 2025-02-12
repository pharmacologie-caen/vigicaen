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
#' @param path_base Character string, a directory containing vigibase txt tables. It is also the
#' output directory.
#' @param path_sub  Character string, a directory containing subsidiary tables.
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
#' @examples
#'
#' # --- Setting to try out ---- ####
#'
#' f_sets <-
#'  list(
#'    DEMO.txt = data.frame(f0= c("96548661   32194501051119460820")),
#'    DRUG.txt = data.frame(f0 =
#'              c("70548965   8          4901354    064392080055011    31- 806")
#'                ),
#'    LINK.txt = data.frame(
#'    f0 = c("2          654654     51---0.78991   0.98745    ",
#'           "2          456456     51---6.98789   -          ")),
#'    FOLLOWUP.txt = data.frame(f0 = c("0548978    0254687    ",
#'                                     "7568798    4565321    ")),
#'    ADR.txt = data.frame(f0 = c("96570161   14         100474561",
#'                                "70578465   17         145078144")),
#'    OUT.txt = data.frame(f0 = c("70547815   - N",
#'                                "96575661   - Y")),
#'    SRCE.txt = data.frame(f0 = c("4898765    1 ",
#'                                 "9804562    1 ")),
#'    IND.txt = data.frame(# 266 length
#'      f0 = paste0("780954     Cutaneous diseases due to other mycobacteria",
#'                  rep(" ", 211))
#'      ),
#'    SUSPECTEDDUPLICATES.txt = data.frame(f0 = c("789054     789542     ",
#'                                                "780546     654352     ")),
#'    AgeGroup_Lx.txt = data.frame(f0 = c("1An age range             ")),
#'    Dechallenge_Lx.txt = data.frame(f0 = paste0("1Some drug action",
#'                                                rep(" ", 237))),
#'    Dechallenge2_Lx.txt = data.frame(f0 = paste0("1Some outcome occurring",
#'                                                 rep(" ", 231))),
#'    Frequency_Lx.txt =
#'      data.frame(f0 =
#'                   paste0("123Some frequency of administration",
#'                          rep(" ", 221))),
#'    Gender_Lx.txt = data.frame(f0 = paste0("1Some gender",
#'                                           rep(" ", 242))),
#'    Notifier_Lx.txt = data.frame(f0 = paste0("1 Some notifier",
#'                                             rep(" ", 240))),
#'    Outcome_Lx.txt = data.frame(f0 = paste0("1Some outcome",
#'                                            rep(" ", 241))),
#'    Rechallenge_Lx.txt = data.frame(f0 = paste0("1A rechallenge action",
#'                                                rep(" ", 60))),
#'    Rechallenge2_Lx.txt =
#'      data.frame(f0 = paste0("1A reaction recurrence status",
#'                             rep(" ", 36))),
#'    Region_Lx.txt = data.frame(f0 = paste0("1A world region",
#'                                           rep(" ", 36))),
#'    RepBasis_Lx.txt = data.frame(f0 = paste0("1A reputation basis",
#'                                             rep(" ", 32))),
#'    ReportType_Lx.txt = data.frame(f0 = paste0("1A type of report",
#'                                               rep(" ", 237))),
#'    RouteOfAdm_Lx.txt = data.frame(f0 = paste0("1 A route of admnistration",
#'                                               rep(" ", 56))),
#'    Seriousness_Lx.txt = data.frame(f0 = paste0("1 Some seriousness criteria",
#'                                                rep(" ", 224))),
#'    SizeUnit_Lx.txt = data.frame(f0 = paste0("1 A dosing unit",
#'                                             rep(" ", 66)))
#'  )
#'
#' tmp_folder <- tempdir()
#'
#' path_base <- paste0(tmp_folder, "/", "main", "/")
#'
#' if(!dir.exists(path_base)) {dir.create(path_base)}
#'
#' path_sub  <- paste0(tmp_folder, "/", "sub",  "/")
#'
#' if(!dir.exists(path_sub)) {dir.create(path_sub)}
#'
#' purrr::iwalk(f_sets, function(d_, name_){
#'   if(name_ %in%
#'      c("SUSPECTEDDUPLICATES.txt",
#'        "AgeGroup_Lx.txt",
#'        "Dechallenge_Lx.txt",
#'        "Dechallenge2_Lx.txt",
#'        "Frequency_Lx.txt",
#'        "Gender_Lx.txt",
#'        "Notifier_Lx.txt",
#'        "Outcome_Lx.txt",
#'        "Rechallenge_Lx.txt",
#'        "Rechallenge2_Lx.txt",
#'        "Region_Lx.txt",
#'        "RepBasis_Lx.txt",
#'        "ReportType_Lx.txt",
#'        "RouteOfAdm_Lx.txt",
#'        "Seriousness_Lx.txt",
#'        "SizeUnit_Lx.txt")
#'   ){
#'     write.table(d_, file = paste0(path_sub, name_),
#'       row.names = FALSE, quote = FALSE, col.names = FALSE)
#'   } else {
#'     write.table(d_, file = paste0(path_base, name_),
#'       row.names = FALSE, quote = FALSE, col.names = FALSE)
#'   }
#' })
#'
#'  # ---- Running tb_vigibase
#'
#'  tb_vigibase(path_base = path_base,
#'         path_sub  = path_sub)


tb_vigibase <-
  function(path_base,
           path_sub
           ){

    path_base <-
      fix_path_endslash(path_base)

    path_sub <-
      fix_path_endslash(path_sub)

    if(!dir.exists(path_base)){
      stop(paste0(path_base, " does not exist"))
    }

    if(!dir.exists(path_sub)){
      stop(paste0(path_sub, " does not exist"))
    }

    cli::cli_h1(
      "tb_vigibase()"
    )
    cli::cli_alert_info(
    "Creating vigibase tables.")

    msg_tb_onceperdatabase()

    cli::cli_inform("It can take up to 30minutes.")

    # ---- demo ---- ####
    cli_progress_bar(
      "Creating vigibase",
      format = "{cli::pb_bar} {cli::pb_percent} | {cli::pb_elapsed} | {cli::pb_status}",
      total = 100
    )

    cli_progress_update(force = TRUE,status = "Read DEMO.txt", set = 3)

    demo <- reader("DEMO.txt", path_base)

    # ---- split
    cli_progress_update(force = TRUE,
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

    # ---- write
    cli_progress_update(force = TRUE,
      status = "Write demo.parquet",
      set = 12)

    arrow::write_parquet(demo,
                  sink = paste0(path_base, "demo.parquet")
    )

    rm(demo)

    gc()

    # ---- drug ---- ####
    cli_progress_update(force = TRUE,
      status = "Read DRUG.txt",
      set = 16)

    drug <- reader("DRUG.txt", path_base)

    # ---- split
    cli_progress_update(force = TRUE,
      status = "Split drug",
      set = 20)

    drug <-
      drug |>
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

    # ---- write
    cli_progress_update(force = TRUE,
      status = "Write drug.parquet",
      set = 27)

    arrow::write_parquet(drug,
                         sink = paste0(path_base, "drug.parquet")
    )

    rm(drug)

    gc()


    # ---- followup ---- ####
    cli_progress_update(force = TRUE,
      status = "Read FOLLOWUP.txt",
      set = 30)

    followup <- reader("FOLLOWUP.txt", path_base)

    # ---- split
    cli_progress_update(force = TRUE,
      status = "Split followup",
      set = 32)

    followup <-
      followup |>
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

    # ---- write
    cli_progress_update(force = TRUE,
      status = "Write followup.parquet",
      set = 34)

    arrow::write_parquet(followup,
                         sink = paste0(path_base, "followup.parquet")
    )

    rm(followup)

    gc()

    # ---- adr ---- ####
    cli_progress_update(force = TRUE,
      status = "Read ADR.txt",
      set = 36)

    adr <- reader("ADR.txt", path_base)

    # ---- split
    cli_progress_update(force = TRUE,
      status = "Split adr",
      set = 39)

    adr <-
      adr |>
      dplyr::transmute(
        UMCReportId = str_sub(.data$f0, start = 1L, end = 11L),
        Adr_Id      = str_sub(.data$f0, start = 12L, end = 22L),
        MedDRA_Id   = str_sub(.data$f0, start = 23L, end = 30L),
        Outcome     = str_sub(.data$f0, start = 31L, end = 31L)
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(c("UMCReportId", "Adr_Id", "MedDRA_Id")),
                      ~ .x |>
                        str_trim() |>
                        as.integer()
        )
      ) |>
      dplyr::compute()

    # ---- write
    cli_progress_update(force = TRUE,
      status = "Write adr.parquet",
      set = 42)

    arrow::write_parquet(adr,
                         sink = paste0(path_base, "adr.parquet")
    )

    # ---- out ---- ####
    cli_progress_update(force = TRUE,
      status = "Read OUT.txt",
      set = 43)

    out <- reader("OUT.txt", path_base)

    # ---- split
    cli_progress_update(force = TRUE,
      status = "Split out",
      set = 45)

    out <-
      out |>
      dplyr::transmute(
        UMCReportId = str_sub(.data$f0, start = 1L, end = 11L),
        Seriousness = str_trim(str_sub(.data$f0, start = 12L, end = 13L)),
        Serious     = str_sub(.data$f0, start = 14L, end = 14L)
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(c("UMCReportId")),
                      ~ .x |>
                        str_trim() |>
                        as.integer()
        )
      ) |>
      dplyr::compute()

    # ---- write
    cli_progress_update(force = TRUE,
      status = "Write out.parquet",
      set = 46)

    arrow::write_parquet(out,
                         sink = paste0(path_base, "out.parquet")
    )


    rm(out)

    gc()

    # ---- srce ---- ####
    cli_progress_update(force = TRUE,
      status = "Read SRCE.txt",
      set = 47)

    srce <- reader("SRCE.txt", path_base)

    # ---- split
    cli_progress_update(force = TRUE,
      status = "Split srce",
      set = 48)

    srce <-
      srce |>
      dplyr::transmute(
        UMCReportId = str_sub(.data$f0, start = 1L, end = 11L),
        Type        = str_trim(str_sub(.data$f0, start = 12L, end = 13L))
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(c("UMCReportId")),
                      ~ .x |>
                        str_trim() |>
                        as.integer()
        )
      ) |>
      dplyr::compute()

    # ---- write
    cli_progress_update(force = TRUE,
      status = "Write srce.parquet",
      set = 49)

    arrow::write_parquet(srce,
                         sink = paste0(path_base, "srce.parquet")
    )

    rm(srce)

    gc()

    # ---- link ---- ####
    cli_progress_update(force = TRUE,
      status = "Read LINK.txt",
      set = 50)

    link <- reader("LINK.txt", path_base)

    # ---- split
    cli_progress_update(force = TRUE,
      status = "Split link (longest step)",
      set = 52)

    link <-
      link |>
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

    # ---- write
    cli_progress_update(force = TRUE,
      status = "Write link.parquet",
      set = 68)

    arrow::write_parquet(link,
                         sink = paste0(path_base, "link.parquet")
    )

    rm(adr, link)
    gc()

    # ---- ind ---- ####
    cli_progress_update(force = TRUE,
      status = "Read IND.txt",
      set = 70)

    ind <- arrow::read_delim_arrow(paste0(path_base, "IND.txt"),
                                   col_names = FALSE,
                                   as_data_frame = FALSE,
                                   delim = "\t",
                                   read_options =
                                     arrow::csv_read_options(
                                       column_names = "f0",
                                       encoding = "ANSI_X3.4-1986")
    )

    # ---- split
    cli_progress_update(force = TRUE,
      status = "Split ind",
      set = 72)

    ind <-
      ind |>
      dplyr::transmute(
        Drug_Id    = str_sub(.data$f0, start = 1L,  end = 11L),
        Indication = str_trim(str_sub(.data$f0, start = 12L, end = 266L))
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(c("Drug_Id")),
                      ~ .x |>
                        str_trim() |>
                        as.integer()
        )
      ) |>
      dplyr::compute()

    # ---- write
    cli_progress_update(force = TRUE,
      status = "Write ind.parquet",
      set = 78)

    arrow::write_parquet(ind,
                         sink = paste0(path_base, "ind.parquet")
    )

    rm(ind)
    gc()

    # ---- suspectedduplicates ---- ####
    cli_progress_update(force = TRUE,
      status = "Read SUSPECTEDDUPLICATES.txt",
      set = 80)

    suspdup <- reader("SUSPECTEDDUPLICATES.txt",
                      folder = path_sub)

    # ---- split
    cli_progress_update(force = TRUE,
      status = "Split suspdup",
      set = 82)

    suspdup <-
      suspdup |>
      dplyr::transmute(
        UMCReportId                = str_sub(.data$f0, start = 1L,  end = 11L),
        SuspectedduplicateReportId = str_sub(.data$f0, start = 12L, end = 22L)
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(c("UMCReportId", "SuspectedduplicateReportId")),
                      ~ .x |>
                        str_trim() |>
                        as.integer()
        )
      ) |>
      dplyr::compute()

    # ---- write
    cli_progress_update(force = TRUE,
      status = "Write suspdup.parquet",
      set = 84)

    arrow::write_parquet(suspdup,
                         sink = paste0(path_base, "suspdup.parquet")
    )


    rm(suspdup)
    gc()

    # AgeGroup
    cli_progress_update(force = TRUE,
      status = "Process AgeGroup_Lx.txt",
      set = 85)

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
    cli_progress_update(force = TRUE,
      status = "Process Dechallenge_Lx.txt",
      set = 86)

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
    cli_progress_update(force = TRUE,
      status = "Process Dechallenge2_Lx.txt",
      set = 87)

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
    cli_progress_update(force = TRUE,
      status = "Process Frequency_Lx.txt",
      set = 88)

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
    cli_progress_update(force = TRUE,
      status = "Process Gender_Lx.txt",
      set = 89)

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
    cli_progress_update(force = TRUE,
      status = "Process Notifier_Lx.txt",
      set = 90)

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
    cli_progress_update(force = TRUE,
      status = "Process Outcome_Lx.txt",
      set = 91)

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
    cli_progress_update(force = TRUE,
      status = "Process Rechallenge_Lx.txt",
      set = 92)

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
    cli_progress_update(force = TRUE,
      status = "Process Rechallenge2_Lx.txt",
      set = 93)

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
    cli_progress_update(force = TRUE,
      status = "Process Region_Lx.txt",
      set = 94)

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
    cli_progress_update(force = TRUE,
      status = "Process RepBasis_Lx.txt",
      set = 95)

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
    cli_progress_update(force = TRUE,
      status = "Process ReportType_Lx.txt",
      set = 96)

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
    cli_progress_update(force = TRUE,
      status = "Process RouteOfAdm_Lx.txt",
      set = 97)

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
    cli_progress_update(force = TRUE,
      status = "Process Seriousness_Lx.txt",
      set = 98)

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
    cli_progress_update(force = TRUE,
      status = "Process SizeUnit_Lx.txt",
      set = 99)

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

    cli_progress_update(force = TRUE,
      status = "Done",
      set = 100)
    cli_progress_done()
  }
