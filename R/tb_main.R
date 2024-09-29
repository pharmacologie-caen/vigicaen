#' Create main VigiBase ECL tables
#'
#' @description `r lifecycle::badge('stable')` `tb_main()` transforms .txt
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
#'
#' @export
#'
#' @seealso [tb_who()], [tb_sub()], [tb_meddra()], [tb_custom()], [dt_parquet()]
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
#'    DRUG.txt = data.frame(f0 = c("70548965   8          4901354    064392080055011    31- 806")
#'                          ),
#'    LINK.txt = data.frame(f0 = c("2          654654     51---0.78991   0.98745    ",
#'                                 "2          456456     51---6.98789   -          ")),
#'    FOLLOWUP.txt = data.frame(f0 = c("0548978    0254687    ",
#'                                     "7568798    4565321    ")),
#'    ADR.txt = data.frame(f0 = c("96570161   14         100474561",
#'                                "70578465   17         145078144")),
#'    OUT.txt = data.frame(f0 = c("70547815   - N",
#'                                "96575661   - Y")),
#'    SRCE.txt = data.frame(f0 = c("4898765    1 ",
#'                                 "9804562    1 ")),
#'    IND.txt = data.frame(# 266 length
#'      f0 = paste0("780954     Cutaneous diseases due to other mycobacteria", rep(" ", 211))
#'      ),
#'    SUSPECTEDDUPLICATES.txt = data.frame(f0 = c("789054     789542     ",
#'                                                "780546     654352     "))
#'  )
#'
#' tmp_folder <- tempdir()
#'
#' path_base <- paste0(tmp_folder, "/", "main", "/")
#'
#' dir.create(path_base)
#'
#' path_sub  <- paste0(tmp_folder, "/", "sub",  "/")
#'
#' dir.create(path_sub)
#'
#' purrr::iwalk(f_sets, function(d_, name_){
#'   if(name_ == "SUSPECTEDDUPLICATES.txt"){
#'     write.table(d_, file = paste0(path_sub, name_), row.names = FALSE,
#'                 quote = FALSE, col.names = FALSE)
#'   } else {
#'     write.table(d_, file = paste0(path_base, name_), row.names = FALSE,
#'                 quote = FALSE, col.names = FALSE)
#'   }
#' })
#'
#'  # ---- Running tb_main
#'
#'  tb_main(path_base = path_base,
#'         path_sub  = path_sub)


tb_main <-
  function(path_base,
           path_sub
           ){

    # helps working with the "here" package, or tempdir

    if(!grepl("(/|\\\\)$", path_base, perl = TRUE)){
      path_base <-
        paste0(path_base, "/")

      path_sub <-
        paste0(path_sub, "/")
    }

    # ---- demo ---- ####
    texter("Read DEMO.txt", "3%%")

    demo <- reader("DEMO.txt", path_base)

    # ---- split
    texter("Split demo", "6%%")

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
    texter("Write demo.parquet", "12%%")

    arrow::write_parquet(demo,
                  sink = paste0(path_base, "demo.parquet")
    )

    # ---- drug ---- ####
    texter("Read DRUG.txt", "16%%")

    drug <- reader("DRUG.txt", path_base)

    # ---- split
    texter("Split drug", "20%%")

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
        dplyr::across(dplyr::all_of(c("UMCReportId", "Drug_Id", "MedicinalProd_Id")),
               ~ .x |>
          str_trim() |>
          as.integer()
        )
      ) |>
      dplyr::compute()

    # ---- write
    texter("Write drug.parquet", "27%%")

    arrow::write_parquet(drug,
                         sink = paste0(path_base, "drug.parquet")
    )


    # ---- followup ---- ####
    texter("Read FOLLOWUP.txt", "30%%")

    followup <- reader("FOLLOWUP.txt", path_base)

    # ---- split
    texter("Split followup", "32%%")

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
    texter("Write followup.parquet", "34%%")

    arrow::write_parquet(followup,
                         sink = paste0(path_base, "followup.parquet")
    )

    # ---- adr ---- ####
    texter("Read ADR.txt", "36%%")

    adr <- reader("ADR.txt", path_base)

    # ---- split
    texter("Split adr", "41%%")

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
    texter("Write adr.parquet", "48%%")

    arrow::write_parquet(adr,
                         sink = paste0(path_base, "adr.parquet")
    )

    # ---- out ---- ####
    texter("Read OUT.txt", "51%%")

    out <- reader("OUT.txt", path_base)

    # ---- split
    texter("Split out", "53%%")

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
    texter("Write out.parquet", "55%%")

    arrow::write_parquet(out,
                         sink = paste0(path_base, "out.parquet")
    )

    # ---- srce ---- ####
    texter("Read SRCE.txt", "57%%")

    srce <- reader("SRCE.txt", path_base)

    # ---- split
    texter("Split srce", "59%%")

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
    texter("Write srce.parquet", "61%%")

    arrow::write_parquet(srce,
                         sink = paste0(path_base, "srce.parquet")
    )

    # ---- link ---- ####
    texter("Read LINK.txt", "63%%")

    link <- reader("LINK.txt", path_base)

    # ---- split
    texter("Split link", "68%%")

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
                      ~ dplyr::if_else(.x == 1568459784.65489, NA_real_, .x))

        )|>
      dplyr::compute()

    # ---- write
    texter("Write link.parquet", "75%%")

    arrow::write_parquet(link,
                         sink = paste0(path_base, "link.parquet")
    )

    # ---- ind ---- ####
    texter("Read IND.txt", "78%%")

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
    texter("Split ind", "88%%")

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
    texter("Write ind.parquet", "92%%")

    arrow::write_parquet(ind,
                         sink = paste0(path_base, "ind.parquet")
    )

    # ---- suspectedduplicates ---- ####
    texter("Read SUSPECTEDDUPLICATES.txt", "95%%")

    suspdup <- reader("SUSPECTEDDUPLICATES.txt",
                      folder = path_sub)

    # ---- split
    texter("Split suspdup", "97%%")

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
    texter("Write suspdup.parquet", "99%%")

    arrow::write_parquet(suspdup,
                         sink = paste0(path_base, "suspdup.parquet")
    )

    texter("Done", "")
  }
