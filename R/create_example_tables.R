#' Example source tables for VigiBase and MedDRA
#'
#' @description `r lifecycle::badge('experimental')` Write some example
#' tables as source text/ascii/parquet files.
#'
#' @details VigiBase tables and MedDRA tables are provided respectively
#' as text files and ascii files. The `tb_*` family turns them
#' into parquet files. These `create_example_*` functions are only used to produce
#' example source files to illustrate the `tb_*` family, and parquet files for the
#' same purpose.
#'
#' @param path Character string. A folder on your computer where the tables should
#' be written.
#' @importFrom utils write.table
#'
#' @returns A set of text/ascii files,
#' as received by the Uppsala Monitoring Centre or MedDRA
#' \itemize{
#'  \item For [create_ex_main_txt()],
#'  DEMO.txt, DRUG.txt, LINK.txt, FOLLOWUP.txt,
#'  ADR.txt, OUT.txt, SRCE.txt, and IND.txt
#'  \item For [create_ex_sub_txt()],
#'  AgeGroup_Lx.txt, Dechallenge_Lx.txt, Dechallenge2_Lx.txt,
#'  Frequency_Lx.txt, Gender_Lx.txt, Notifier_Lx.txt, Outcome_Lx.txt,
#'  Rechallenge_Lx.txt, Rechallenge2_Lx.txt, Region_Lx.txt, RepBasis_Lx.txt,
#'  ReportType_Lx.txt, RouteOfAdm_Lx.txt, Seriousness_Lx.txt,
#'  and SizeUnit_Lx.txt
#'  \item For [create_ex_who_txt()],
#'  ATC.txt, CCODE.txt, ING.txt, MP.txt, ORG.txt,
#'  PF.txt, PP.txt, PRT.txt, PRG.txt, SRCE.txt, STR.txt,
#'  SUN.txt, ThG.txt, and Unit-X.txt
#'  \item For [create_ex_meddra_asc()],
#'  llt.asc, mdhier.asc, smq_content.asc, smq_list.asc
#'  \item For [create_ex_main_pq()],
#'  demo.parquet, adr.parquet, drug.parquet, link.parquet,
#'  srce.parquet, ind.parquet, out.parquet, followup.parquet, suspdup.parquet
#'  }
#' @seealso [tb_vigibase()], [tb_who()], [tb_meddra()]
#' @export
#'
#' @name create_example_tables
#'
#' @examples
#'
#' path <- paste0(tempdir(), "/crex/")
#'
#' dir.create(path)
#'
#' # You may want to use different paths for each type of tables
#' create_ex_main_txt(path)
#'
#' create_ex_sub_txt(path)
#'
#' create_ex_who_txt(path)
#'
#' create_ex_meddra_asc(path)
#'
#' create_ex_main_pq(path)
#'
#' # Remove temporary folders when you're done
#' unlink(path, recursive = TRUE)

create_ex_main_txt <-
  function(
    path
  ){
    purrr::iwalk(f_sets_main(), function(d_, name_)
        write.table(d_, file = paste0(path, name_),
          row.names = FALSE, quote = FALSE, col.names = FALSE)
    )
  }

#' @describeIn create_example_tables sub txt tables
#' @export

create_ex_sub_txt <-
  function(
    path
  ){
    purrr::iwalk(f_sets_sub(), function(d_, name_)
      write.table(d_, file = paste0(path, name_),
                  row.names = FALSE, quote = FALSE, col.names = FALSE)
    )
  }

#' @describeIn create_example_tables WHO txt tables
#' @export

create_ex_who_txt <-
  function(
    path
  ){

    purrr::iwalk(f_sets_who(), function(d_, name_)
      write.table(d_, file = paste0(path, name_),
                  row.names = FALSE, quote = FALSE, col.names = FALSE)
    )
  }

#' @describeIn create_example_tables MedDRA txt tables
#' @export

create_ex_meddra_asc <-
  function(
    path
  ){
    purrr::iwalk(f_sets_meddra(), function(d_, name_)
      write.table(d_, file = paste0(path, name_),
                  row.names = FALSE, quote = FALSE, col.names = FALSE)
    )
  }

#' @describeIn create_example_tables main parquet tables
#' @export

create_ex_main_pq <-
  function(
    path
  ){
    purrr::iwalk(
      f_sets_main_pq(),
      function(dataset, name)
        arrow::write_parquet(
          dataset |>
            arrow::as_arrow_table(),
          sink = paste0(path, "/", name, ".parquet")
        )

    )
  }

# Helpers ------

# File sets for each type of table

f_sets_main <-
  function() {
    list(
      DEMO.txt = data.frame(f0 = c(
        "10000001   32194501051119460820", # Not duplicate
        "10000002   32194501051119460820"  # Duplicate
      )),
      DRUG.txt = data.frame(f0 =
                              c(
                                "10000001   8          4901354    064392080055011    31- 806", # Not duplicate
                                "10000002   9          4901355    064392080055012    32- 807"  # Duplicate
                              )),
      LINK.txt = data.frame(f0 = c(
        "8          17     51---0.78991   0.98745    ",  # Not duplicate Drug_Id
        "9          14     51---6.98789   -          "   # Duplicate Drug_Id
      )),
      FOLLOWUP.txt = data.frame(f0 = c("0548978    0254687    ", "7568798    4565321    ")),
      ADR.txt = data.frame(f0 = c(
        "96570161   14         100474561", "70578465   17         145078144"
      )),
      OUT.txt = data.frame(f0 = c("70547815   - N", "96575661   - Y")),
      SRCE.txt = data.frame(f0 = c("4898765    1 ", "9804562    1 ")),
      IND.txt = data.frame(
        # 266 length
        f0 = paste0(
          "780954     Cutaneous diseases due to other mycobacteria",
          rep(" ", 211)
        )
      )
    )
  }

f_sets_main_pq <-
  function() {
    rlang::list2(
      demo =
        data.table(
          UMCReportId = c(1, 2, 3, 4),
          AgeGroup = c(1, 2, 7, 9)
        ),
      drug =
        data.table(
          UMCReportId = c(1, 2, 3, 4),
          Drug_Id = c("d1", "d2", "d3", "d4"),
          DrecNo = c(133138448, 133138448, 111841511, 111841511),
          MedicinalProd_Id = c(25027716, 97354576, 104264760, 37484408)
        ),
      adr  =
        data.table(
          UMCReportId = c(1, 2, 3, 4),
          Adr_Id = c("a1", "a2", "a3", "a4"),
          MedDRA_Id = c(110049083, 146319904, 146319904, 72535511)
        ),
      link =
        data.table(
          Drug_Id = c("d1", "d2", "d3", "d4"),
          Adr_Id = c("a1", "a2", "a3", "a4")
        ),
      srce =
        data.table(UMCReportId = c(1, 2, 3, 4)),
      ind  =
        data.table(Drug_Id = c("d1", "d2", "d3", "d4")),
      out  =
        data.table(UMCReportId = c(1, 2, 3, 4)),
      followup =
        data.table(UMCReportId = c(1, 2, 3, 4)),
      suspdup =
        data.table(
          UMCReportId = c(3),
          SuspectedduplicateReportId = c(4)
        )
    )
  }

f_sets_sub <-
  function() {
    list(
      SUSPECTEDDUPLICATES.txt = data.frame(f0 = c("10000001    10000002 ")),
      AgeGroup_Lx.txt = data.frame(f0 = c("1An age range             ")),
      Dechallenge_Lx.txt = data.frame(f0 = paste0("1Some drug action", rep(" ", 237))),
      Dechallenge2_Lx.txt = data.frame(f0 = paste0("1Some outcome occurring", rep(" ", 231))),
      Frequency_Lx.txt =
        data.frame(f0 =
                     paste0(
                       "123Some frequency of administration", rep(" ", 221)
                     )),
      Gender_Lx.txt = data.frame(f0 = paste0("1Some gender", rep(" ", 242))),
      Notifier_Lx.txt = data.frame(f0 = paste0("1 Some notifier", rep(" ", 240))),
      Outcome_Lx.txt = data.frame(f0 = paste0("1Some outcome", rep(" ", 241))),
      Rechallenge_Lx.txt = data.frame(f0 = paste0("1A rechallenge action", rep(" ", 60))),
      Rechallenge2_Lx.txt =
        data.frame(f0 = paste0(
          "1A reaction recurrence status", rep(" ", 36)
        )),
      Region_Lx.txt = data.frame(f0 = paste0("1A world region", rep(" ", 36))),
      RepBasis_Lx.txt = data.frame(f0 = paste0("1A reputation basis", rep(" ", 32))),
      ReportType_Lx.txt = data.frame(f0 = paste0("1A type of report", rep(" ", 237))),
      RouteOfAdm_Lx.txt = data.frame(f0 = paste0("1 A route of admnistration", rep(" ", 56))),
      Seriousness_Lx.txt = data.frame(f0 = paste0(
        "1 Some seriousness criteria", rep(" ", 224)
      )),
      SizeUnit_Lx.txt = data.frame(f0 = paste0("1 A dosing unit", rep(" ", 66)))
    )
  }

f_sets_who <-
  function() {
    # just in case you want to remember how you made this list

    # run each reading from an example dataset, without processing it.
    #df_expr <- list(atc  ,   ccode ,   ing    ,
    #                mp    ,   org ,   pf   ,
    #                pp    ,   prt    ,  srce,
    #                str ,   sun  ,   thg   ,
    #                unitx) |>
    #  rlang::set_names(
    #    "atc",  "ccode", "ing",
    #    "mp",   "org",   "pf",
    #    "pp",   "prt",   "srce",
    #    "str",  "sun",   "thg",
    #    "unitx"
    #  ) |>
    #  rlang::set_names(~ paste0(stringr::str_to_upper(.x), ".txt")) |>
    #  purrr::map(
    #    function(d_){
    #
    #      string <- d_ |> head(1) |> collect() |> pull()
    #      rlang::expr(data.frame(f0 = !!string))
    #    }
    #  )
    #rlang::call2(expr(list2), !!!df_expr)

    list(
      ATC.txt = data.frame(f0 = "A         1ALIMENTARY TRACT AND METABOLISM                                                                               "),
      CCODE.txt = data.frame(f0 = "ABW       Aruba                                                                           "),
      ING.txt = data.frame(f0 = "1         198512312301                                    38        1         1         "),
      MP.txt = data.frame(f0 = "1                                            0000010100100000000010000000001YMethyldopa                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              N/A                 0         001       N/A          001       0         1985123120170907"),
      ORG.txt = data.frame(f0 = "0         None                                                                            UNS       "),
      PF.txt = data.frame(f0 = "001       Unspecified                                                                     "),
      PP.txt = data.frame(f0 = "1         001                 1         0119851231"),
      PRT.txt = data.frame(f0 = "001       Medicinal product                                                               "),
      PRG.txt = data.frame(f0 = "0         None                                                        20020701"),
      SRCE.txt = data.frame(f0 = "001       INN - International Nonproprietary Names - WHO                                  N/A       "),
      STR.txt = data.frame(f0 = "000001    Unspecified                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         "),
      SUN.txt = data.frame(f0 = "1         0000050000EN        Formaldehyde solution                                                                                            180       "),
      ThG.txt = data.frame(f0 = "100007    N06BA     19890630Y40683     "),
      `Unit-X.txt` = data.frame(f0 = "01        kg                                      ")
    )

  }

f_sets_meddra <-
  function() {
    list(
      llt.asc = data.frame(
        f0 = c(
          '486813518$A first LLT name$6548988$$$$$$$N$$',
          '568798788$A second LLT name$47984764$$$$$$$Y$$'
        )
      ),
      mdhier.asc = data.frame(
        f0 = c(
          "6548988$84068465$98765468$8746878$A PT name$An HLT class$An HLGT class$A SOC class$A SOC class abrev$$12365489$Y$",
          "47984764$7852085$6852245$2358467$A 2nd PT name$HLT class$HLGT class$SOC class$SOC abrev$$10005329$Y$"
        )
      ),
      smq_content.asc = data.frame(
        f0 = c(
          "20486512$6548641$5$1$A$0$A$7.1$7.1$",
          "20904441$98143546$5$1$A$0$A$7.1$7.1$"
        )
      ),
      smq_list.asc = data.frame(
        f0 = c(
          "20486512$The SMQ name (SMQ)$1$Long text describing this SMQ to provide context to the user$References$$26.1$A$N$",
          "20904441$Another name (SMQ)$1$Another long text$References$$26.1$A$N$"
        )
      )
    )
  }
