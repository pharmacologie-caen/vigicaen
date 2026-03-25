#' Example source tables for VigiBase and MedDRA
#'
#' @description `r lifecycle::badge('stable')` Write some example
#' tables as source csv/ascii/parquet files.
#'
#' @details VigiBase tables and MedDRA tables are provided respectively
#' as csv files and ascii files. The `tb_*` family turns them
#' into parquet files. These `create_example_*` functions are only used to produce
#' example source files to illustrate the `tb_*` family, and parquet files for the
#' same purpose. Note that there is a little difference among main and sub
#' tables, whether created in csv or parquet, as suspected duplicates moves
#' from sub to main during `tb_vigibase()` process.
#'
#' @param path Character string. A folder on your computer where the tables should
#' be written.
#' @importFrom utils write.table
#'
#' @returns A set of text/ascii files,
#' as received by the Uppsala Monitoring Centre or MedDRA
#' \itemize{
#'  \item For [create_ex_main_csv()],
#'  DEMO.csv, DRUG.csv, LINK.csv, FOLLOWUP.csv,
#'  ADR.csv, OUT.csv, SRCE.csv, and IND.csv
#'  \item For [create_ex_sub_csv()],
#'  AgeGroup_Lx.csv, Dechallenge_Lx.csv, Dechallenge2_Lx.csv,
#'  Frequency_Lx.csv, Gender_Lx.csv, Notifier_Lx.csv, Outcome_Lx.csv,
#'  Rechallenge_Lx.csv, Rechallenge2_Lx.csv, Region_Lx.csv, RepBasis_Lx.csv,
#'  ReportType_Lx.csv, RouteOfAdm_Lx.csv, Seriousness_Lx.csv, SizeUnit_Lx.csv,
#'  and SUSPECTEDDUPLICATES.csv
#'  \item For [create_ex_who_csv()],
#'  ATC.csv, CCODE.csv, ING.csv, MP.csv, ORG.csv,
#'  PF.csv, PP.csv, PRT.csv, PRG.csv, SRCE.csv, STR.csv,
#'  SUN.csv, ThG.csv, and Unit-X.csv
#'  \item For [create_ex_meddra_asc()],
#'  llt.asc, mdhier.asc, smq_content.asc, smq_list.asc
#'  \item For [create_ex_main_pq()],
#'  demo.parquet, adr.parquet, drug.parquet, link.parquet,
#'  srce.parquet, ind.parquet, out.parquet, followup.parquet, suspdup.parquet
#'  \item For [create_ex_sub_pq()],
#'  agegroup.parquet, dechallenge.parquet, dechallenge2.parquet,
#'  frequency.parquet, gender.parquet, notifier.parquet, outcome.parquet,
#'  rechallenge.parquet, rechallenge2.parquet, region.parquet, repbasis.parquet,
#'  reporttype.parquet, routeofadm.parquet, seriousness.parquet,
#'  and sizeunit.parquet
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
#'
#' create_ex_meddra_asc(path)
#'
#' create_ex_main_pq(path)
#'
#' create_ex_sub_pq(path)
#'
#' create_ex_main_csv(path)
#'
#' create_ex_who_csv(path)
#'
#' create_ex_sub_csv(path)
#'
#' # Remove temporary folders when you're done
#' unlink(path, recursive = TRUE)

create_ex_meddra_asc <-
  function(
    path
  ){
    path_fix <- fix_path_endslash(path)
    purrr::iwalk(f_sets_meddra(), function(d_, name_)
      write.table(d_, file = paste0(path_fix, name_),
                  row.names = FALSE, quote = FALSE, col.names = FALSE)
    )
  }

#' @describeIn create_example_tables main parquet tables
#' @export

create_ex_main_pq <-
  function(
    path
  ){
    path_fix <- fix_path_endslash(path)
    purrr::iwalk(
      f_sets_main_pq(),
      function(dataset, name)
        arrow::write_parquet(
          dataset |>
            arrow::as_arrow_table(),
          sink = paste0(path_fix, "/", name, ".parquet")
        )

    )
  }

#' @describeIn create_example_tables subsidiary parquet tables
#' @export

create_ex_sub_pq <-
  function(
    path
  ){
    path_fix <- fix_path_endslash(path)
    purrr::iwalk(
      f_sets_sub_pq(),
      function(dataset, name){
        name_pq <- name |> stringr::str_replace("_Lx$", "")

        arrow::write_parquet(
          dataset |>
            arrow::as_arrow_table(),
          sink = paste0(path_fix, "/", name_pq, ".parquet")
        )
      }
    )
  }

#' @describeIn create_example_tables main csv tables
#' @export

create_ex_main_csv <-
  function(
    path
  ){
    path_fix <- fix_path_endslash(path)

    purrr::iwalk(f_sets_main(), function(d_, name_){

      name_upper <- name_ |>
        stringr::str_to_upper()

      name_csv <- paste0(name_upper, ".csv")

      name_csv <-
        ifelse(
          name_csv == "SUSPDUP.csv",
          "SUSPECTEDDUPLICATES.csv",
          name_csv
        )

      write.table(d_, file = paste0(path_fix, name_csv),
                  sep = ",", dec = ".",
                  row.names = FALSE, quote = TRUE, col.names = FALSE)
    })
  }

#' @describeIn create_example_tables who csv tables
#' @export

create_ex_who_csv <-
  function(
    path
  ){
    path_fix <- fix_path_endslash(path)

    purrr::iwalk(f_sets_who(), function(d_, name_){

      name_upper <- name_ |>
        stringr::str_to_upper()

      name_csv <- paste0(name_upper, ".csv")

      name_csv <-
        ifelse(
          name_csv == "UNITX.csv",
          "UNIT-X.csv",
          name_csv
        )

      write.table(d_, file = paste0(path_fix, name_csv),
                  sep = ",", dec = ".",
                  row.names = FALSE, quote = TRUE, col.names = FALSE)
    })
  }

#' @describeIn create_example_tables sub csv tables
#' @export

create_ex_sub_csv <-
  function(
    path
  ){
    path_fix <- fix_path_endslash(path)

    purrr::iwalk(f_sets_sub(), function(d_, name_){

      name_csv <- paste0(name_, ".csv")

      write.table(d_, file = paste0(path_fix, name_csv),
                  sep = ",", dec = ".",
                  row.names = FALSE, quote = TRUE, col.names = FALSE)
    })
  }


# Helpers ------

# File sets for each type of table

f_sets_main <-
  function() {
    rlang::list2(
      demo =
        data.table(
          UMCReportId = c(10000001, 10000002, 3, 4),
          AgeGroup = c(1, 2, 7, 9) |> as.character(),
          Gender  = c(1, 2, 1, 1) |> as.character(),
          DateDatabase = c(20200101, 20200101, 20200101, 20200101) |> as.character(),
          Type = c(2, 1, 1, 2) |> as.character(),
          Region = c(2, 4, 2, 4) |> as.character(),
          FirstDateDatabase = c(20200101, 20200101, 20200101, 20200101) |> as.character()
        ),
      drug =
        data.table(
          UMCReportId = c(10000001, 10000002),
          Drug_Id = c(8, 9),
          Record_Id = c(97354576, 104264760),
          DrecNo = c(133138448, 111841511),
          Seq1 = c("01", "-"),
          Seq2 = c("001", "-"),
          Route = c("41", "42"),
          Basis = c("1", "1"),
          Amount = c("840", "-"),
          AmountU = c("9", "-"),
          Frequency = c("-", "-"),
          FrequencyU = c("803", "-")
        ),
      adr  =
        data.table(
          UMCReportId = c(10000001, 10000002),
          Adr_Id = c(17L, 14L),
          MedDRA_Id = c(110049083, 146319904),
          Outcome = c("3", "4")
        ),
      link =
        data.table(
          Drug_Id = c(8, 9),
          Adr_Id = c(17L, 14L),
          Dechallenge1 = c("1", "2"),
          Dechallenge2 = c("3", "5"),
          Rechallenge1 = c("1", "1"),
          Rechallenge2 = c("3", "3"),
          TimeToOnsetMin = c(725.9, 680),
          TimeToOnsetMax = c(730.9, 681)
        ),
      srce =
        data.table(UMCReportId = c(10000001, 10000002),
                   Type = c(1, 1) |> as.character()),
      ind  =
        data.table(Drug_Id = c(8, 9),
                   Indication = c("One indication", "Another ind")),
      out  =
        data.table(UMCReportId = c(10000001, 10000002),
                   Seriousness = c("-", "6"),
                   Serious = c("N", "Y")),
      followup =
        data.table(UMCReportId = c(10000001, 10000002),
                   ReplacedUMCReportId = c(2, 3))
    )
  }

f_sets_main_pq <-
  function() {
    f_sets <- f_sets_main()

    # link is modified between csv and parquet

    f_sets$link <-
      f_sets$link |>
      dplyr::mutate(
        tto_mean = c(728.40, 680.5),
        range    = c(2.5, 0.5),
        UMCReportId = c(10000001L, 10000002L)
      )

    # suspdup is moved from sub to main

    f_sets$suspdup <-
      data.table(
        UMCReportId = c(10000001L),
        SuspectedduplicateReportId = c(10000002)
      )

    return(f_sets)
  }

f_sets_who <-
  function() {
      rlang::list2(
        atc = data.table(
          ATC.code = "A",
          Level = 1,
          Text = "ALIMENTARY TRACT AND METABOLISM"
        ),
        ccode = data.table(
          Country.code = "ABW",
          Country.Name = "Aruba"
        ),
        ing = data.table(
          Ingredient_Id      = 1L,
            Create.date      = 19851231,
            Substance_Id     = 2,
            Quantity         = 3,
            Quantity.2       = 0,
            Unit             = 1,
            Record_Id        = 1
        ),
        mp = data.table(
          Record_Id   = 1,
          Umc_Product_Id     = 1,
          Drug.record.number = 101001,
          Sequence.number.1  = "0",
          Sequence.number.2  = "1",
          Sequence.number.3  = "1",
          Sequence.number.4  = "1",
          Substance_or_substance_synonym = "Y",
          Drug.name          = "Methyldopa",
          Name.specifier     = 1,
          Marketing.Authorization.Number = NA_real_,
          Marketing.Authorization.Date = NA_real_,
          Marketing.Authorization.Withdrawal.date = NA_real_,

          Country = "ABW",
          Company = 1,
          Marketing.Authorization.Holder = 1,
          Reference.code    = 1,
          Source.country    = 1,
          Year.of.Reference = 1985,
          Product.type      = 1,
          Create.date       = "19850101",
          Date.changed      = NA_character_
        ),
        org = data.table(
          Organization_Id = 0L,
          Name = "None",
          Country.code = "UNS"
        ),
        pf = data.table(
          Pharmform_Id = 1L,
          Text = "Unspecified"
        ),
        prt = data.table(
          Prodtype_Id = 1L,
          Text = "Medicinal product"
        ),
        srce = data.table(
          Reference.code = 1L,
          Reference = "INN - International Nonproprietary Names - WHO",
          Country.code = "N/A"
        ),
        str = data.table(
          Strength_Id = 1L,
          Text = "Unspecified"
        ),
        sun = data.table(
          Substance_Id      = 1L,
          CAS.number        = 50000L,
          Language.code     = "EN",
          Substance.name    = "Formaldehyde solution",
          Year.of.Reference = NA_integer_,
          Reference.code    = 183L
        ),
        thg = data.table(
          Therapgroup_Id = 100007L,
          ATC.code = "N06BA",
          Create.date = 19890630L,
          Official.ATC.code = "Y",
          Record_Id = 40683L
        ),
        unitx = data.table(
          Unit_Id = 1L,
          Text = "kg"
        )
      )
    }

f_sets_who_pq <-
  function() {
    f_sets <- f_sets_who()

    f_sets$mp <-
      f_sets$mp |>
      dplyr::mutate(
        DrecNo = .data$Drug.record.number,
        drug_name_t = .data$Drug.name |>
          str_trim() |>
          stringr::str_to_lower()
      ) |>
      dplyr::select(
        dplyr::all_of(c(
          "Record_Id",
          "Sequence.number.1",
          "Sequence.number.2",
          "DrecNo",
          "drug_name_t",
          "Create.date",
          "Date.changed",
          "Country")
        ))

    return(f_sets)
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

f_sets_sub <-
  function() {
    rlang::list2(
      AgeGroup_Lx     = data.table(Code = "1", Text = c("An age range")),
      Dechallenge_Lx  = data.table(Code = "1", Text = c("Some drug action")),
      Dechallenge2_Lx = data.table(Code = "1", Text = c("Some outcome occurring")),
      Frequency_Lx    = data.table(Code = "123", Text = c("Some frequency of administration")),
      Gender_Lx       = data.table(Code = "1", Text = c("Some gender")),
      Notifier_Lx     = data.table(Code = "1", Text = c("Some notifier")),
      Outcome_Lx      = data.table(Code = "1", Text = c("Some outcome")),
      Rechallenge_Lx  = data.table(Code = "1", Text = c("A rechallenge action")),
      Rechallenge2_Lx = data.table(Code = "1", Text = c("A reaction recurrence status")),
      Region_Lx       = data.table(Code = "1", Text = c("A world region")),
      RepBasis_Lx     = data.table(Code = "1", Text = c("A reputation basis")),
      ReportType_Lx   = data.table(Code = "1", Text = c("A type of report")),
      RouteOfAdm_Lx   = data.table(Code = "1", Text = c("A route of admnistration")),
      Seriousness_Lx  = data.table(Code = "1", Text = c("Some seriousness criteria")),
      SizeUnit_Lx     = data.table(Code = "1", Text = c("A dosing unit")),
      SUSPECTEDDUPLICATES =
        data.table(
          UMCReportId = c(10000001L),
          SuspectedduplicateReportId = c(10000002)
        )
    )
  }

f_sets_sub_pq <-
  function(){
    f_sets <- f_sets_sub()

    f_sets$SUSPECTEDDUPLICATES <- NULL   # suspdup is moved from sub to main

    return(f_sets)
  }
