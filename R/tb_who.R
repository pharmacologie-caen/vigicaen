#' Create main WHO tables
#'
#' @description `r lifecycle::badge('stable')` Transform Vigibase WHO .txt files
#' to .parquet files
#'
#' WHODrug is delivered as zipped text files folder, that you should
#' transform to a more efficient format. Parquet format from arrow has many advantages:
#' It can work with out-of-memory data, which makes it possible to process tables on
#' a computer with not-so-much RAM. It is also lightweighted and standard across different
#' languages.
#' The function also creates variables in each table. See [tb_vigibase()] for some running examples, and try `?mp_` or `?thg_` for more details.
#' Use [dt_parquet()] to load the tables afterward.
#'
#' @param path_who Character string, a directory containing whodrug txt tables. It is also the
#' output directory.
#'
#' @importFrom stringr str_sub str_trim
#'
#' @seealso [tb_vigibase()], [tb_meddra()], [tb_subset()], [dt_parquet()]
#'
#' @export
#'
#' @returns .parquet files into the `path_who` directory.
#' Some columns are returned as `integer` (all Id columns, including MedicinalProd_Id,
#' with notable exception of DrecNo),
#' and some columns as `numeric` (Quantity from ingredient table)
#' All other columns are `character`.
#'
#' @examples
#'
#' # Use the examples from tb_main if you want to see these functions in action.
#'
#' path_who <- "/whodrug_directory/"
#'
#' ## NOT RUN ##
#' # tb_who(path_who = path_who)

tb_who <-
  function(path_who
           ){

    path_who <-
        fix_path_endslash(path_who)

    if(!dir.exists(path_who)){
      stop(paste0(path_who, " does not exist"))
    }

    cli::cli_h1(
      "tb_who()"
    )
    cli::cli_alert_info(
      "Creating WHO Drug tables.")

    msg_tb_onceperdatabase()

    cli_progress_bar(
      "Creating WHODrug",
      format = "{cli::pb_bar} {cli::pb_percent} | {cli::pb_elapsed} | {cli::pb_status}",
      total = 100
    )


    # ---- mp ---- ####
    cli_progress_update(force = TRUE,status = "Read MP.txt", set = 3)

    mp <- reader("MP.txt", path_who)

    # ---- split
    cli_progress_update(force = TRUE,status = "Split mp", set = 6)

    mp <-
      mp |>
      dplyr::transmute(
        MedicinalProd_Id   = str_sub(.data$f0, start = 1L,  end = 10L),
        MedID              = str_sub(.data$f0, start = 11L, end = 45L),
        Drug.record.number = str_sub(.data$f0, start = 46L, end = 51L),
        Sequence.number.1  = str_sub(.data$f0, start = 52L, end = 53L),
        Sequence.number.2  = str_sub(.data$f0, start = 54L, end = 56L),
        Sequence.number.3  = str_sub(.data$f0, start = 57L, end = 66L),
        Pharmform_Id       = str_sub(.data$f0, start = 64L, end = 66L),
        Sequence.number.4  = str_sub(.data$f0, start = 67L, end = 76L),
        Strength_Id        = str_sub(.data$f0, start = 71L, end = 76L),
        Generic            = str_sub(.data$f0, start = 77L, end = 77L),
        Drug.name          = str_sub(.data$f0, start = 78L, end = 1577L),
        Name.specifier     = str_sub(.data$f0, start = 1578L, end = 1607L),
        Marketing.Authorization.Number = str_sub(.data$f0, start = 1608L, end =
                                                   1637L),
        Marketing.Authorization.Date = str_sub(.data$f0, start = 1638L, end = 1645L),
        Marketing.Authorization.Withdrawal.date = str_sub(.data$f0, start = 1646L, end =
                                                            1653L),
        Country = str_sub(.data$f0, start = 1654L, end = 1663L),
        Company = str_sub(.data$f0, start = 1664L, end = 1673L),
        Marketing.Authorization.Holder = str_sub(.data$f0, start = 1674L, end =
                                                   1683L),
        Reference.code    = str_sub(.data$f0, start = 1684L, end = 1693L),
        Source.country    = str_sub(.data$f0, start = 1694L, end = 1703L),
        Year.of.Reference = str_sub(.data$f0, start = 1704L, end = 1706L),
        Product.type      = str_sub(.data$f0, start = 1707L, end = 1716L),
        Product.group     = str_sub(.data$f0, start = 1717L, end = 1726L),
        Create.date       = str_sub(.data$f0, start = 1727L, end = 1734L),
        Date.changed      = str_sub(.data$f0, start = 1735L, end = 1742L)
      ) |>
      dplyr::mutate(
        MedicinalProd_Id = .data$MedicinalProd_Id |>
          str_trim() |>
          as.integer(),
        drug_name_t = .data$Drug.name |>
          str_trim() |>
          stringr::str_to_lower(),
        DrecNo = .data$Drug.record.number |>
          as.integer()
      ) |>
      dplyr::select(
        dplyr::all_of(c(
          "MedicinalProd_Id",
          "Sequence.number.1",
          "Sequence.number.2",
          "DrecNo",
          "drug_name_t",
          "Create.date",
          "Date.changed",
          "Country")
      )) |>
      dplyr::compute()

    # ---- write
    cli_progress_update(
      force = TRUE,
      status = "Write mp.parquet",
      set = 12)

    arrow::write_parquet(mp,
                         sink = paste0(path_who, "mp.parquet")
    )

    # ---- pharmaceutical products ---- ####
    cli_progress_update(
      force = TRUE,
      status = "Read PP.txt",
      set = 16)

    pp <- reader("PP.txt", path_who)

    # ---- split
    cli_progress_update(
      force = TRUE,
      status = "Split pp",
      set = 20)

    pp <-
      pp |>
      dplyr::transmute(
        Pharmproduct_Id       = str_sub(.data$f0, start = 1, end = 10),
        Pharmaceutical.form   = str_sub(.data$f0, start = 11, end = 20),
        Route.of.administration = str_sub(.data$f0, start = 21, end = 30),
        MedicinalProd_Id      = str_sub(.data$f0, start = 31, end = 40),
        Number.of.ingredients = str_sub(.data$f0, start = 41, end = 42),
        Create.date           = str_sub(.data$f0, start = 43, end = 50)
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(c("Pharmproduct_Id", "MedicinalProd_Id")),
                      ~ .x |>
                        str_trim() |>
                        as.integer()
        )
      ) |>
      dplyr::compute()

    # ---- write
    cli_progress_update(
      force = TRUE,
      status = "Write pp.parquet",
      set = 27)

    arrow::write_parquet(pp,
                         sink = paste0(path_who, "pp.parquet")
    )


    # ---- Therapeutic group ---- ####
    cli_progress_update(
      force = TRUE,
      status = "Read ThG.txt",
      set = 30)

    thg <- reader("ThG.txt", path_who)

    # ---- split
    cli_progress_update(
      force = TRUE,
      status = "Split thg",
      set = 32)

    thg <-
      thg |>
      dplyr::transmute(
        Therapgroup_Id    = str_sub(.data$f0, start = 1, end = 10),
        ATC.code          = str_trim(str_sub(.data$f0, start = 11, end = 20)),
        Create.date       = str_sub(.data$f0, start = 21, end = 28),
        Official.ATC.code = str_sub(.data$f0, start = 29, end = 29),
        MedicinalProd_Id  = str_sub(.data$f0, start = 30, end = 39)
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(c("Therapgroup_Id", "MedicinalProd_Id")),
                      ~ .x |>
                        str_trim() |>
                        as.integer()
        )
      ) |>
      dplyr::compute()

    # ---- write
    cli_progress_update(
      force = TRUE,
      status = "Write thg.parquet",
      set = 34)

    arrow::write_parquet(thg,
                         sink = paste0(path_who, "thg.parquet")
    )

    # ---- ingredient ---- ####
    cli_progress_update(
      force = TRUE,
      status = "Read ING.txt",
      set = 36)

    ing <- reader("ING.txt", path_who)

    # ---- split
    cli_progress_update(
      force = TRUE,
      status = "Split ing",
      set = 41)

    ing <-
      ing |>
      dplyr::transmute(
        Ingredient_Id    = str_sub(.data$f0, start=1,  end=10),
        Create.date      = str_sub(.data$f0, start=11, end=18),
        Substance_Id     = str_sub(.data$f0, start=19, end=28),
        Quantity         = str_sub(.data$f0, start=29, end=43),
        Quantity.2       = str_sub(.data$f0, start=44, end=58),
        Unit             = str_sub(.data$f0, start=59, end=68),
        Pharmproduct_Id  = str_sub(.data$f0, start=69, end=78),
        MedicinalProd_Id = str_sub(.data$f0, start=79, end=88)
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(
          c("Ingredient_Id", "MedicinalProd_Id", "Pharmproduct_Id")),
          ~ .x |>
            str_trim() |>
            as.integer()
        ),
        dplyr::across(dplyr::all_of(c("Quantity")),
                      ~
                        .x |>
                        str_trim() |>
                        stringr::str_replace("^-$", "1568459784.65489") |>
                        stringr::str_replace("^$", "1568459784.65489") |>
                        as.numeric()
        ),
        dplyr::across(dplyr::all_of(c("Quantity")),
                      ~ dplyr::if_else(.x == 1568459784.65489, NA_real_, .x))

      ) |>
      dplyr::compute()

    # ---- write
    cli_progress_update(
      force = TRUE,
      status = "Write ing.parquet",
      set = 48)

    arrow::write_parquet(ing,
                         sink = paste0(path_who, "ing.parquet")
    )

    # ---- Reference (SRCE) ---- ####
    cli_progress_update(
      force = TRUE,
      status = "Read SRCE.txt",
      set = 51)

    srce <- reader("SRCE.txt", path_who)

    # ---- split
    cli_progress_update(
      force = TRUE,
      status = "Split srce",
      set = 53)

    srce <-
      srce |>
      dplyr::transmute(
        Reference.code = str_sub(.data$f0, start = 1, end = 10),
        Reference      = str_sub(.data$f0, start = 11, end = 90),
        Country.code   = str_sub(.data$f0, start = 91, end = 100)
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(),
                      ~ .x |>
                        str_trim()
        )
      ) |>
      dplyr::compute()

    # ---- write
    cli_progress_update(
      force = TRUE,
      status = "Write srce.parquet",
      set = 55)

    arrow::write_parquet(srce,
                         sink = paste0(path_who, "srce.parquet")
    )

    # ---- Organization ---- ####
    cli_progress_update(
      force = TRUE,
      status = "Read ORG.txt",
      set = 57)

    org <- reader("ORG.txt", path_who)

    # ---- split
    cli_progress_update(
      force = TRUE,
      status = "Split org",
      set = 59)

    org <-
      org |>
      dplyr::transmute(
        Organization_Id = str_sub(.data$f0, start = 1, end = 10),
        Name            = str_sub(.data$f0, start = 11, end = 90),
        Country.code    = str_sub(.data$f0, start = 91, end = 100)
      ) |>
      dplyr::compute()

    # ---- write
    cli_progress_update(
      force = TRUE,
      status = "Write org.parquet",
      set = 61)

    arrow::write_parquet(org,
                         sink = paste0(path_who, "org.parquet")
    )

    # ---- Country CCODE ---- ####
    cli_progress_update(
      force = TRUE,
      status = "Read CCODE.txt",
      set = 63)

    ccode <- reader("CCODE.txt", path_who)

    # ---- split
    cli_progress_update(
      force = TRUE,
      status = "Split ccode",
      set = 68)

    ccode <-
      ccode |>
      dplyr::transmute(
        Country.code = str_sub(.data$f0, start = 1, end = 10),
        Country.name = str_sub(.data$f0, start = 11, end = 90)
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(),
                      ~ .x |>
                        str_trim()
        )
      ) |>
      dplyr::compute()

    # ---- write
    cli_progress_update(
      force = TRUE,
      status = "Write ccode.parquet",
      set = 75)

    arrow::write_parquet(ccode,
                         sink = paste0(path_who, "ccode.parquet")
    )

    # ---- ATC code ---- ####
    cli_progress_update(
      force = TRUE,
      status = "Read ATC.txt",
      set = 79)

    atc <- reader("ATC.txt", path_who)

    # ---- split
    cli_progress_update(
      force = TRUE,
      status = "Split atc",
      set = 80)

    atc <-
      atc |>
      dplyr::transmute(
        ATC.code = str_sub(.data$f0, start = 1, end = 10),
        Level    = str_sub(.data$f0, start = 11, end = 11),
        Text     = str_sub(.data$f0, start = 12, end = 121)
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(),
                      ~ .x |>
                        str_trim()
        )
      ) |>
      dplyr::compute()

    # ---- write
    cli_progress_update(
      force = TRUE,
      status = "Write atc.parquet",
      set = 81)

    arrow::write_parquet(atc,
                         sink = paste0(path_who, "atc.parquet")
    )

    # ---- Substance ---- ####
    cli_progress_update(
      force = TRUE,
      status = "Read SUN.txt",
      set = 82)

    sun <- reader("SUN.txt", path_who)

    # ---- split
    cli_progress_update(
      force = TRUE,
      status = "Split sun",
      set = 83)

    sun <-
      sun |>
      dplyr::transmute(
        Substance_Id      = str_sub(.data$f0, start = 1, end = 10),
        CAS.number        = str_sub(.data$f0, start = 11, end = 20),
        Language.code     = str_sub(.data$f0, start = 21, end = 30),
        Substance.name    = str_sub(.data$f0, start = 31, end = 140),
        Year.of.Reference = str_sub(.data$f0, start = 141, end = 143),
        Reference.code    = str_sub(.data$f0, start = 144, end = 153)
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(),
                      ~ .x |>
                        str_trim()
        ),
        Substance_Id = as.integer(.data$Substance_Id)
      ) |>
      dplyr::compute()

    # ---- write
    cli_progress_update(
      force = TRUE,
      status = "Write sun.parquet",
      set = 84)

    arrow::write_parquet(sun,
                         sink = paste0(path_who, "sun.parquet")
    )

    # ---- Pharmaceutical form (PF) ---- ####
    cli_progress_update(
      force = TRUE,
      status = "Read PF.txt",
      set = 85)

    pf <- reader("PF.txt", path_who)

    # ---- split
    cli_progress_update(
      force = TRUE,
      status = "Split pf",
      set = 86)

    pf <-
      pf |>
      dplyr::transmute(
        Pharmform_Id = str_sub(.data$f0, start = 1, end = 10),
        Text         = str_sub(.data$f0, start = 11, end = 90)
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(),
                      ~ .x |>
                        str_trim()
        )
      ) |>
      dplyr::compute()

    # ---- write
    cli_progress_update(
      force = TRUE,
      status = "Write pf.parquet",
      set = 87)

    arrow::write_parquet(pf,
                         sink = paste0(path_who, "pf.parquet")
    )

    # ---- Strength = dosage ---- ####
    cli_progress_update(
      force = TRUE,
      status = "Read STR.txt",
      set = 88)

    str <-  arrow::read_delim_arrow(
      paste0(path_who, "STR.txt"),
      col_names = FALSE,
      as_data_frame = FALSE,
      delim = "\t",
      read_options =
        arrow::csv_read_options(column_names = "f0",
                                encoding = "ANSI_X3.4-1986")
    )

    # ---- split
    cli_progress_update(
      force = TRUE,
      status = "Split str",
      set = 89)

    str <-
      str |>
      dplyr::transmute(
        Strength_Id = str_sub(.data$f0, start = 1, end = 10),
        Text        = str_sub(.data$f0, start = 11, end = 510)
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(),
                      ~ .x |>
                        stringr::str_trim()
        )
      ) |>
      dplyr::compute()

    # ---- write
    cli_progress_update(
      force = TRUE,
      status = "Write str.parquet",
      set = 90)

    arrow::write_parquet(str,
                         sink = paste0(path_who, "str.parquet")
    )

    # ---- Product group ---- ####
    cli_progress_update(
      force = TRUE,
      status = "Read PRG.txt",
      set = 91)

    prg <- reader("PRG.txt", path_who)

    # ---- split
    cli_progress_update(
      force = TRUE,
      status = "Split prg",
      set = 92)

    prg <-
      prg |>
      dplyr::transmute(
        Productgroup_Id   = str_sub(.data$f0, start = 1, end = 10),
        Productgroup.name = str_sub(.data$f0, start = 11, end = 70),
        Date.recorded     = str_sub(.data$f0, start = 71, end = 78)
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(),
                      ~ .x |>
                        stringr::str_trim()
        ),
        Productgroup_Id = as.integer(.data$Productgroup_Id)
      ) |>
      dplyr::compute()

    # ---- write
    cli_progress_update(
      force = TRUE,
      status = "Write prg.parquet",
      set = 93)

    arrow::write_parquet(prg,
                         sink = paste0(path_who, "prg.parquet")
    )

    # ---- Product type ---- ####
    cli_progress_update(
      force = TRUE,
      status = "Read PRT.txt",
      set = 94)

    prt <- reader("PRT.txt", path_who)

    # ---- split
    cli_progress_update(
      force = TRUE,
      status = "Split prt",
      set = 95)

    prt <-
      prt |>
      dplyr::transmute(
        Prodtype_Id = str_sub(.data$f0, start = 1, end = 10),
        Text        = str_sub(.data$f0, start = 11, end = 90)
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(),
                      ~ .x |>
                        str_trim()
        ),
        Prodtype_Id = as.integer(.data$Prodtype_Id)
      ) |>
      dplyr::compute()

    # ---- write
    cli_progress_update(
      force = TRUE,
      status = "Write prt.parquet",
      set = 96)

    arrow::write_parquet(prt,
                         sink = paste0(path_who, "prt.parquet")
    )

    # ---- Unit full code ASCII ---- ####
    cli_progress_update(
      force = TRUE,
      status = "Read Unit-X.txt",
      set = 97)

    unitx <-  arrow::read_delim_arrow(
      paste0(path_who, "Unit-X.txt"),
      col_names = FALSE,
      as_data_frame = FALSE,
      delim = "\t",
      read_options =
        arrow::csv_read_options(column_names = "f0",
                                encoding = "ANSI_X3.4-1986")
    )

    # ---- split
    cli_progress_update(
      force = TRUE,
      status = "Split unitx",
      set = 98)

    unitx <-
      unitx |>
      dplyr::transmute(
        Unit_Id = str_sub(.data$f0, start = 1, end = 10),
        Text    = str_sub(.data$f0, start = 11, end = 50)
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(),
                      ~ .x |>
                        str_trim()
        ),
        Unit_Id = as.integer(.data$Unit_Id)
      ) |>
      dplyr::compute()

    # ---- write
    cli_progress_update(
      force = TRUE,
      status = "Write unitx.parquet",
      set = 99)

    arrow::write_parquet(unitx,
                         sink = paste0(path_who, "unitx.parquet")
    )

    cli_progress_update(
      force = TRUE,
      status = "Done",
      set = 100)

    cli_progress_done()
  }
