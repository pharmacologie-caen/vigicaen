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

    # ---- mp ---- ####
    texter("Read MP.txt", "3%%")

    mp <- reader("MP.txt", path_who)

    # ---- split
    texter("Split mp", "6%%")

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
    texter("Write mp.parquet", "12%%")

    arrow::write_parquet(mp,
                         sink = paste0(path_who, "mp.parquet")
    )

    # ---- pharmaceutical products ---- ####
    texter("Read PP.txt", "16%%")

    pp <- reader("PP.txt", path_who)

    # ---- split
    texter("Split pp", "20%%")

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
    texter("Write pp.parquet", "27%%")

    arrow::write_parquet(pp,
                         sink = paste0(path_who, "pp.parquet")
    )


    # ---- Therapeutic group ---- ####
    texter("Read ThG.txt", "30%%")

    thg <- reader("ThG.txt", path_who)

    # ---- split
    texter("Split thg", "32%%")

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
    texter("Write thg.parquet", "34%%")

    arrow::write_parquet(thg,
                         sink = paste0(path_who, "thg.parquet")
    )

    # ---- ingredient ---- ####
    texter("Read ING.txt", "36%%")

    ing <- reader("ING.txt", path_who)

    # ---- split
    texter("Split ing", "41%%")

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
    texter("Write ing.parquet", "48%%")

    arrow::write_parquet(ing,
                         sink = paste0(path_who, "ing.parquet")
    )

    # ---- Reference (SRCE) ---- ####
    texter("Read SRCE.txt", "51%%")

    srce <- reader("SRCE.txt", path_who)

    # ---- split
    texter("Split srce", "53%%")

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
    texter("Write srce.parquet", "55%%")

    arrow::write_parquet(srce,
                         sink = paste0(path_who, "srce.parquet")
    )

    # ---- Organization ---- ####
    texter("Read ORG.txt", "57%%")

    org <- reader("ORG.txt", path_who)

    # ---- split
    texter("Split org", "59%%")

    org <-
      org |>
      dplyr::transmute(
        Organization_Id = str_sub(.data$f0, start = 1, end = 10),
        Name            = str_sub(.data$f0, start = 11, end = 90),
        Country.code    = str_sub(.data$f0, start = 91, end = 100)
      ) |>
      dplyr::compute()

    # ---- write
    texter("Write org.parquet", "61%%")

    arrow::write_parquet(org,
                         sink = paste0(path_who, "org.parquet")
    )

    # ---- Country CCODE ---- ####
    texter("Read CCODE.txt", "63%%")

    ccode <- reader("CCODE.txt", path_who)

    # ---- split
    texter("Split ccode", "68%%")

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
    texter("Write ccode.parquet", "75%%")

    arrow::write_parquet(ccode,
                         sink = paste0(path_who, "ccode.parquet")
    )

    # ---- ATC code ---- ####
    texter("Read ATC.txt", "79%%")

    atc <- reader("ATC.txt", path_who)

    # ---- split
    texter("Split atc", "80%%")

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
    texter("Write atc.parquet", "81%%")

    arrow::write_parquet(atc,
                         sink = paste0(path_who, "atc.parquet")
    )

    # ---- Substance ---- ####
    texter("Read SUN.txt", "82%%")

    sun <- reader("SUN.txt", path_who)

    # ---- split
    texter("Split sun", "83%%")

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
    texter("Write sun.parquet", "84%%")

    arrow::write_parquet(sun,
                         sink = paste0(path_who, "sun.parquet")
    )

    # ---- Pharmaceutical form (PF) ---- ####
    texter("Read PF.txt", "85%%")

    pf <- reader("PF.txt", path_who)

    # ---- split
    texter("Split sun", "86%%")

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
    texter("Write pf.parquet", "87%%")

    arrow::write_parquet(pf,
                         sink = paste0(path_who, "pf.parquet")
    )

    # ---- Strength = dosage ---- ####
    texter("Read STR.txt", "88%%")

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
    texter("Split str", "89%%")

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
    texter("Write str.parquet", "90%%")

    arrow::write_parquet(str,
                         sink = paste0(path_who, "str.parquet")
    )

    # ---- Product group ---- ####
    texter("Read PRG.txt", "91%%")

    prg <- reader("PRG.txt", path_who)

    # ---- split
    texter("Split prg", "92%%")

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
    texter("Write prg.parquet", "93%%")

    arrow::write_parquet(prg,
                         sink = paste0(path_who, "prg.parquet")
    )

    # ---- Product type ---- ####
    texter("Read PRT.txt", "94%%")

    prt <- reader("PRT.txt", path_who)

    # ---- split
    texter("Split prt", "95%%")

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
    texter("Write prt.parquet", "96%%")

    arrow::write_parquet(prt,
                         sink = paste0(path_who, "prt.parquet")
    )

    # ---- Unit full code ASCII ---- ####
    texter("Read Unit-X.txt", "97%%")

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
    texter("Split unitx", "98%%")

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
    texter("Write unitx.parquet", "99%%")

    arrow::write_parquet(unitx,
                         sink = paste0(path_who, "unitx.parquet")
    )

    texter("Done", "")
  }
