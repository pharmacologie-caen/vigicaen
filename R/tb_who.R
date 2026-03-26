#' Create WHO tables
#'
#' @description `r lifecycle::badge('stable')` Transform Vigibase WHO .csv files
#' to .parquet files
#'
#' WHODrug is delivered as zipped csv files folder, that you should
#' transform to a more efficient format. Parquet format from arrow has many advantages:
#' It can work with out-of-memory data, which makes it possible to process tables on
#' a computer with not-so-much RAM. It is also lightweighted and standard across different
#' languages.
#' The function also creates variables in each table. See [tb_vigibase()] for some running examples, and try `?mp_` or `?thg_` for more details.
#' Use [dt_parquet()] to load the tables afterward.
#'
#' @param path_who Character string, a directory containing whodrug csv tables. It is also the
#' output directory.
#' @param force Logical, to be passed to `cli::cli_progress_update()`. Used for internal
#' purposes.
#'
#' @importFrom stringr str_sub str_trim
#'
#' @seealso [tb_vigibase()], [tb_meddra()], [tb_subset()], [dt_parquet()]
#'
#' @export
#'
#' @returns .parquet files into the `path_who` directory.
#' Some columns are returned as `integer` (all Id columns, including Record_Id,
#' with notable exception of DrecNo),
#' and some columns as `numeric` (Quantity from ingredient table)
#' All other columns are `character`.
#'
#' @examplesIf interactive()
#'
#' # Use the examples from tb_main if you want to see these functions in action.
#'
#' path_who <- paste0(tempdir(), "/whodrug_directory/")
#' dir.create(path_who)
#' create_ex_who_csv(path_who)
#'
#' tb_who(path_who = path_who)
#'
#' # Clear temporary files when you're done
#' unlink(path_who, recursive = TRUE)

tb_who <-
  function(path_who,
           force = FALSE
           ){

    path_who <- fix_path_endslash(path_who)

    check_dir_exists(path_who)

    cli::cli_h1("tb_who()")
    cli::cli_alert_info("Creating WHO Drug tables.")

    msg_tb_onceperdatabase()

    cli_progress_bar(
      "Creating WHODrug",
      format = "{cli::pb_bar} {cli::pb_percent} | {cli::pb_elapsed} | {cli::pb_status}",
      total = 100
    )


    # ---- mp ---- ####
    cli_progress_update(force = force,status = "Read MP.csv", set = 3)

    mp <- arrow::read_csv_arrow(
      paste0(path_who, "MP.csv"),
      col_names = FALSE,
      as_data_frame = FALSE,
      schema =
        arrow::schema(
          Record_Id                     = arrow::int32(),
          Umc_Product_id                = arrow::string(),
          DrecNo                        = arrow::int32(), # full name Drug.Record.Number
          Sequence.number.1             = arrow::string(),
          Sequence.number.2             = arrow::string(),
          Sequence.number.3             = arrow::string(),
          Sequence.number.4             = arrow::string(),
          Substance_or_substance_synonym= arrow::string(),
          Drug.name                     = arrow::string(),
          Name.specifier                = arrow::string(),
          Marketing.Authorization.Number= arrow::string(),
          Marketing.Authorization.Date  = arrow::string(),
          Marketing.Authorization.Withdrawal.date= arrow::string(),
          Country                       = arrow::string(),
          Company                       = arrow::string(),
          Marketing.Authorization.Holder= arrow::string(),
          Reference.code                = arrow::string(),
          Source.country                = arrow::string(),
          Year.of.Reference             = arrow::string(),
          Product.type                  = arrow::string(),
          Create.date                   = arrow::string(),
          Date.changed                  = arrow::string()
        )
    )

    mp <-
      mp |>
      dplyr::mutate(
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
      )) |>
      dplyr::compute()

    # ---- write
    cli_progress_update(force = force, status = "Write mp.parquet", set = 12)

    arrow::write_parquet(mp, sink = paste0(path_who, "mp.parquet"))

    # ---- Therapeutic group ---- ####
    cli_progress_update(force = force, status = "Read ThG.csv", set = 30)

    thg <- arrow::read_csv_arrow(
      paste0(path_who, "ThG.csv"),
      col_names = FALSE,
      as_data_frame = FALSE,
      schema =
        arrow::schema(
          Therapgroup_Id    = arrow::int32(),
          ATC.code          = arrow::string(),
          Create.date       = arrow::string(),
          Official.ATC.code = arrow::string(),
          Record_Id         = arrow::int32()
        )
    )

    # ---- write
    cli_progress_update(force = force, status = "Write thg.parquet", set = 34)

    arrow::write_parquet(thg, sink = paste0(path_who, "thg.parquet"))

    # ---- ingredient ---- ####
    cli_progress_update(force = force, status = "Read ING.csv", set = 36)

    ing <- arrow::read_csv_arrow(
      paste0(path_who, "ING.csv"),
      col_names = FALSE,
      as_data_frame = FALSE,
      schema =
        arrow::schema(
          Ingredient_Id   = arrow::int32(),
          Create.date     = arrow::string(),
          Substance_Id    = arrow::string(),
          Quantity        = arrow::string(),
          Quantity.2      = arrow::string(),
          Unit            = arrow::string(),
          Record_Id       = arrow::int32()
        )
    )

    ing <-
      ing |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(c("Quantity")),
          ~
            .x |>
            str_trim() |>
            stringr::str_replace("^-$", "1568459784.65489") |>
            stringr::str_replace("^$", "1568459784.65489") |>
            as.numeric()
        ),
        dplyr::across(
          dplyr::all_of(c("Quantity")),
          ~ dplyr::if_else(.x == 1568459784.65489, NA_real_, .x)
        )
      ) |>
      dplyr::compute()

    # ---- write
    cli_progress_update(force = force, status = "Write ing.parquet", set = 48)

    arrow::write_parquet(ing, sink = paste0(path_who, "ing.parquet"))

    # ---- Reference (SRCE) ---- ####
    cli_progress_update(force = force, status = "Read SRCE.csv", set = 51)

    srce <- arrow::read_csv_arrow(
      paste0(path_who, "SRCE.csv"),
      col_names = FALSE,
      as_data_frame = FALSE,
      schema =
        arrow::schema(
          Reference.code = arrow::string(),
          Reference      = arrow::string(),
          Country.code   = arrow::string()
        )
    )

    # ---- write
    cli_progress_update(force = force, status = "Write srce.parquet", set = 55)

    arrow::write_parquet(srce, sink = paste0(path_who, "srce.parquet"))

    # ---- Organization ---- ####
    cli_progress_update(force = force, status = "Read ORG.csv", set = 57)

    org <- arrow::read_csv_arrow(
      paste0(path_who, "ORG.csv"),
      col_names = FALSE,
      as_data_frame = FALSE,
      schema =
        arrow::schema(
          Organization_Id = arrow::string(),
          Name            = arrow::string(),
          Country.code    = arrow::string()
        )
    )

    # ---- write
    cli_progress_update(
      force = force,
      status = "Write org.parquet",
      set = 61)

    arrow::write_parquet(org,
                         sink = paste0(path_who, "org.parquet")
    )

    # ---- Country CCODE ---- ####
    cli_progress_update(
      force = force,
      status = "Read CCODE.csv",
      set = 63)

    ccode <- arrow::read_csv_arrow(
      paste0(path_who, "CCODE.csv"),
      col_names = FALSE,
      as_data_frame = FALSE,
      schema =
        arrow::schema(
          Country.code = arrow::string(),
          Country.name = arrow::string()
        )
    )

    # ---- write
    cli_progress_update(
      force = force,
      status = "Write ccode.parquet",
      set = 75)

    arrow::write_parquet(ccode,
                         sink = paste0(path_who, "ccode.parquet")
    )

    # ---- ATC code ---- ####
    cli_progress_update(
      force = force,
      status = "Read ATC.csv",
      set = 79)

    atc <- arrow::read_csv_arrow(
      paste0(path_who, "ATC.csv"),
      col_names = FALSE,
      as_data_frame = FALSE,
      schema =
        arrow::schema(
          ATC.code = arrow::string(),
          Level    = arrow::string(),
          Text     = arrow::string()
        )
    )

    # ---- write
    cli_progress_update(
      force = force,
      status = "Write atc.parquet",
      set = 81)

    arrow::write_parquet(atc,
                         sink = paste0(path_who, "atc.parquet")
    )

    # ---- Substance ---- ####
    cli_progress_update(
      force = force,
      status = "Read SUN.csv",
      set = 82)

    sun <- arrow::read_csv_arrow(
      paste0(path_who, "SUN.csv"),
      col_names = FALSE,
      as_data_frame = FALSE,
      schema =
        arrow::schema(
          Substance_Id      = arrow::int32(),
          CAS.number        = arrow::string(),
          Language.code     = arrow::string(),
          Substance.name    = arrow::string(),
          Year.of.Reference = arrow::string(),
          Reference.code    = arrow::string()
        )
    )

    # ---- write
    cli_progress_update(
      force = force,
      status = "Write sun.parquet",
      set = 84)

    arrow::write_parquet(sun,
                         sink = paste0(path_who, "sun.parquet")
    )

    # ---- Pharmaceutical form (PF) ---- ####
    cli_progress_update(
      force = force,
      status = "Read PF.csv",
      set = 85)

    pf <- arrow::read_csv_arrow(
      paste0(path_who, "PF.csv"),
      col_names = FALSE,
      as_data_frame = FALSE,
      schema =
        arrow::schema(
          Pharmform_Id = arrow::string(),
          Text         = arrow::string()
        )
    )

    # ---- write
    cli_progress_update(
      force = force,
      status = "Write pf.parquet",
      set = 87)

    arrow::write_parquet(pf,
                         sink = paste0(path_who, "pf.parquet")
    )

    # ---- Strength = dosage ---- ####
    cli_progress_update(
      force = force,
      status = "Read STR.csv",
      set = 88)

    str <- arrow::read_csv_arrow(
      paste0(path_who, "STR.csv"),
      col_names = FALSE,
      as_data_frame = FALSE,
      schema =
        arrow::schema(
          Strength_Id = arrow::string(),
          Text         = arrow::string()
        )
    )

    # ---- write
    cli_progress_update(
      force = force,
      status = "Write str.parquet",
      set = 90)

    arrow::write_parquet(str,
                         sink = paste0(path_who, "str.parquet")
    )

    # ---- Product type ---- ####
    cli_progress_update(
      force = force,
      status = "Read PRT.csv",
      set = 94)

    prt <- arrow::read_csv_arrow(
      paste0(path_who, "PRT.csv"),
      col_names = FALSE,
      as_data_frame = FALSE,
      schema =
        arrow::schema(
          Prodtype_Id = arrow::string(),
          Text         = arrow::string()
        )
    )

    # ---- write
    cli_progress_update(
      force = force,
      status = "Write prt.parquet",
      set = 96)

    arrow::write_parquet(prt,
                         sink = paste0(path_who, "prt.parquet")
    )

    # ---- Unit full code ASCII ---- ####
    cli_progress_update(
      force = force,
      status = "Read Unit-X.csv",
      set = 97)

    unitx <- arrow::read_csv_arrow(
      paste0(path_who, "Unit-X.csv"),
      col_names = FALSE,
      as_data_frame = FALSE,
      schema =
        arrow::schema(
          Unit_Id  = arrow::int32(),
          Text     = arrow::string()
        )
    )

    # ---- write
    cli_progress_update(
      force = force,
      status = "Write unitx.parquet",
      set = 99)

    arrow::write_parquet(unitx,
                         sink = paste0(path_who, "unitx.parquet")
    )

    cli_progress_update(
      force = force,
      status = "Done",
      set = 100)

    cli_progress_done()
  }
