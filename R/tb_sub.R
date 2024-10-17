#' Create subsidiary VigiBase ECL tables
#'
#' @description `r lifecycle::badge('stable')` `tb_sub()` transform .txt files
#' to .parquet files
#'
#' Subsidiary files in VigiBase provide context to many numerically stored variables.
#' Most important file here is the suspected duplicates table, but it is processed
#' in [tb_main()]. More documentation can be found in [tb_main()].
#' Use [dt_parquet()] to load the tables afterward.
#'
#' @param path_sub Character string, a directory containing whodrug txt tables. It is also the
#' output directory.
#'
#' @importFrom stringr str_sub str_trim
#'
#' @keywords import sub
#'
#' @seealso [tb_main()], [tb_who()], [tb_meddra()], [tb_custom()], [dt_parquet()]
#'
#' @export
#'
#' @return .parquet files into the `path_sub` directory. Character strings are
#' stringr::str_trim()ed.
#'
#' @examples
#'
#' # Use the examples from tb_main if you want to see these functions in action.
#'
#' path_who <- "/vigibase_subsidiary_files/"
#'
#' ## NOT RUN ##
#' # tb_sub(path_sub = path_sub)
#'

tb_sub <- function(path_sub){
# AgeGroup
texter("Read AgeGroup_Lx.txt", "1%%")

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
texter("Read Dechallenge_Lx.txt", "7%%")

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
texter("Read Dechallenge2_Lx.txt", "13%%")

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
texter("Read Frequency_Lx.txt", "19%%")

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
texter("Read Gender_Lx.txt", "25%%")

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
texter("Read Notifier_Lx.txt", "31%%")

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
texter("Read Outcome_Lx.txt", "37%%")

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
texter("Read Rechallenge_Lx.txt", "43%%")

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
texter("Read Rechallenge2_Lx.txt", "49%%")

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
texter("Read Region_Lx.txt", "55%%")

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
texter("Read RepBasis_Lx.txt", "61%%")

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
texter("Read ReportType_Lx.txt", "67%%")

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
texter("Read RouteOfAdm_Lx.txt", "75%%")

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
texter("Read Seriousness_Lx.txt", "88%%")

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
texter("Read SizeUnit_Lx.txt", "96%%")

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

texter("Done", "")

}
