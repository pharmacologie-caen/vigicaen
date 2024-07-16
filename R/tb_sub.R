#' Create main VigiBase ECL tables
#'
#' Transform .txt files to .parquet files
#'
#' WHODrug is delivered as zipped text files, that you should
#' transform to a more efficient format. Parquet format from arrow has many advantages:
#' It works with out-of-memory data, which makes it possible to process tables on
#' a computer with not-so-much RAM. It is also lightweighted and standard across different
#' langages.
#' The function also creates variables in each table. See \code{\link{tb_main}} for some running examples.
#'
#' @param path_sub Character string, a directory containing whodrug txt tables. It is also the
#' output directory.
#'
#' @importFrom stringr str_sub str_trim
#'
#' @seealso \code{\link{tb_main}} \code{\link{tb_sub}}
#'
#' @export
#'
#' @return .parquet files into the `path_who` directory.
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
#'

tb_sub <- function(path_sub){
# AgeGroup
texter("Read AgeGroup_Lx.txt", "82%%")

AgeGroup <- reader("AgeGroup_Lx.txt")
AgeGroup <-
  AgeGroup |>
  dplyr::transmute(
    AgeGroup = substr(.data$f0, start=1, stop=1),
    Code = substr(.data$f0, start=2, stop=nchar(.data$f0))
  ) |>
  dplyr::compute()
arrow::write_parquet(AgeGroup, sink = paste0(path_sub, "AgeGroup.parquet"))

# Dechallenge
texter("Read Dechallenge_Lx.txt", "83%%")

Dechallenge <- reader("Dechallenge_Lx.txt")
Dechallenge <-
  Dechallenge |>
  dplyr::transmute(
    Dechallenge1 = substr(.data$f0, start=1, stop=1),
    Code = substr(.data$f0, start=2, stop=nchar(.data$f0))
  ) |>
  dplyr::compute()
arrow::write_parquet(Dechallenge, sink = paste0(path_sub, "Dechallenge.parquet"))

# Dechallenge2
texter("Read Dechallenge2_Lx.txt", "84%%")

Dechallenge2 <- reader("Dechallenge2_Lx.txt")
Dechallenge2 <-
  Dechallenge2 |>
  dplyr::transmute(
    Dechallenge2 = substr(.data$f0, start=1, stop=1),
    Code = substr(.data$f0, start=2, stop=nchar(.data$f0))
  ) |>
  dplyr::compute()
arrow::write_parquet(Dechallenge2, sink = paste0(path_sub, "Dechallenge2.parquet"))

# FrequencyU
texter("Read Frequency_Lx.txt", "85%%")

Frequency <- reader("Frequency_Lx.txt")
Frequency <-
  Frequency |>
  dplyr::transmute(
    FrequencyU = substr(.data$f0, start=1, stop=3),
    Code = substr(.data$f0, start=4, stop=nchar(.data$f0))
  ) |>
  dplyr::compute()
arrow::write_parquet(Frequency, sink = paste0(path_sub, "Frequency.parquet"))

# Gender
texter("Read Gender_Lx.txt", "86%%")

Gender <- reader("Gender_Lx.txt")
Gender <-
  Gender |>
  dplyr::transmute(
    Gender = substr(.data$f0, start=1, stop=1),
    Code = substr(.data$f0, start=2, stop=nchar(.data$f0))
  ) |>
  dplyr::compute()
arrow::write_parquet(Gender, sink = paste0(path_sub, "Gender.parquet"))

# Notifier
texter("Read Notifier_Lx.txt", "87%%")

Notifier <- reader("Notifier_Lx.txt")
Notifier <-
  Notifier |>
  dplyr::transmute(
    Type = substr(.data$f0, start=1, stop=2),
    Code = substr(.data$f0, start=3, stop=nchar(.data$f0))
  ) |>
  dplyr::compute()
arrow::write_parquet(Notifier, sink = paste0(path_sub, "Notifier.parquet"))

# Outcome
texter("Read Outcome_Lx.txt", "88%%")

Outcome <- reader("Outcome_Lx.txt")
Outcome <-
  Outcome |>
  dplyr::transmute(
    Outcome = substr(.data$f0, start=1, stop=1),
    Code = substr(.data$f0, start=2, stop=nchar(.data$f0))
  ) |>
  dplyr::compute()
arrow::write_parquet(Outcome, sink = paste0(path_sub, "Outcome.parquet"))

# Rechallenge
texter("Read Rechallenge_Lx.txt", "89%%")

Rechallenge <- reader("Rechallenge_Lx.txt")
Rechallenge <-
  Rechallenge |>
  dplyr::transmute(
    Rechallenge1 = substr(.data$f0, start=1, stop=1),
    Code = substr(.data$f0, start=2, stop=nchar(.data$f0))
  ) |>
  dplyr::compute()
arrow::write_parquet(Rechallenge, sink = paste0(path_sub, "Rechallenge.parquet"))

# Rechallenge2
texter("Read Rechallenge2_Lx.txt", "90%%")

Rechallenge2 <- reader("Rechallenge2_Lx.txt")
Rechallenge2 <-
  Rechallenge2 |>
  dplyr::transmute(
    Rechallenge2 = substr(.data$f0, start=1, stop=1),
    Code = substr(.data$f0, start=2, stop=nchar(.data$f0))
  ) |>
  dplyr::compute()
arrow::write_parquet(Rechallenge2, sink = paste0(path_sub, "Rechallenge2.parquet"))

# Region
texter("Read Region_Lx.txt", "91%%")

Region <- reader("Region_Lx.txt")
Region <-
  Region |>
  dplyr::transmute(
    Region = substr(.data$f0, start=1, stop=1),
    Code = substr(.data$f0, start=2, stop=nchar(.data$f0))
  ) |>
  dplyr::compute()
arrow::write_parquet(Region, sink = paste0(path_sub, "Region.parquet"))

# RepBasis
texter("Read RepBasis_Lx.txt", "92%%")

RepBasis <- reader("RepBasis_Lx.txt")
RepBasis <-
  RepBasis |>
  dplyr::transmute(
    Basis = substr(.data$f0, start=1, stop=1),
    Code = substr(.data$f0, start=2, stop=nchar(.data$f0))
  ) |>
  dplyr::compute()
arrow::write_parquet(RepBasis, sink = paste0(path_sub, "RepBasis.parquet"))

# ReportType
texter("Read ReportType_Lx.txt", "93%%")

ReportType <- reader("ReportType_Lx.txt")
ReportType <-
  ReportType |>
  dplyr::transmute(
    ReportType = substr(.data$f0, start=1, stop=1),
    Code = substr(.data$f0, start=2, stop=nchar(.data$f0))
  ) |>
  dplyr::compute()
arrow::write_parquet(ReportType, sink = paste0(path_sub, "ReportType.parquet"))

# RouteOfAdm
texter("Read RouteOfAdm_Lx.txt", "94%%")

RouteOfAdm <- reader("RouteOfAdm_Lx.txt")
RouteOfAdm <-
  RouteOfAdm |>
  dplyr::transmute(
    Route = substr(.data$f0, start=1, stop=2),
    Code = substr(.data$f0, start=3, stop=nchar(.data$f0))
  ) |>
  dplyr::compute()
arrow::write_parquet(RouteOfAdm, sink = paste0(path_sub, "RouteOfAdm.parquet"))

# Seriousness
texter("Read Seriousness_Lx.txt", "95%%")

Seriousness <- reader("Seriousness_Lx.txt")
Seriousness <-
  Seriousness |>
  dplyr::transmute(
    Seriousness = substr(.data$f0, start=1, stop=2),
    Code = substr(.data$f0, start=3, stop=nchar(.data$f0))
  ) |>
  dplyr::compute()
arrow::write_parquet(Seriousness, sink = paste0(path_sub, "Seriousness.parquet"))

# SizeUnit
texter("Read SizeUnit_Lx.txt", "96%%")

SizeUnit <- reader("SizeUnit_Lx.txt")
SizeUnit <-
  SizeUnit |>
  dplyr::transmute(
    AmountU = substr(.data$f0, start=1, stop=2),
    Code = substr(.data$f0, start=3, stop=nchar(.data$f0))
  ) |>
  dplyr::compute()
arrow::write_parquet(SizeUnit, sink = paste0(path_sub, "SizeUnit.parquet"))

}
