#' Read parquet and convert to data.table
#'
#' This will load data **IN**-memory. File extension can be omitted.
#'
#' @description `r lifecycle::badge('experimental')` Output is a data.table.
#' For meddra and whodrug tables, it is still a good option to load data in-memory.
#' This function is wrapping `arrow::read_parquet`, `dplyr::collect` and
#' `data.table::as.data.table` altogether.
#' If you want to load **OUT** of memory, use `arrow::read_parquet()`.
#'
#' @param path_base A character string, providing the path to read from.
#' @param name A character string, the file name.
#' @param ext A character string, optional, specifying the file extension.
#' @keywords import
#' @export
#' @examples
#'
#' # Say you have a data.frame stored in an parquet format, such as this one
#' demo <-
#'   data.table(
#'     UMCReportId = c(1, 2, 3, 4),
#'     AgeGroup = c(1, 7, 7, 8)
#'   ) |>
#'   arrow::as_arrow_table()
#'
#' tmp_folder <- tempdir()
#' path_data <- paste0(tmp_folder, "/")
#'
#' arrow::write_parquet(demo,
#'                      sink = paste0(path_data, "demo.parquet")
#' )
#'
#' # Now you have a new session without demo
#' rm(demo)
#'
#' # You may import the file directly to data.table format with dt_parquet
#' demo <-
#'   dt_parquet(path_data, "demo")

dt_parquet <- function(path_base,
                   name = NULL,
                   ext = ".parquet"){

  ext <-
    if(!is.null(name) && !grepl(".parquet$", name, perl = TRUE)) {
      ext
    } else if(is.null(name) && !grepl(".parquet$", path_base, perl = TRUE)){
      ext
    }

  # helps working with the "here" package, or tempdir

  if(!is.null(name) && !grepl("(/|\\\\)$", path_base, perl = TRUE)){
    path_base <-
      paste0(path_base, "/")
  }

  path <- paste0(path_base, name, ext)

  path |>
    arrow::read_parquet() |>
    dplyr::collect() |>
    data.table::as.data.table()
}
