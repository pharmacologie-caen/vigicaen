#' Read parquet and convert to data.table
#'
#' @description `r lifecycle::badge('experimental')` dt_parquet() will
#' load data IN- our OUT- of memory. File extension can be omitted.
#'
#' @details  Output is a data.table.
#' For meddra and whodrug tables, it is still a good option to load data in-memory.
#' This function is wrapping `arrow::read_parquet()`, `dplyr::collect()` and
#' `data.table::as.data.table()` altogether.
#' If you want to load **OUT** of memory, set arg `in_memory` to FALSE.
#' **Be careful that doing so will change the function output format**.
#'
#' @param path_base A character string, providing the path to read from.
#' @param name A character string, the file name.
#' @param ext A character string, optional, specifying the file extension.
#' @param in_memory Logical, should data be loaded in memory?
#' @keywords import
#' @seealso [tb_main()], [tb_who()], [tb_sub()], [tb_meddra()]
#' @export
#' @examples
#'
#' # Say you have a data.frame stored in a parquet format, such as this one
#' demo <-
#'   data.table::data.table(
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
                   ext = ".parquet",
                   in_memory = TRUE){

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

  if(in_memory == TRUE){
    path |>
    arrow::read_parquet(as_data_frame = TRUE) |>
    data.table::as.data.table()
  } else {
    path |>
      arrow::read_parquet(as_data_frame = FALSE)
  }
}
