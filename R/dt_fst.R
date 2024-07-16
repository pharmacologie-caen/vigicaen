#' Read fst and convert to data.table
#'
#' Short hand to `as.data.table(read_fst())`. File extension can be omitted.
#'
#' @description `r lifecycle::badge('deprecated')` Output is a data.table.
#' The function is deprecated, with the use of parquet tables. Tables can now
#' be loaded with the arrow::read_parquet() function.
#'
#' @param path_base A character string, providing the path to read from.
#' @param name A character string, the file name.
#' @param ext A character string, optional, specifying the file extension.
#' @keywords import
#' @export
#' @examples
#' # Say you have a data.frame stored in an fst format, such as this one
#' df <- data.frame(a = 1:100, b = rnorm(100))
#'
#' path <- tempdir()
#'
#' fst::write_fst(x = df,
#'               path = paste0(path, "/", "df.fst")
#'               )
#' # Now you have a new session without df.
#' rm(df)
#'
#' # You may import the file directly to data.table format with dt_fst
#' df <- dt_fst(path, "df")

dt_fst <- function(path_base,
                   name = NULL,
                   ext = ".fst"){

  lifecycle::deprecate_warn("0.12.0", "dt_fst()", "arrow::read_parquet()")

  ext <-
    if(!is.null(name) && !grepl(".fst$", name, perl = TRUE)) {
      ext
    } else if(is.null(name) && !grepl(".fst$", path_base, perl = TRUE)){
      ext
    }

  # helps working with the "here" package, or tempdir

  if(!is.null(name) && !grepl("(/|\\\\)$", path_base, perl = TRUE)){
    path_base <-
      paste0(path_base, "/")
  }

  path <- paste0(path_base, name, ext)
  data.table::as.data.table(fst::read_fst(path))
}
