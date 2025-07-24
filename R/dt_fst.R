#' Read fst and convert to data.table
#'
#' @description `r lifecycle::badge('deprecated')` Short hand to `as.data.table(read_fst())`.
#' File extension can be omitted.
#'
#' @details Output is a data.table.
#' The function is deprecated, with the use of parquet tables. Tables can now
#' be loaded **IN**-memory or **OUT** of memory with \code{\link{dt_parquet}}.
#'
#' @param path_base A character string, providing the path to read from.
#' @param name A character string, the file name.
#' @param ext A character string, optional, specifying the file extension.
#' @keywords import
#' @export
#' @returns A data.table, read from `path_base/(name)`.
#' @seealso [dt_parquet()], [tb_vigibase()], [tb_who()], [tb_meddra()]
#' @examples
#'
#' # dt_fst is deprecated and will generate an error

dt_fst <- function(path_base,
                   name = NULL,
                   ext = ".fst"){

  lifecycle::deprecate_stop("0.12.0", "dt_fst()", "dt_parquet()")

}
