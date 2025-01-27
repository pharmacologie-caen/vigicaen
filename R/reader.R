#' Internal arrow reader
#'
#' Used in [tb_vigibase()], [tb_who()], [tb_meddra()]
#'
#' @param file_name Character string
#' @param folder Character string
#'
#'
#' @examples
#'
#' demo <-data.frame(f0= c("96548661   32194501051119460820"))
#'
#' tmp_folder <- tempdir()
#'
#' path_base <- paste0(tmp_folder, "/")
#'
#' write.table(demo, file = paste0(path_base, "DEMO.txt"),
#'             row.names = FALSE, quote = FALSE, col.names = FALSE)
#'
#' vigicaen:::reader("DEMO.txt", path_base)

reader <- function(file_name, folder){
  arrow::read_delim_arrow(paste0(folder, file_name),
                          col_names = FALSE,
                          as_data_frame = FALSE,
                          delim = "\t",
                          schema = arrow::schema(f0 = arrow::utf8())
  )
}
