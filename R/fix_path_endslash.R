#' Internal: Fix path end slash
#'
#' This function adds a slash at the end of a path if it is missing.
#'
#' Note, the function does NOT check that the path exists.
#' This internal fixer is notably useful to make `here::here()` use more fluent.
#' And also with `tempdir()`.
#'
#' @param .path A character string with the path to be fixed.
#'
#' @return A character string with the path, fixed with its end slash.
#'
#' @seealso [tb_vigibase()], [tb_meddra()], [tb_subset()], [dt_parquet()]
#'
#' @examples
#' vigicaen:::fix_path_endslash("C:/Users/username/Documents")

fix_path_endslash <-
  function(.path){
    if(!grepl("(/|\\\\)$", .path, perl = TRUE)){
      paste0(.path, "/")
    } else {
      .path
    }
  }
