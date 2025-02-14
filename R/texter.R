#' Internal text displayer
#'
#' Used in tb_main, tb_who, tb_sub. % Should be escaped in step.
#'
#' @param msg Character string
#' @param step Character string
#'
#' @noRd
#' @examples
#'
#' vigicaen:::texter("I am doing this step", "3%%")

texter <- function(msg, step){
  cat(sprintf(
    paste0("\r",
           msg |>
             stringr::str_pad(
               side = "right",
               width = 28
             ),
           step |>
             stringr::str_pad(
               side = "left",
               width = 5
             )
    )
  ))

  invisible(msg)
}
