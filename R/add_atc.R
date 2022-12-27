#' Add ATC columns to a dataset (tidyverse syntax)
#'
#' This function creates an expression that must be evaluated in `j` in a data.table to produce new atc columns.
#'
#' IMPORTANT: At the moment, all `create_x_exp` functions assume default names for data.tables: drug, adr and thg.
#' Vigilyze style means all conditioning of drugs will be retrieved after requesting an ATC class, even if a specific conditioning is not present in the ATC class. This is the default behavior in vigilyze.
#'
#' @param .data The dataset used to identify individual reports (usually, it is `demo`)
#' @param atc_codes A character vector. ATC codes
#' @param vigilyze A logical. Should ATC classes be retrieved using the vigilyze style? See details
#' @param mp_short A modified MP data.table. See \code{\link{ex_}}
#' @param thg_data A data.frame. Correspondence between ATC codes and MedicinalProd_Id (usually, it is `thg`)
#' @keywords atc
#' @export
#' @importFrom dplyr %>%
#' @examples
#' atc_codes <- c("L03", "C")
#'
#' thg <- thg_ # table dans laquelle sont lus les codes ATC
#' demo <- demo_
#' drug <- drug_
#'
#' atc_exp <-
#'   create_atc_exp(atc_codes, mp_short = ex_$mp_short)
#'
#' demo[, c(atc_codes) := eval(atc_exp)]
#' demo[, .N, by = list(L03, C)]

add_atc <-
  function(.data,
           atc_codes,
           vigilyze = TRUE,
           mp_short,
           thg_data)
  {
    drug_expression <-
      if (vigilyze) {
        rlang::expr(
          DrecNo %in%
            find_drecno(
              thg[substr(ATC.code, start = 1, stop = length_code) == atc_code,
                  as.integer(MedicinalProd_Id)],
              mp_short,
              allow_combination = FALSE,
              method = "mpi_list"
            )[["DrecNo"]])
      } else {
        rlang::expr(MedicinalProd_Id %in%
                      thg[substr(ATC.code, start = 1, stop = length_code) == atc_code,
                          as.integer(MedicinalProd_Id)])
      }

    atc_exp <- function(atc_code, env, drug_expression) {
      drug_expression <- rlang::enexpr(drug_expression)
      length_code <- nchar(atc_code)
      eval(rlang::expr(data.table::fifelse(UMCReportId %in%
                                             drug[!!drug_expression, unique(UMCReportId)],
                                           1, 0)), envir = env)
    }

    e_l <- lapply(atc_codes, function(x) {
      rlang::call2(rlang::expr(!!atc_exp), x, rlang::expr(.SD), drug_expression)
    })

    var_expr <- rlang::call2(rlang::expr(list),
                             !!!e_l)

    var_expr
  }
