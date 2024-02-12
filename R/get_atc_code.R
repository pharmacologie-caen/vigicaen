#' Get ATC codes (DrecNos or MPIs)
#'
#' This function gets Drug Record Numbers or MedicinalProd_Ids associated to one or more ATC classes.
#'
#' Provide `atc_sel` in the same way as `d_sel` with \code{\link{add_drug}}.
#' Vigilyze style means all conditioning of drugs will be retrieved after requesting an ATC class (i.e., drugs are identified with their DrecNos), even if a specific conditioning is not present in the ATC class. This is the default behavior in vigilyze.
#'
#' @param atc_sel A named list of ATC codes. See Details.
#' @param vigilyze A logical. Should ATC classes be retrieved using the vigilyze style? See details
#' @param mp_short A modified MP data.table. See \code{\link{ex_}}
#' @param thg_data A data.table. Correspondence between ATC codes and MedicinalProd_Id (usually, it is `thg`)
#' @keywords atc
#' @export
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @examples
#' # ## Find codes associated with one or more atc classes
#'
#' # First, define which atc you want to use
#'
#' atc_sel <-
#'      rlang::list2(l03_j01 = c("L03AA", "J01CA"),
#'                   c09aa = c("C09AA")
#'      )
#'
#' # You can get DrecNos for you ATCs (if vigilyze is TRUE)
#'
#' atc_drecno <-
#'   get_atc_code(atc_sel = atc_sel,
#'                mp_short = mp_short_,
#'                thg_data = thg_,
#'                vigilyze = TRUE)
#'
#' # Or you can get MedicinalProd_Ids (if vigilyze is FALSE)
#'
#' atc_mpi <-
#'   get_atc_code(atc_sel = atc_sel,
#'                mp_short = mp_short_,
#'                thg_data = thg_,
#'                vigilyze = FALSE)


get_atc_code <-
  function(atc_sel,
           mp_short,
           thg_data,
           vigilyze = TRUE) {

    # core function ----
    core_get_atc_code <-
      function(atc_) {
        length_code <- stringr::str_count(atc_)

        atc_mpi <-
          thg_data[stringr::str_sub(ATC.code,
                                    start = 1,
                                    end = length_code) == atc_,
                   as.integer(MedicinalProd_Id)]

        atc_mpi
      }

    # apply core function to each element ----
    # extract mpi (requested even if you want drecnos) ----

    atc_sel_mpi <-
      purrr::map(atc_sel, function(one_sel)
        purrr::map(one_sel,
                   core_get_atc_code) %>%
          purrr::flatten_dbl()
      )

    # vigilyze is TRUE : use get_drecno ----
    # vigilyze is FALSE : return result ----

    if (vigilyze) {
      message("vigilyze set to TRUE, extracting DrecNos (?get_atc_code for details)")

      get_drecno(
        d_sel = atc_sel_mpi,
        mp_short = mp_short,
        allow_combination = FALSE,
        method = "mpi_list",
        inspect = FALSE,
        show_all = FALSE
      )

    } else {

      message("vigilyze set to FALSE, extracting MedicinalProd_ids (?get_atc_code for details)")

      atc_sel_mpi
    }

  }
