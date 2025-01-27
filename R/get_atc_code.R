#' Get ATC codes (DrecNos or MPIs)
#'
#' @description `r lifecycle::badge('stable')` Collect
#' Drug Record Numbers or MedicinalProd_Ids associated to one or more ATC classes.
#'
#' @details `get_atc_code()` is an *ID collector* function. Provide `atc_sel` in the same way as `d_sel` in [add_drug()],
#' but remember to specify its method arg as `MedicinalProd_Id` if
#' `vigilyze` is set to `FALSE`.
#' Vigilyze style means all conditioning of drugs will be retrieved after
#' requesting an ATC class (i.e., drugs are identified with their DrecNos),
#' even if a specific conditioning is not present in the ATC class.
#' This is the default behavior in vigilyze.
#'
#' @param atc_sel A named list of ATC codes. See Details.
#' @param vigilyze A logical. Should ATC classes be retrieved using the vigilyze style? See details
#' @param mp A modified MP data.table. See \code{\link{mp_}}
#' @param thg_data A data.table. Correspondence between ATC codes and MedicinalProd_Id (usually, it is `thg`)
#' @keywords data_management drug atc
#' @export
#' @returns A list of **DreNos** if `vigilyze` is set to `TRUE`,
#' or a list of **MedicinalProd_Ids** if `vigilyze` is set to `FALSE`
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @seealso \code{\link{mp_}}, \code{\link{thg_}}, [add_drug()], [get_drecno()]
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
#'                mp = mp_,
#'                thg_data = thg_,
#'                vigilyze = TRUE)
#'
#' # Or you can get MedicinalProd_Ids (if vigilyze is FALSE)
#'
#' atc_mpi <-
#'   get_atc_code(atc_sel = atc_sel,
#'                mp = mp_,
#'                thg_data = thg_,
#'                vigilyze = FALSE)


get_atc_code <-
  function(atc_sel,
           mp,
           thg_data,
           vigilyze = TRUE) {



    atc_sel_renamed <-
      atc_sel |>
      rlang::set_names(
        ~ .x |>
          stringr::str_trim() |>
          stringr::str_to_lower()
      )

    if(!all(names(atc_sel) == names(atc_sel_renamed))){
      warning("names of atc_sel were tolower-ed and trimed")
    }

    if("Table"  %in% class(mp)){
      # automatically collect mp if out of memory
      # since it's a small table
      mp <-
        dplyr::collect(mp)
    }

    if("Table"  %in% class(thg_data)){
      # automatically collect thg_data if out of memory
      # since it's a small table
      thg_data <-
        dplyr::collect(thg_data)
    }


    # core function ----
    core_get_atc_code <-
      function(atc_,
               ATC.code = {{ ATC.code }},
               MedicinalProd_Id = {{ MedicinalProd_Id }}) {
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
      purrr::map(atc_sel_renamed, function(one_sel)
        purrr::map(one_sel,
                   core_get_atc_code) |>
          purrr::flatten_dbl()
      )

    # vigilyze is TRUE : use get_drecno ----
    # vigilyze is FALSE : return result ----

    if (vigilyze) {
      cli::cli_alert_info(
        "vigilyze set to TRUE, extracting DrecNos (?get_atc_code for details)"
        )

      get_drecno(
        d_sel = atc_sel_mpi,
        mp = mp,
        allow_combination = FALSE,
        method = "mpi_list",
        verbose = FALSE
      )

    } else {

      cli::cli_alert_info(
        "vigilyze set to FALSE, extracting MedicinalProd_ids (?get_atc_code for details)"
        )

      atc_sel_mpi
    }

  }
