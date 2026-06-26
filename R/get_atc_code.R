#' Get ATC codes (DrecNos or MPIs)
#'
#' @description `r lifecycle::badge('stable')` Collect
#' Drug Record Numbers or Record_Ids associated to one or more ATC classes.
#'
#' @details `get_atc_code()` is an *ID collector* function. Provide `atc_sel` in the same way as `d_sel` in [add_drug()],
#' but remember to specify its method arg as `Record_Id` if
#' `vigilyze` is set to `FALSE`.
#' Vigilyze style means all conditioning of drugs will be retrieved after
#' requesting an ATC class (i.e., drugs are identified with their DrecNos),
#' even if a specific conditioning is not present in the ATC class.
#' This is the default behavior in vigilyze.
#'
#' @param atc_sel A named list of ATC codes. See Details.
#' @param vigilyze A logical. Should ATC classes be retrieved using the vigilyze style? See details
#' @param mp A modified MP data.table. See \code{\link{mp_}}
#' @param thg_data A data.table. Correspondence between ATC codes and Record_Id (usually, it is `thg`)
#' @param verbose A logical. Allows you to see matching ATC classes in the console.
#' @keywords data_management drug atc
#' @export
#' @returns A named list of integers. **DrecNos** if `vigilyze` is set to `TRUE`,
#' or **Record_Ids** if `vigilyze` is set to `FALSE`.
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @seealso \code{\link{mp_}}, \code{\link{thg_}}, [add_drug()], [get_drecno()]
#' @examples
#' # ## Find codes associated with one or more atc classes
#'
#' # First, define which atc you want to use
#'
#' atc_sel <-
#'      rlang::list2(penicillins_gcsf = c("L03AA", "J01CA"),
#'                   ace_inhibitors = c("C09AA")
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
#' # Or you can get Record_Ids (if vigilyze is FALSE)
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
           vigilyze = TRUE,
           verbose = TRUE) {



    atc_sel_renamed <-
      atc_sel |>
      rlang::set_names(
        ~ .x |>
          stringr::str_trim() |>
          stringr::str_to_lower()
      )

    if(!all(names(atc_sel) == names(atc_sel_renamed))){
      cli::cli_warn("names of atc_sel were tolower-ed and trimed")
    }

    if(any(c("Table", "Dataset") %in% class(mp))){
      # automatically collect mp if out of memory
      # since it's a small table
      mp <-
        dplyr::collect(mp)
    }

    if(any(c("Table", "Dataset") %in% class(thg_data))){
      # automatically collect thg_data if out of memory
      # since it's a small table
      thg_data <-
        dplyr::collect(thg_data)
    }


    # core function ----
    core_get_atc_code <-
      function(atc_,
               ATC.code = {{ ATC.code }},
               Record_Id = {{ Record_Id }}) {
        length_code <- stringr::str_count(atc_)

        atc_mpi <-
          thg_data[stringr::str_sub(ATC.code,
                                    start = 1,
                                    end = length_code) == atc_,
                   as.integer(Record_Id)]

        atc_mpi
      }

    # apply core function to each element ----
    # extract mpi (requested even if you want drecnos) ----

    atc_sel_rcid_split <-
      purrr::map(atc_sel_renamed, function(one_sel)
        purrr::map(one_sel,
                   core_get_atc_code)
      )

    atc_sel_rcid <-
      atc_sel_rcid_split |>
      purrr::map(
        purrr::flatten_dbl
      )

    atc_sel_no_match <-
      purrr::imap(
        atc_sel_rcid_split,
        function(one_item, one_name) {
          one_item_name <-
            atc_sel_renamed[[one_name]]

          one_item_name[lengths(one_item) == 0]
        }
      ) |>
      purrr::compact()

    any_match <-
      atc_sel_rcid |>
      purrr::map_lgl(
        ~ length(.x) > 0
      ) |>
      any()

    any_no_match <-
      atc_sel_no_match |>
      purrr::map_lgl(
        ~ length(.x) > 0
      ) |>
      any()

    # vigilyze is TRUE : use get_drecno ----
    # vigilyze is FALSE : return result ----

    output <-
      if (vigilyze) {
        get_drecno(
          d_sel = atc_sel_rcid,
          mp = mp,
          allow_combination = FALSE,
          method = "record_id",
          verbose = FALSE
        )
      } else {
        atc_sel_rcid
      }

    # ---- Render get_atc_code() messages ----

    if ((verbose == TRUE && any_match) || any_no_match)
      cli_h1("get_atc_code()")

    if (verbose == TRUE && any_match) {

      output_label <-
        if(vigilyze) "DrecNo values" else "Record_Id values"

      cli_h2("{col_green({symbol$tick})} Matched ATC classes ({.arg atc_sel})")

      ul <- cli_ul()

      purrr::iwalk(
        output,
        function(one_ids, one_name) {
          if (length(one_ids) > 0) {
            cli::cli_li(
              c(">" = paste0(
                "{.code {one_name}}: ",
                length(unique(one_ids)),
                " ",
                output_label
              ))
            )
          }
        }
      )

      cli_end(ul)

      if(vigilyze){

        cli_par()

        cli::cli_alert_info(
          "vigilyze set to TRUE, extracting DrecNos (?get_atc_code for details)"
        )
      } else {
        cli::cli_alert_info(
          "vigilyze set to FALSE, extracting Record_Ids (?get_atc_code for details)"
        )
      }

      cli::cli_alert_info(
        "Set {.arg verbose} to FALSE to suppress this section."
      )
    }

    if (any_no_match) {
      cli_h2("{col_red({symbol$cross})} Unmatched ATC classes")

      lid <- cli_ul()
      for (i in seq_along(atc_sel_no_match)) {
        cli_li(paste0(
          'In {.code {names(atc_sel_no_match)[i]}}:',
          " {.val {atc_sel_no_match[[i]]}}"
        ))
      }
      cli_end(lid)
    }

    if ((verbose == TRUE && any_match) || any_no_match)
      cli_rule()

    output
  }
