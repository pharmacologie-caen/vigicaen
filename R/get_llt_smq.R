#' Extract low level terms from SMQs
#'
#' @description `r lifecycle::badge('stable')` Collect llts from `smq_list`
#' and `smq_content` data.tables, given an SMQ.
#'
#' @details `get_llt_smq()` is an *ID collector* function. SMQ stands for Standardized MedDRA query.
#' `get_llt_smq()`only works with NON-algorithmic SMQs
#' (this status is given in the smq_list table).
#' See \code{\link{smq_list_}} and \code{\link{smq_content_}}. You can choose
#' between the narrow and the broad scope of the SMQ.
#' If you want to work with the SOC hierarchy, use [get_llt_soc()].
#'
#' @param smq A named list of character vector of length 1.
#' @param smq_scope A character vector. One of "narrow" or "broad".
#' @param smq_list A data.table. A list of SMQs.
#' @param smq_content A data.table. A list of SMQs content.
#' @param smq_list_content  `r lifecycle::badge('deprecated')`
#' @keywords data_management meddra smq llt
#' @seealso [get_llt_soc()]
#' @export
#' @examples
#' ## Finding llt codes for Embolism (SMQ)
#'
#' smq_sel <- rlang::list2(
#'   embolism = "Embolic and thrombotic events, venous (SMQ)"
#'  )
#' get_llt_smq(smq_sel,
#'                 smq_scope = "narrow",
#'                 smq_list = smq_list_,
#'                 smq_content = smq_content_
#'                 )
#'
#' # You can query multiple SMQs in one item, and query high level SMQs
#' smq_sel2 <-
#'   rlang::list2(
#'     sepsis = c("Sepsis (SMQ)","Toxic-septic shock conditions (SMQ)"),
#'     ischemic_heart_disease = c("Ischaemic heart disease (SMQ)"),
#'   )
#'
#' get_llt_smq(smq_sel2,
#'             smq_scope = "narrow",
#'             smq_list = smq_list_,
#'             smq_content = smq_content_
#'             )

get_llt_smq <-
  function(
    smq,
    smq_scope = c("narrow", "broad"),
    smq_list,
    smq_content,
    smq_list_content = deprecated()
  ){

    check_id_list(smq)

    # prevent error from old version written scripts (<v0.14.1)
    check_data_smqlist(smq_list)

    smq <-
      purrr::map(smq, function(s_)
      ifelse(
        stringr::str_detect(s_, "\\s\\(SMQ\\)$"),
        s_,
        paste0(s_, " (SMQ)")
      )
      )

    smq_scope <- rlang::arg_match(smq_scope)

    smq_scope_code <-
      dplyr::case_when(
        smq_scope == "narrow" ~ c("2"),
        smq_scope == "broad" ~ c("1", "2"),
        TRUE ~ "this is an error"
      )

    if("Table"  %in% class(smq_list)){
      # automatically collect smq_list and smq_content if out of memory
      # since they are small tables
      smq_list <-
        dplyr::collect(smq_list)
    }


    if("Table"  %in% class(smq_content)){
      smq_content <-
        dplyr::collect(smq_content)
    }

    # Check if user has supplied smq_list_content.
    if (lifecycle::is_present(smq_list_content)) {

      # Signal the deprecation to the user
      lifecycle::deprecate_soft(
        when = "0.14.1",
        what = "get_llt_smq(smq_list_content)",
        with = "get_llt_smq(smq_list)",
        details = "and `smq_content`"
        )

    }

    # ---- Collect and flag smq codes ----

    smq_codes <-
      purrr::map(
        smq,
        function(one_smq){
          one_smq_codes <-
            purrr::map(one_smq, function(one_smq_name)
              find_smq(one_smq_name, smq_list)
              )

          # gather all correct matchs
          all_smq_item_codes <-
            one_smq_codes |>
            purrr::map(
              function(x)
                c(x[["match_exact"]],
                  x[["match_sub"]])
            ) |>
            purrr::list_c()

          # gather sub smq matchs
          all_smq_item_submatchs <-
            one_smq_codes |>
            purrr::map(
              function(x){

                name_for_sub <-
                  if(length(names(x[["match_exact"]])) > 0)
                    names(x[["match_exact"]])
                  else
                    "No exact match"

                list(v1 = names(x[["match_sub"]])) |>
                  # v1 temporary, overwritten right next
                rlang::set_names(
                  name_for_sub
                )
              }
            ) |>
            unlist(recursive = FALSE)

          # gather failures
          all_smq_item_failures <-
            one_smq_codes |>
            purrr::map(
              function(x)
                x[["match_failed"]]
            ) |> unlist()

          return(
            list(
              all_smq_codes = all_smq_item_codes,
              all_smq_submatchs = all_smq_item_submatchs,
              all_smq_failures = all_smq_item_failures
            )
          )
        }
      )

    # collect smq codes
    res_list_codes <-
      smq_codes |> purrr::map(function(x)
        x[["all_smq_codes"]])

    # collect submatchs
    res_list_submatchs <-
      smq_codes |> purrr::map(function(x)
        x[["all_smq_submatchs"]])

    # collect failures
    res_list_failures <-
      smq_codes |> purrr::map(function(x)
        x[["all_smq_failures"]])

    # extract llt_codes from smq_codes

    llt_list <-
      purrr::map(smq_codes, function(one_smq) {
        smq_content |>
          dplyr::filter(
            .data$smq_code %in% one_smq$all_smq_codes &
              .data$term_scope %in% smq_scope_code &
              # according to scope
              .data$term_status == "A"
            # only active terms
          ) |>
          dplyr::pull(.data$term_code) |>
          unique()
      })

    # ---- Prepare messages and warning triggers ----

    # all sub smq matchs

    any_sub <-
      smq_codes |>
      purrr::map(function(one_smq) {
        one_smq[["all_smq_submatchs"]] |>
          purrr::map(# double map... because submatches are still nested.
            function(one_smq_sub)
              length(one_smq_sub) > 0)
      }) |>
      purrr::list_c() |>
      purrr::list_c() |>
      any()

    # any failure

    any_failure <-
      smq_codes |>
      purrr::map(function(one_smq)
        length(one_smq[["all_smq_failures"]]) > 0) |>
      purrr::list_c() |>
      any()

    # ---- Render get_llt_smq() messages ----

    if (any_sub | any_failure)
      cli_h1("get_llt_smq()")

    if (any_sub == TRUE) {
      msg_getlltsmq_sub(res_list_submatchs)
    }

    if (any_failure == TRUE) {
      msg_getlltsmq_failure(res_list_failures)
    }

    if (any_sub | any_failure)
      cli_rule()


    return(llt_list)

  }

# Helpers ---------------------------------

find_smq <- function(
    one_smq_name,
    smq_list
){
  check_length <-
    function(x,
             arg = rlang::caller_arg(x),
             call = rlang::caller_env()){


      if (length(x) > 1) {

        cli::cli_abort(
          c(
            "{.arg {arg}} has length > 1.",
            "x" = "{.arg smq} structure is probably incorrect."
          ),
          call = call,
          .internal = TRUE

        )
      }
    }

  # Function is meant to be used for a single smq at a time.
  check_length(one_smq_name)

  # check for any match

  exact_match <-
    smq_list |>
    dplyr::filter(
      .data$smq_name == one_smq_name
    ) |>
    dplyr::select(
      dplyr::all_of(c("smq_code", "smq_algorithm"))
    )

  no_match <-
    if(nrow(exact_match) == 0){
      one_smq_name
    }

  # Should do a few things:

  # check for sub SMQs

  prep_osn <-
    gsub("\\(", "\\\\(", one_smq_name)

  prep_osn <- # so that parenthesis are appropriately escaped
    gsub("\\)", "\\\\)", prep_osn)

  sub_smqs_match <-
    smq_list |>
    dplyr::filter(
      grepl(prep_osn, .data$smq_description)
    ) |>
    dplyr::select(
      dplyr::all_of(c("smq_code", "smq_name", "smq_algorithm"))
    )

  # check that it's not an algorithmic one

  any_algorithmic <-
    any(exact_match$smq_algorithm != "N") |
    any(sub_smqs_match$smq_algorithm != "N")

  if(any_algorithmic){
    cli::cli_abort(
      c(
        "SMQ {.val {one_smq_name}} or one of its Sub-SMQs is/are algorithmic",
        "x" = "Algorithmic SMQs are not handled by {.code get_llt_smq()}."
      ),
      call = rlang::caller_env()
    )
  }

  # return the corresponding codes

  output <-
    list(match_exact  = exact_match$smq_code |>
           rlang::set_names(one_smq_name),
         match_sub    = sub_smqs_match$smq_code |>
           rlang::set_names(sub_smqs_match$smq_name),
         match_failed = no_match)

  return(output)

}

msg_getlltsmq_sub <-
  function(res_list_submatchs

  ){

    res_list_submatchs_compact <-
      purrr::map(
        res_list_submatchs,
        function(r_l){
          r_l |>
            purrr::keep(
              function(x) length(x) > 0
            )
        }
          ) |>
      purrr::compact()


    msg_sub <-
      function() {

        cli_par()

        cli_h3(paste0(col_cyan("{symbol$info}"), " Sub-SMQs found"))

        cli_end()

        cli_par()
        cli_text(paste( col_green('{symbol$info}'), "High SMQ   | ",
                        col_cyan("{symbol$tick}"),"   Sub SMQ(s)"))
        cli_end()

        cli_par()

        lid <- cli_ul()
        for (i in seq_along(res_list_submatchs_compact)) {
          cli_li(paste0(
            'In {.code {names(res_list_submatchs_compact)[i]}}:'))
          ulid <- cli_ul()

          for(j in seq_along(res_list_submatchs_compact[[i]])) {
            cli_li(
              paste0(
                     col_green("{symbol$info}"),
                     " {.val {names(res_list_submatchs_compact[[i]][j])}} | ",
                     col_cyan("{symbol$tick}"),
                     "  {res_list_submatchs_compact[[i]][[j]]}")
            )
          }
          cli_end(ulid)

        }

        cli_end(lid)
      }

    msg_sub()
  }

msg_getlltsmq_failure <-
  function(res_list_failures){

    res_list_failures_compact <-
      purrr::compact(res_list_failures)


    msg_fail <-
      function() {

        cli_par()

        cli_h3(paste0(col_red("{symbol$cross}"), " Unmatched SMQs"))

        cli_end()

        cli_par()

        lid <- cli_ul()
        for (i in seq_along(res_list_failures_compact)) {
          cli_li(paste0(
            'In {.code {names(res_list_failures_compact)[i]}}:',
            " {.val {res_list_failures_compact[[i]]}}"))
        }

        cli_end(lid)
      }

    msg_fail()
  }
