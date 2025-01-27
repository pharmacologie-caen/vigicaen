#' Get DrecNo from drug names or MedicinalProd_Id
#'
#' @description `r lifecycle::badge('stable')` Collect
#'  Drug Record Numbers associated to one or more drugs.
#'
#' @details `get_drecno()` is an *ID collector* function.
#' Collected IDs can be used to create drug columns in datasets
#' like `demo`, `link`, etc. (see `vignette("basic_workflow")`)
#'
#' @section Argument `verbose`:
#'
#' The `verbose` argument is here to let you check
#' the result of `get_drecno()`. This is an important step in your
#' project setup: You must ensure that the drugs you are looking for
#' are correctly matched.
#'
#' @section Argument `d_sel`:
#'
#' `d_sel` must be a named list of character vectors.
#' To learn why, see `vignette("basic_workflow")`.
#' Names of `d_sel` are automatically lowered and trimed.
#'
#' @section Matching drugs:
#'
#' With "drug_name" method, either exact match or perl regex
#' match can be used. The latter
#' is built upon lookarounds to ensure that a string does not match to
#' composite drug names including the string,
#' i.e. `trastuzumab emtasine` for `trastuzumab`, or close names
#' like `alitretinoin` when looking for `tretinoin`.
#'
#' Exact match is used for "mpi_list" method.
#'
#' @section Choosing a method:
#'
#' "drug_name" let you work with drug names. It's likely to be
#' the appropriate method in most of the cases.
#'
#' "mpi_list" is used when you have a list of MedicinalProd_Ids.
#' A drug can have multiple MedicinalProd_Ids, corresponding to
#' different packagings. The MedicinalProd_Id matching is typically used to identify DrecNo(s)
#' contained in an ATC class (extracted from `thg`), since not all MPI of drugs are present in `thg` (explanations in [get_atc_code()]).
#'
#' @section WHO names:
#'
#' WHO names are attributed to drugs by... the WHO.
#' A drug only has one WHO name, but can have multiple
#' international nonproprietary names (e.g. "tretinoin" and
#' "all-trans retinoic acid").
#'
#' You should use WHO names to ensure proper identification of
#' drugs and DrecNos, especially if you work with combinations.
#'
#' @section Argument `allow_combination`:
#'
#' Fixed associations of drugs refers to specialty containing
#' more than one active ingredient (for example,
#' acetylsalicylic acid and clopidogrel).
#' In VigiLyze, the default is **NOT** to account for
#' these fixed associations. For example, when you
#' call "acetylsalicylic acid" in VigiLyze, you don't have the cases
#' reported with the fixed-association "acetylsalicylic acid; clopidogrel"
#'  **unless the substances were distinctly coded by the reporter.**
#' Here, the default is to find a drug even if it is prescribed in a fixed association.
#' Importantly, when retrieving fixed-association drugs, the non-of-interest
#' drug alone drecno is not found, hence the cases related to this drug will not be added to those of the drug of interest.
#'
#' @param d_sel A named list. Selection of drug names or medicinalprod_id. See details
#' @param mp A modified MP data.table. See \code{\link{mp_}}
#' @param allow_combination A logical. Should fixed associations including the drug of interest be retrieved? See details.
#' @param method Should DrecNo be found from drug names or from MedicinalProd_Id?
#' @param verbose A logical. Allows you to see matching drug names in the console.
#' Turn to FALSE once you've checked the matching.
#' @param inspect `r lifecycle::badge('deprecated')` Use `verbose` instead.
#' @param show_all `r lifecycle::badge('deprecated')`  Use `verbose` instead.
#' @keywords data_management drug atc
#' @export
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @import cli
#' @seealso [add_drug()], [get_atc_code()]
#' @examples
#'
#' # ## Get drecnos for a list a drugs. Check spelling and use WHO name, #' in lowercase
#'
#' d_sel_names <- list(
#'   nivolumab = "nivolumab",
#'   ipilimumab = "ipilimumab",
#'   nivo_ipi = c("nivolumab", "ipilimumab")
#'   )
#'
#' # Read mp with get_drecno(), to identify drugs without combinations
#'
#' # Take the time to read the matching drugs. Did you forget a drug?
#'
#' d_drecno <-
#'   get_drecno(d_sel_names,
#'              mp = mp_,
#'              allow_combination = FALSE,
#'              method = "drug_name")
#' d_drecno
#'
#' # And DrecNos of drugs allowing for combinations
#'
#' d_drecno <-
#'   get_drecno(d_sel = d_sel_names,
#'              mp = mp_,
#'              allow_combination = TRUE,
#'              method = "drug_name")
#' d_drecno

get_drecno <- function(
    d_sel,
    mp,
    allow_combination = TRUE,
    method = c("drug_name", "mpi_list"),
    verbose = TRUE,
    show_all = deprecated(),
    inspect = deprecated()
    ){

  check_id_list(d_sel)

  d_sel_renamed <-
    d_sel |>
    rlang::set_names(
      ~ .x |>
        stringr::str_trim() |>
        stringr::str_to_lower()
    )

  check_data_mp(mp)

  if("Table" %in% class(mp)){
    # automatically collect mp if out of memory
    # since it's a small table
    mp <-
      dplyr::collect(mp)
  }

  method <- rlang::arg_match(method)

  # Check if user has supplied `inspect`.
  if (lifecycle::is_present(inspect)) {

    # Signal the deprecation to the user
    lifecycle::deprecate_soft(
      when = "0.14.1",
      what = "get_drecno(inspect)",
      with = "get_drecno(verbose)"
    )
  }

  # Check if user has supplied `show_all`.
  if (lifecycle::is_present(show_all)) {

    # Signal the deprecation to the user
    lifecycle::deprecate_soft(
      when = "0.14.1",
      what = "get_drecno(show_all)",
      details = c("i" = "Unmatching DrecNos or MedicinalProd_Ids will be shown by default.")
    )
  }

  # find_mpi <- function(x_mpi_list, env = mp,
  #                      MedicinalProd_Id = {{ MedicinalProd_Id }})
  #   eval(rlang::expr(MedicinalProd_Id %in% x_mpi_list), envir = env)


  # ---- drug name finding ----

  if (method == "drug_name") {
    # Pick one
    find_select <-
      if (allow_combination) {
        find_combination
      } else {
        find_isolated
      }

    res_list <-
      d_sel_renamed |>
      purrr::map(function(d_n){
        # 2 level map, because find_drug_and_check_exist is working with an atomic character vector

        # at individual drug level (each element of one d_sel item)

        # run core
        each_d_sel_item_output <-
          purrr::map(d_n,
                       function(one_drug_name_)
                         find_drug_and_check_exist(
                           one_drug_name_,
                           finder = find_select,
                           mp = mp
                         )
                       )

        # gather drecno tables
        all_d_sel_item_drecnos <-
          each_d_sel_item_output |>
          purrr::map(
            function(x)
              x[["drecno_table"]]
          ) |>
          purrr::list_rbind(names_to = "drug")

        # gather no match
        all_d_sel_item_no_match <-
          each_d_sel_item_output |>
          purrr::map(
            function(x)
              x[["d_no_match"]]
          ) |> unlist()

        # gather not who
        all_d_sel_item_not_who <-
          each_d_sel_item_output |>
          purrr::map(
            function(x)
              x[["d_not_who"]]
          ) |> unlist()

        # return a gathered output (one per d_sel item)
        return(list(drecno_table = all_d_sel_item_drecnos,
                    d_no_match = all_d_sel_item_no_match,
                    d_not_who = all_d_sel_item_not_who))

        })

    # collect drecno tables
    res_list_dt <-
      res_list|> purrr::map(function(x) x[["drecno_table"]])

    res_list_no_match <-
      res_list |>  purrr::map(function(x) x[["d_no_match"]])

    res_list_not_who <-
      res_list |> purrr::map(function(x) x[["d_not_who"]])

  }

  # drug_finder_and_exist_checker(c("nivolumab", "ipilimumab"))

  # ---- MedicinalProd_Id finding ----

  if(method == "mpi_list") {

    res_list_dt <-
      purrr::map(d_sel_renamed, function(d_n)
        mp |>
          dplyr::filter(
            !!find_mpi(d_n)
            ) |>
          dplyr::mutate(who = .data$Sequence.number.1 == "01" &
                          .data$Sequence.number.2 == "001") |>
          dplyr::distinct(.data$DrecNo, .data$drug_name_t, .data$who)
          # dplyr::mutate(drug = 1) # for compatibility with inspect instructions
      )

  }

  # ---- Prepare messages and warning triggers ----

  # 1. - Names transformation

  any_renamed_name <-
    !all(names(d_sel) == names(d_sel_renamed))

  # 2. - Matched drugs

  # 3. - Warnings on no match

  if (method == "drug_name") {
    any_no_match <-
      res_list_no_match |> purrr::map(function(x)
        ! is.null(x)) |>
      unlist() |> any()

    # 4. -  # Warnings on not who name

    any_not_who <-
      res_list_not_who |> purrr::map(function(x)
        ! is.null(x)) |>
      unlist() |> any()
  }

  # ---- Render get_drecno() messages ----

  if (any_renamed_name |
      (method == "drug_name" && any(any_no_match, any_not_who, verbose))
  )
    cli_h1("get_drecno()")

  if (any_renamed_name == TRUE){
    msg_getdrecno_renaming(d_sel, d_sel_renamed)
  }

  if (verbose == TRUE |
      (method == "drug_name" && any(any_no_match, any_not_who))
      ) {

    cli_h2("{.arg d_sel}: Matching drugs")

    if (verbose == TRUE)
      msg_getdrecno_match(d_sel, res_list_dt)

    if (method == "drug_name" && any_no_match == TRUE)
      msg_getdrecno_no_match(res_list_no_match)

    if (method == "drug_name" && any_not_who == TRUE){
      msg_get_drecno_not_who(res_list_not_who)

      if (allow_combination == TRUE){
        cli_warn(
          c("You might have missed combinations.",
            ">" = paste0("Switch to ", col_cyan("{symbol$info}"), " WHO names.")
          )
        )
        }
      }
  }

  if(method == "mpi_list" && allow_combination)
    cli_alert_info("{.arg allow_combination} is ignored if {.arg method} = {.val mpi_list}.")

  if (any_renamed_name |
      (method == "drug_name" && any(any_no_match, any_not_who, verbose))
  )
    cli_rule()


    output <-
      purrr::map(res_list_dt, function(r_l, DrecNo = {{ DrecNo }})
        r_l |>
          dplyr::filter(
            .data$who == TRUE
          ) |>
          dplyr::pull(.data$DrecNo) |>
          unique())

    return(output)

}

# Helpers -----------------------------------------------------

find_drug_and_check_exist <-
  function(one_drug_name, # character string, a single element of
           # ONE item of d_sel e.g. d_sel[[1]][[1]]
           finder, # One of find_isolated, find_combination
           mp,
           Sequence.number.1 = {{ Sequence.number.1 }},
           Sequence.number.2 = {{ Sequence.number.2 }},
           DrecNo = {{ DrecNo }},
           drug_name_t = {{ drug_name_t }}) {
    # single drug checking - core of get_drecno()

    check_length <-
      function(x,
               arg = rlang::caller_arg(x),
               call = rlang::caller_env()){


        if (length(x) > 1) {

          cli::cli_abort(
            c(
              "{.arg {arg}} has length > 1.",
              "x" = "{.arg d_sel} structure is probably incorrect."
            ),
            call = call,
            .internal = TRUE

          )
        }
      }

    # Function is meant to be used for a single drug at a time.
    # Might want to check `d_sel` structure.
    check_length(one_drug_name)

    # drug finder
    drecno_list <-
      mp |>
      dplyr::filter(
        !!finder(one_drug_name
               # , env = mp)
        )
      ) |>
      dplyr::mutate(
        who = .data$Sequence.number.1 == "01" &
          .data$Sequence.number.2 == "001") |>
      dplyr::distinct(
        .data$DrecNo,
        .data$drug_name_t,
        .data$who)

    who_match <-
      drecno_list |>
      dplyr::filter(.data$who == TRUE) |>
      dplyr::summarise(is_who = dplyr::n() > 0) |>
      dplyr::pull(.data$is_who)
    # in case of combination, find_combination may find both who and non-who names
    # if providing a who name,
    # which still means that matching was fine.
    # that's why who_match is TRUE even if only one who name is found.


    # if there is no match, put it in d_no_match
    if (nrow(drecno_list) == 0){

      d_no_match <- one_drug_name
    }

    # if no who name, find the proper who name and put it in d_not_who
    if (nrow(drecno_list) > 0 &&
        who_match == FALSE) {
      drecno <-
        drecno_list |>
        dplyr::filter(
          !!find_isolated(one_drug_name)
          # there can be combinations and there can be multiple MP_Ids
          ) |>
        dplyr::pull(.data$DrecNo) |>
        unique()

      who_name <-
        mp |>
        dplyr::filter(
          .data$DrecNo == drecno  &
            .data$Sequence.number.1 == "01" &
            .data$Sequence.number.2 == "001"
        ) |>
        dplyr::pull(.data$drug_name_t) |>
        unique() # there can be multiple packagings for a non WHO name drug

      d_not_who <- one_drug_name |> rlang::set_names(who_name)
    }

    output <-
      list(
        drecno_table = drecno_list,
        d_no_match = if (nrow(drecno_list) == 0){d_no_match},
        d_not_who = if (nrow(drecno_list) > 0 && who_match == FALSE){d_not_who}
      )

    # return
    return(output)

  }

find_combination <- function(x_drug_name){
  x_drug_name <-
    gsub("\\(", "\\\\(", x_drug_name)

  x_drug_name <- # so that parenthesis are appropriately escaped
    gsub("\\)", "\\\\)", x_drug_name)

  rlang::expr(grepl(paste0("(?<![[:alpha:]])", !!x_drug_name, "(?![\\s[:alpha:]])"),
                         .data$drug_name_t,
                         perl = TRUE))
  # negative lookbehind: x is not preceeded by alphabetic characters
  # negative lookahead: x is not followed by a space or an alphabetic character.
}

find_isolated <- function(x_drug_name){
  rlang::expr(.data$drug_name_t == !!x_drug_name)
}

find_mpi <- function(x_mpi_list)
  rlang::expr(.data$MedicinalProd_Id %in% !!x_mpi_list)


msg_getdrecno_no_match <-
  function(res_list_no_match
           ){

    res_list_no_match_compact <-
      purrr::compact(res_list_no_match)

    msg_no_match <-
      function() {

        cli_par()

        cli_h3(paste0(col_yellow("!"), " Some drugs were not found"))

        cli_end()
        cli_par()

        lid <- cli_ul()
        for (i in seq_along(res_list_no_match_compact)) {
          cli_li(paste0(
            'In {.code {names(res_list_no_match_compact)[i]}}:',
            col_red(' {symbol$cross} '),
            "{.val {res_list_no_match_compact[[i]]}}",
            ''
          ))

        }

        cli_end(lid)
      }

    msg_no_match()
  }

msg_get_drecno_not_who <-
  function(res_list_not_who
           ){
    res_list_not_who_compact <-
      purrr::compact(res_list_not_who)

    msg_not_who <-
      function() {

        cli_par()
        cli_h3(paste0(col_yellow("!"), " Some drugs are not WHO name"))
        cli_end()

        cli_par()
        cli_text(paste( col_yellow('{symbol$warning}'), "Not WHO name   | ",
                        col_cyan("{symbol$info}"),"   WHO name"))
        cli_end()

        cli_par()

        lid <- cli_ul()
        for (i in seq_along(res_list_not_who_compact)) {
          cli_li(paste0(
            'In {.code {names(res_list_not_who_compact)[i]}}:'))
          ulid <- cli_ul()

          for(j in seq_along(res_list_not_who_compact[[i]])) {
            cli_li(
              paste( col_yellow('{symbol$warning}'),
                     "{.val {res_list_not_who_compact[[i]][j]}} | ",
                     col_cyan("{symbol$info}"),
                     " {names(res_list_not_who_compact[[i]][j])}"))
          }
          cli_end(ulid)

        }

        cli_end(lid)
      }

    msg_not_who()
  }

msg_getdrecno_renaming <-
  function(d_sel, d_sel_renamed){
    ds_rename_position <-
      !names(d_sel) %in% names(d_sel_renamed)

    msg_ds_length <-
      if (sum(ds_rename_position) <= 5) {
        sum(ds_rename_position)
      } else {
        5
      }

    msg_ds_rename <-
      function() {
        cli_h2("{.arg d_sel}: Checking names")

        cli_par()

        cli_alert_warning(c("names of {.arg d_sel} were lowered and trimed"))

        cli_end()
        cli_par()

        lid <- cli_ul()
        for (i in 1:msg_ds_length) {
          cli_li(paste0(
            "{.code {names(d_sel)[ds_rename_position][i]}}",
            ' {symbol$arrow_right} ',
            "{.code {names(d_sel_renamed)[ds_rename_position][i]}}"
          ))

        }
        if (sum(ds_rename_position) > 5) {
          cli_li("...")
          cli_li("{symbol$info} Showing first 5 only")
        }

        cli_end(lid)
      }

    msg_ds_rename()
  }

msg_getdrecno_match <-
  function(d_sel, res_list_dt){
    msg_match <- function() {
      lines <-
        purrr::map(res_list_dt, function(r_l)
          r_l |>
            dplyr::filter(.data$who == TRUE) |>
            dplyr::pull(.data$drug_name_t) |>
            unique() # when requesting drugs with several nonproprietary names
        )

      drug_match_printer <-
        function(d_sel,
                 arg = rlang::caller_arg(d_sel),
                 call = rlang::caller_env()) {
          cli_h3(paste0(col_green("{symbol$tick}"), " Matched drugs"))
        }

      cli_par()
      drug_match_printer(d_sel)
      cli_end()
      cli_par()
      lines_cli <-
        lines |> purrr::imap(function(l_, n_){

          l_lab <- if(length(l_) == 0) "{symbol$cross} No match" else "{.val {l_}}"

          cli::cli_inform(
            c(">" = paste0(
              "{.code {n_}}: ",
              l_lab)
            )
          )
        })

      cli_end()
      cli_par()
      cli::cli_alert_info(
        "Set {.arg verbose} to FALSE to suppress this section."
      )
      cli_end()

    }
   msg_match()
  }
