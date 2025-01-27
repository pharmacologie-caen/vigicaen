#' Internal id list checkers
#'
#' Internal helpers to check id list are appropriate named lists.
#'
#' @param id_list The id list to check
#' @param arg Helper to format the error message.
#' @param call Helper to format the error message.
#'
#' @return An error if the dataset is invalid. Nothing in other cases
#'
#' @examples
#'
#' drug_valid <- data.frame(
#'   DrecNo = 1, UMCReportId = 1, MedicinalProd_Id = 1, Drug_Id = 1)
#'
#'  vigicaen:::check_data_drug(drug_valid, ".data")
#'
#' @name id_list_checkers

NULL

#' @describeIn id_list_checkers named list checker

check_id_list <-
  function(id_list,
           arg = rlang::caller_arg(id_list),
           call = rlang::caller_env()) {

    its_a_list <-
      rlang::is_list(id_list) &
      !any(class(id_list) %in% c("data.frame", "data.table", "Array", "Table"))

    names_exists <-
      !all(is.null(names(id_list)))

    unique_character_type <-
      purrr::map(id_list, rlang::is_character) |>
      purrr::list_c() |> all()

    unique_numeric_type <-
      purrr::map(id_list, rlang::is_double) |>
      purrr::list_c() |> all()

    unique_integer_type <-
      purrr::map(id_list, rlang::is_integer) |>
      purrr::list_c() |> all()


    if (!its_a_list) {
      cli::cli_abort(
        c("{.arg {arg}} is not a (named) {.strong list}.",
          "i" = "{.code data.frame}, {.code data.table}, {.code Array}, and {.code Table} are not allowed.",
          ">" = "Supply a named list to {.arg {arg}}. See ?ex_."),
        call = call
      )
    }

    if (!names_exists) {
      cli::cli_abort(
        c("{.arg {arg}} is not a {.strong named} list.",
          ">" = "Supply a named list to {.arg {arg}}. See ?ex_."),
        call = call
      )
    }

    if(!unique_character_type & !unique_numeric_type & !unique_integer_type) {
      cli::cli_abort(
        c("{.arg {arg}} is not a list of {.strong character} or {.strong numeric} vectors.",
          "i" = "Nested {.code lists} are not allowed.",
          ">" = "Supply a named list of character or numeric vectors to {.arg {arg}}. See ?ex_."),
        call = call
      )
    }
  }

#' @describeIn id_list_checkers numeric list id checker

check_id_list_numeric <-
function(id_list,
         arg = rlang::caller_arg(id_list),
         call = rlang::caller_env()){

  check_id_list(id_list, arg = arg, call = call)

  its_numeric <-
    id_list |>
    purrr::map(
      function(item) {
        rlang::is_integer(item) |
        rlang::is_double(item)
      }
    ) |>
    purrr::list_c() |> all()

  if (!its_numeric) {
    cli::cli_abort(
      c("Type of {.arg {arg}} is not numeric or integer",
        "i" = "Did you provide a list of drug or adr {.strong names}, instead of {.strong ids}?",
        ">" = "Use {.code get_*} functions to collect {.strong ids}."),
      call = call
    )
  }
}

