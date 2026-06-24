#' Internal id list checkers
#'
#' Internal helpers to check id list are appropriate named lists.
#'
#' @param id_list The id list to check
#' @param arg Helper to format the error message.
#' @param call Helper to format the error message.
#'
#' @returns An error if the dataset is invalid. Nothing in other cases
#' @noRd

NULL

check_id_list <-
  function(id_list,
           arg = rlang::caller_arg(id_list),
           call = rlang::caller_env()) {

    its_a_list <-
      rlang::is_list(id_list)

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

      what_it_is <- class(id_list)

      cli::cli_abort(
        c("{.arg {arg}} must be a named {.strong list}.",
          "x" = "{.arg {arg}} is of class {what_it_is}.",
          ">" = "Supply a named list to {.arg {arg}}. See ?ex_."),
        call = call
      )
    }

    if (!names_exists) {
      cli::cli_abort(
        c("{.arg {arg}} must be a {.strong named} list.",
          "x" = "{.arg {arg}} items have no name.",
          ">" = "Supply a named list to {.arg {arg}}. See ?ex_."),
        call = call
      )
    }

    names_of_list <- names(id_list)
    dup_names <- unique(names_of_list[duplicated(names_of_list)])

    if (length(dup_names) > 0) {
      dup_positions <- purrr::map_chr(
        dup_names,
        function(dup_name) {
          pos <- which(names_of_list == dup_name)
          cli::format_inline("{.val {dup_name}} at position{?s} {pos}")
        }
      )

      cli::cli_abort(
        c(
          "{.arg {arg}} must have {.strong unique} names.",
          "x" = "Redundant name{?s} found in {.arg {arg}}:",
          rlang::set_names(dup_positions, rep("i", length(dup_positions)))
        ),
        call = call
      )
    }

    if(!unique_character_type & !unique_numeric_type & !unique_integer_type) {
      cli::cli_abort(
        c("{.arg {arg}} items must all be of type {.strong character} or {.strong numeric}.",
          "i" = "Nested {.code lists} are not allowed.",
          ">" = "Supply a named list of character or numeric vectors to {.arg {arg}}. See ?ex_."),
        call = call
      )
    }
  }

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

    non_numeric <-
      id_list |>
      purrr::imap(
        function(item, name) {
          if(!(rlang::is_integer(item) |
              rlang::is_double(item))) {
            name
          }
        }
      ) |>
      purrr::compact() |>
      unlist()

    cli::cli_abort(
      c("Type of {.arg {arg}} must be numeric or integer.",
        "x" = "Non-numeric/integer elements detected in {.val {non_numeric}}.",
        "i" = "Did you provide a list of drug or adr {.strong names}, instead of {.strong ids}?",
        ">" = "Use {.code get_*} functions to collect numeric {.strong ids}."),
      call = call
    )
  }
}

