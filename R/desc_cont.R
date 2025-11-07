#' Summarise continuous variables
#'
#' @description `r lifecycle::badge('stable')` Summarize continuous data and
#' handle output format.
#'
#' @details Many other packages provide tools to summarize data. This one is just
#' the package author's favorite.
#' This makes it much easier to map to nice labeling thereafter.
#' The `format` argument shows the output of the function. You can change square
#' and round brackets, spaces, separators... Important `format`
#' inputs are
#' \itemize{
#' \item `median` the median value
#' \item `q1` the first quartile
#' \item `q3` the third quartile
#' \item `min` the minimum value
#' \item `max` the maximum value
#' }
#' The analogous for categorical variables is [desc_facvar()].
#'
#' @param .data A data.frame, where `vc` are column names of continuous variables
#' @param vc A character vector, list of column names. Should only contain continuous variables
#' @param format A character string. How would you like the output? See details.
#' @param digits A numeric. How many digits? This argument calls internal formatting function
#' @param export_raw_values A logical. Should the raw values be exported?
#'
#' @returns A data.frame with columns
#' \itemize{
#' \item `var` the variable name
#' \item `level` NA, it is provided to have a consistent output
#' with [desc_facvar()]
#' \item `value` the formatted value with possibly the median,
#' interquartile range, and range (see details)
#' \item `n_avail` the number of cases with available data for this
#' variable.
#' }
#'
#' @importFrom stats median qnorm quantile var
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @export
#' @seealso [desc_facvar()]
#'
#' @examples
#' df <-
#'   data.frame(
#'     smoke_status = c("smoker", "non-smoker",
#'            "smoker", "smoker",
#'            "smoker", "smoker",
#'            "non-smoker"
#'            ),
#'     age = c(60, 50, 56, 49, 75, 69, 85),
#'     bmi = c(18, 30, 25, 22, 23, 21, 22)
#'   )
#'
#' # Use default formatting
#'
#' desc_cont(.data = df, vc = c("age", "bmi"))
#'
#' # Use custom formatting
#'
#' desc_cont(.data = df,
#'           vc = c("age", "bmi"),
#'           format = "median (q1;q3)"
#'           )
#'
#' # You might want to export raw values, to run plotting or
#' # other formatting functions
#'
#' desc_cont(.data = df, vc = c("age", "bmi"),
#'           export_raw_values = TRUE)

desc_cont <-
  function(.data,
           vc,
           format = "median (q1-q3) [min-max]",
           digits = 1,
           export_raw_values = FALSE
           ){

    # checkers ----

    check_columns_in_data(.data, vc)

    # only numeric or integer vars ----

    check_columns_numeric_integer(.data, vc)

    # ---- formatting arguments ----

    display_median <-
      stringr::str_detect(format, "median")

    display_q1 <-
      stringr::str_detect(format, "q1")

    display_q3 <-
      stringr::str_detect(format, "q3")

    display_min <-
      stringr::str_detect(format, "min")

    display_max <-
      stringr::str_detect(format, "max")

    if(!any(display_median,
            display_q1,
            display_q3,
            display_min,
            display_max)
       ){
      error_required_format_values(
        format = format,
        required_values = c("median", "q1", "q3", "min", "max")
      )

    }

    var_to_export <-
      if(export_raw_values){
        c("var", "level", "value", "n_avail",
          "median", "q1", "q3", "min", "max")
      } else {
        c("var", "level", "value", "n_avail")
      }

    # ---- core ----

    cc_core <- function(one_var){
      vc_s <- rlang::ensym(one_var)

      check_all_na <-
        if (any(c("Table", "Dataset", "arrow_dplyr_query") %in% class(.data))) {

          n_na <-
            .data |>
            dplyr::summarise(
              all_na = all(is.na({{ vc_s }}))
            ) |>
            dplyr::collect()

          n_na$all_na

          } else {

          all(is.na(.data[[one_var]]))

        }

      if (check_all_na)
        message("var ", one_var, " is empty")

      r1 <-
        if (!check_all_na) {
          .data |>
            dplyr::collect() |> # compatibility with arrow
            dplyr::summarise(
              var = one_var,
              level = NA_character_,
              median    =
                median({{ vc_s }}, na.rm = TRUE),
              q1 =
                quantile({{ vc_s }}, .25, na.rm = TRUE),
              q3  =
                quantile({{ vc_s }}, .75, na.rm = TRUE),
              min =
                min({{ vc_s }}, na.rm = TRUE),
              max =
                max({{ vc_s }}, na.rm = TRUE),

              dplyr::across(dplyr::all_of(c("median", "q1", "q3", "min", "max")),
                     ~ cff(.x, dig = .env$digits),
                     .names = "{.col}_fmt"),

              value =
                .env$format |>
                stringr::str_replace_all(
                  "median",
                  paste0(.data$median_fmt)
                ) |>
                stringr::str_replace_all(
                  "q1",
                  paste0(.data$q1_fmt)
                ) |>
                stringr::str_replace_all(
                  "q3",
                  .data$q3_fmt
                ) |>
                stringr::str_replace_all(
                  "min",
                  .data$min_fmt
                ) |>
                stringr::str_replace_all(
                  "max",
                  .data$max_fmt
                )
              ,
              n_missing =
                sum(is.na({{ vc_s }})),
              n_avail =
                sum(!is.na({{ vc_s }}))
            ) |>
            dplyr::select(dplyr::all_of(var_to_export))
        } else {
          .data |>
            dplyr::summarise(
              var = one_var,
              level = NA_character_,
              value = "-",
              n_avail = sum(!is.na({
                {
                  vc_s
                }
              }))
            )
        }

      r1 |>
        dplyr::collect()
    }

    # ---- apply cc_core ----

    purrr::map(
      vc,
      cc_core
    ) |>
      purrr::list_rbind()
  }

# Helpers ------------

error_columns_in_data <-
  function(
    col_arg,
    must_be_in,
    missing_cols,
    call = rlang::caller_env()
    ){
    cli::cli_abort(
      message =
        c("{.arg {col_arg}} columns must be in {.arg {must_be_in}}.",
          "x" = "The followings were not found in {.arg {must_be_in}}: {.val {missing_cols}}."),
      class  = "columns_not_in_data",
      col_arg = col_arg,
      must_be_in = must_be_in,
      missing_cols = missing_cols,
      call = call
    )
  }

check_columns_in_data <-
  function(
    .data,
    cols,
    call = rlang::caller_env()
    )  {
      if(!all(cols %in% names(.data))){
        missing_cols <-
          cols[!cols %in% names(.data)]

        error_columns_in_data(
          col_arg = rlang::caller_arg(cols),
          must_be_in = ".data",
          missing_cols = missing_cols,
          call = call
        )
      }
    }

error_columns_numeric_integer <-
  function(
    col_arg,
    not_numeric_integer,
    call = rlang::caller_env()
    ){
    cli::cli_abort(
      message =
        c("{.arg {col_arg}} columns must be numeric or integer.",
          "x" = "The following {?is/are} not numeric/integer: {.val {not_numeric_integer}}."),
      class  = "columns_not_numeric_integer",
      col_arg = col_arg,
      not_numeric_integer = not_numeric_integer,
      call = call
    )
  }

check_columns_numeric_integer <-
  function(.data, cols, call = rlang::caller_env())  {

    if (any(c("Table", "Dataset", "arrow_dplyr_query") %in% class(.data))) {
      # for arrow objects, extract type from schema
      col_classes <-
        arrow::schema(.data)$fields |>
        purrr::map( ~ .x$type$ToString()) |>
        rlang::set_names(purrr::map_chr(arrow::schema(.data)$fields, ~ .x$name)) |>
        purrr::keep_at(cols) |>
        purrr::list_simplify()
    } else {
      col_classes <-
        purrr::map(.data, class) |>
        purrr::keep_at(cols) |>
        purrr::list_simplify()
    }

    if (!all(col_classes %in% c("numeric", "integer", "double"))) {
      not_numeric_integer <-
        cols[!(col_classes %in% c("numeric", "integer", "double"))]


      # not_numeric_integer <-
      #   cols[!(purrr::map_lgl(.data[cols], is.numeric) |
      #            purrr::map_lgl(.data[cols], is.integer))]

      error_columns_numeric_integer(
        col_arg = rlang::caller_arg(cols),
        not_numeric_integer = not_numeric_integer,
        call = call
      )
    }
  }

error_required_format_values <-
  function(
    format,
    required_values,
    call = rlang::caller_env()
  ){
    cli::cli_abort(
      message =
        '{.arg format} must contain at least one of {glue::glue_collapse(required_values, sep = ", ", last = " or ")}',
      class  = "required_format_values",
      format = format,
      required_values = required_values,
      call = call
    )
  }
