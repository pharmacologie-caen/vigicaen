#' Summarise categorical variables
#'
#' @description `r lifecycle::badge('stable')` Summarize categorical data and
#' handle output format.
#'
#' @details Many other packages provide tools to summarize data. This one is just
#' the package author's favorite.
#' Important `format` inputs are
#' \itemize{
#' \item `n_` number of patients with the categorical variable at said level
#' \item `N_` the first quartile number of patients with an available value for this variable
#' \item `pc_` percentage of n / N
#' }
#' The format argument should contain at least the words "n_", "N_",
#' and optionally "pc_".
#' `ncat_max` ensures that you didn't provided a continuous
#' variable to [desc_facvar()]. If you have many levels for one of your variables,
#' set to `Inf` or high value.
#' Equivalent for continuous data is [desc_cont()].
#'
#' @param .data A data.frame, where `vf` are column names of categorical variables
#' @param vf A character vector
#' @param format A character string, formatting options.
#' @param digits A numeric. Number of digits for the percentage (passed to interval formatting function).
#' @param pad_width A numeric. Minimum character length of value output (passed to `stringr::str_pad()`).
#' @param ncat_max A numeric. How many levels should be allowed for all variables? See details.
#' @param export_raw_values A logical. Should the raw values be exported?
#' @importFrom rlang .data
#' @importFrom rlang .env
#'
#' @returns A data.frame with columns
#' \itemize{
#'  \item `var` the variable name
#'  \item `level` the level of the variable
#'  \item `value` the formatted value with possible number of cases `n_`,
#'  number of available cases `N_`, and percentage `pc_`, depending on
#'  format argument.
#'  \item `n_avail` the number of cases with available data
#'  for this variable.
#'  }
#'
#' @export
#' @seealso [desc_cont()]
#'
#' @examples
#' df1 <-
#'   data.frame(
#'     smoke_status = c("smoker", "non-smoker",
#'            "smoker", "smoker",
#'            "smoker", "smoker",
#'            "non-smoker"
#'            ),
#'    hypertension = c(1, 1, 0, 1, 1, 1, 1),
#'     age = c(60, 50, 56, 49, 75, 69, 85),
#'     bmi = c(18, 30, 25, 22, 23, 21, 22)
#'   )
#'
#' # Use default formatting
#' desc_facvar(.data = df1, vf = c("hypertension", "smoke_status"))
#'
#' # Use custom formatting
#' desc_facvar(.data = df1,
#'            vf = c("hypertension", "smoke_status"),
#'            format = "n_ out of N_, pc_%",
#'            digits = 1)
#'
#' # You might want to export raw values, to run plotting or
#' # other formatting functions
#'
#' desc_facvar(.data = df1,
#'             vf = c("hypertension", "smoke_status"),
#'             export_raw_values = TRUE)

desc_facvar <-
  function( .data,
            vf,
            format = "n_/N_ (pc_%)",
            digits = 0,
            pad_width = 12,
            ncat_max = 10,
            export_raw_values = FALSE){

    # only columns present in the dataset
    if(!all(vf %in% names(.data))){
      err_msg <-
        paste0(vf[!vf %in% names(.data)], collapse = ", ")
      stop(
        paste0(
          "Column(s) ",
          err_msg, " is(are) absent of .data")
      )
    }

    # ---- number of levels checker ----

    lev_length <-
      vf |>
      purrr::map(function(one_var)
        length(unique(.data[[one_var]]))
        ) |>
      purrr::list_c() |>
      rlang::set_names(vf)

    if(any(lev_length > ncat_max)){

      oob_vars <-
        lev_length[lev_length > ncat_max]

      cli_abort(
        c("Too many levels detected in: {names(oob_vars)}",
          "x" = paste0(
            "Number of levels: {oob_vars} ",
            "exceeded {.arg ncat_max}({ncat_max})"),
          "i" = "Did you pass a continuous variable to {.code desc_facvar()}?",
          ">" = "Set {.arg ncat_max} to suppress this error."
      )
      )
    }

    # ---- formatting arguments ----

    display_n <-
      stringr::str_detect(format, "n_")

    display_N <-
      stringr::str_detect(format, "N_")

    display_pc <-
      stringr::str_detect(format, "pc_")

    many_params <-
      c("n_", "N_", "pc_") |>
      rlang::set_names() |>
      purrr::map(
        ~ stringr::str_count(format, .x)
      )

    if(!any(display_n, display_N, display_pc)){
      stop("format arg does not contain any of n_, N_, or pc_. Please provide at least one.")
    }

    many_params |>
      purrr::imap(function(counts, param_name)
        if(counts > 1)
          stop(paste0("format code `", param_name, "` is present more than once in `format`."))
        )

    var_to_export <-
      if(export_raw_values){
        c("var", "level", "value", "n_avail", "n", "pc")
      } else {
        c("var", "level", "value", "n_avail")
      }

    # ---- core ----

    cf_core <- function(
                  one_var) {
      vf_s <- rlang::ensym(one_var)

      r1 <-
        .data |>
        dplyr::group_by({
          {
            vf_s
          }
        }, .drop = FALSE) |>
        dplyr::summarise(n = dplyr::n()) |>
        dplyr::rename(level = {
          {
            vf_s
          }
        }) |>
        dplyr::mutate(level = as.character(.data$level),
                      var = .env$one_var)

      n_isna <-
        r1 |>
        dplyr::filter(is.na(.data$level)) |>
        dplyr::pull(.data$n)

      r1 |>
        dplyr::mutate(n_missing =
                        if (length(.env$n_isna) > 0) {
                          .env$n_isna
                        } else {
                          0L
                        },
                      n_avail = sum(.data$n) - .data$n_missing) |>
        dplyr::filter(!is.na(.data$level)) |>
        dplyr::mutate(
          pc = .data$n / .data$n_avail * 100,

          pc_fmt = cff(.data$pc,
                       dig = .env$digits),
          value =
            .env$format |>
            stringr::str_replace(
              "n_",
              cff(.data$n)
            ) |>
            stringr::str_replace(
              "N_",
              cff(.data$n_avail)
            ) |>
            stringr::str_replace(
              "pc_",
              .data$pc_fmt
            ),

          value =
            stringr::str_pad(.data$value,
                             width = .env$pad_width,
                             side = "both")

        ) |>
        dplyr::select(dplyr::all_of(.env$var_to_export))
    }

    # ---- apply core ----

    purrr::map(
      vf,
      cf_core
      ) |>
      purrr::list_rbind()
  }
