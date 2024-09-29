#' Summarise categorical variables
#'
#' Shorthand to apply a subjectively outstanding format
#'
#' Twin function of count_cont, but for categorical variables.
#' Valid format options should include `n_` (number of patients with the
#' categorical variable at said level), `N_` (number of patients with an
#' available value for this variable), and `pc_`, percentage between n and N.
#' The format argument should contain at least the words "n", and "N",
#' and optionally the word "pc". Words should be in that order, not mixed.
#' `ncat_max` ensures that you didn't accidentaly provided a continuous
#' variable to `desc_facvar`. If you have many levels for one of your variables,
#' set to `Inf` or high value.
#'
#' @param .data A data.frame, where vf are column names of categorical variables
#' @param vf A character vector
#' @param format A character string, formatting options.
#' @param digits A numeric. Number of digits for the percentage (passed to interval formatting function).
#' @param pad_width A numeric. Minimum character length of value output (passed to `stringr::str_pad()`).
#' @param ncat_max A numeric. How many levels should be allowed for all variables? See details.
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @export
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
#' desc_facvar(vf = c("hypertension", "smoke_status"), .data = df1)

desc_facvar <-
  function( .data,
            vf,
            format = "n_/N_ (pc_%)",
            digits = 0,
            pad_width = 12,
            ncat_max = 10){

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

    lev_check <- function(one_var){
      n_lev <-
        length(unique(.data[[one_var]]))

      if(n_lev > ncat_max){
        stop(paste0("too many levels detected in ", one_var, ", see details."))
      }
    }

    purrr::map(vf, lev_check)

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

    # ---- core ----

    cf_core <- function(one_var) {
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
          pc = cff(.data$n / .data$n_avail * 100,
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
              .data$pc
            ),

          value =
            stringr::str_pad(.data$value,
                             width = .env$pad_width,
                             side = "both")

        ) |>
        dplyr::select(dplyr::all_of(c("var", "level", "value", "n_avail")))
    }

    # ---- apply core ----

    purrr::map(vf, cf_core) |>
      purrr::list_rbind()
  }
