#' Summarise categorical variables
#'
#' Shorthand to apply a subjectively outstanding format
#'
#' Twin function of count_cont, but for categorical variables. Valid format options should include `n` (number of patients with the categorical variable at said level), `N` (number of patients with an available value for this variable), and `pc`, percentage between n and N. The format argument should contain at least the words "n", and "N", and optionally the word "pc". Words should be in that order, not mixed.
#' `ncat_max` ensures that you didn't accidentaly provided a continuous variable to `desc_facvar`. If you have many levels for one of your variables, set to `Inf` or high value.
#'
#' @param .data A data.frame, where vf are column names of categorical variables
#' @param vf A character vector
#' @param format A character string, formatting options.
#' @param digits A numeric. Number of digits for the percentage (passed to `pharmacocaen::cff()`).
#' @param pad_width A numeric. Minimum character length of value output (passed to `stringr::str_pad()`).
#' @param ncat_max A numeric. How many levels should be allowed for all variables? See details.
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
            format = "n/N (pc%)",
            digits = 0,
            pad_width = 12,
            ncat_max = 10){

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
      stringr::str_detect(format, "n")

    display_N <-
      stringr::str_detect(format, "N")

    display_pc <-
      stringr::str_detect(format, "pc")


    # ---- establish valid schemes ----

    sch_full <-
      all(display_n, display_N, display_pc)

    sch_nN <-
      all(display_n, display_N)

    if(!any(sch_full, sch_nN)){
      stop("format argument does not reproduce a valid formatting, see details.")
    }

    # ---- extract format ----


      post_n_format <-
        stringr::str_extract(format, "(?<=n).*(?=N)")

      if(sch_full){
        post_N_format <-
          stringr::str_extract(format, "(?<=N).*(?=pc)")
        post_pc_format <-
          stringr::str_extract(format, "(?<=pc).*$")
      }

    if(!sch_full && sch_nN) {
        post_N_format <-
          stringr::str_extract(format, "(?<=N).*$")
      }

    # ---- core ----

    cf_core <- function(one_var) {
      vf_s <- rlang::ensym(one_var)
      r1 <-
        .data %>%
        dplyr::group_by({
          {
            vf_s
          }
        }, .drop = FALSE) %>%
        dplyr::summarise(n = dplyr::n()) %>%
        dplyr::rename(level = {
          {
            vf_s
          }
        }) %>%
        dplyr::mutate(level = as.character(.data$level),
                      var = .env$one_var)

      n_isna <-
        r1 %>%
        dplyr::filter(is.na(.data$level)) %>%
        dplyr::pull(.data$n)

      r1 %>%
        dplyr::mutate(n_missing =
                        if (length(.env$n_isna) > 0) {
                          .env$n_isna
                        } else {
                          0L
                        },
                      n_avail = sum(.data$n) - .data$n_missing) %>%
        dplyr::filter(!is.na(level)) %>%
        dplyr::mutate(
          pc = pharmacocaen::cff(.data$n / .data$n_avail * 100,
                            dig = .env$digits),
          value =
            if (sch_full) {
              paste0(
                .data$n,
                .env$post_n_format,
                .data$n_avail,
                .env$post_N_format,
                .data$pc,
                .env$post_pc_format
              )
            } else if (sch_nN) {
              paste0(.data$n,
                     .env$post_n_format,
                     .data$n_avail,
                     .env$post_N_format)
            },

          value =
            stringr::str_pad(value,
                             width = .env$pad_width,
                             side = "both")

        ) %>%
        dplyr::select(var, level, value, n_avail)
    }

    # ---- apply core ----

    purrr::map(vf, cf_core) %>%
      purrr::list_rbind()
  }
