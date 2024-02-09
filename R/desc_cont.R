#' Summarise continuous variables
#'
#' Shorthand to apply a subjectively outstanding format
#'
#' That's very close to what tableone would do, except I have hand on the output.
#' This makes it much easier to map to nice labelling thereafter.
#' The format argument shows the output of the function. You can change square and round brackets, spaces, separators... YOu can choose to display median, and interquartile range and/or range. The format argument should contain at least the word "median", and optionally the words "q1" and "q3", or "min" and "max". Words should be in that order, not mixed.
#' The analogous for categorical variables is `count_facvar`.
#'
#' @param .data A data.frame, where vc are column names of continuous variables
#' @param vc A character vector, list of column names. Should only contain continuous variables
#' @param format A character string. How would you like the output? See details.
#' @param digits A numeric. How many digits? This argument calls `pharmacocaen::cff()`
#' @importFrom stats median qnorm quantile var
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @export
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
#' desc_cont(vc = c("age", "bmi"),
#'            .data = df,
#'            format = "median (q1;q3)")

desc_cont <-
  function(.data,
           vc,
           format = "median (q1-q3) [min-max]",
           digits = 1
           ){

    # checkers ----

    # only numeric or integer vars ----
    col_classes <-
      purrr::map(.data, class) %>%
      purrr::keep_at(vc) %>%
      purrr::list_simplify()

    if(!all(col_classes %in% c("numeric", "integer"))){
      stop("Non numeric or integer columns selected")
    }

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

    # ---- establish valid schemes ----

    sch_full <-
      all(display_median, display_q1, display_q3,
          display_min, display_max)

    sch_median_iqr <-
      all(display_median, display_q1, display_q3)

    sch_median_range <-
      all(display_median, display_min, display_max)

    if(!any(sch_full, sch_median_iqr, sch_median_range)){
      stop("format argument does not reproduce a valid formatting, see details.")
    }

    # ---- extracting format ----

    interq_format <-
      if(sch_full || sch_median_iqr){
        stringr::str_extract(format, "(?<=q1).*(?=q3)")
      }

    interr_format <-
      if(sch_full || sch_median_range){
        stringr::str_extract(format, "(?<=min).*(?=max)")
      }


    if(sch_full){
      post_median_format <-
        stringr::str_extract(format, "(?<=median).*(?=q1)")
      post_iqr_format <-
        stringr::str_extract(format, "(?<=q3).*(?=min)")
      post_r_format <-
        stringr::str_extract(format, "(?<=max).*$")
    }

    if(!sch_full && sch_median_iqr){
      post_median_format <-
        stringr::str_extract(format, "(?<=median).*(?=q1)")
      post_iqr_format <-
        stringr::str_extract(format, "(?<=q3).*$")
    }

    if(!sch_full && !sch_median_iqr && sch_median_range){
      post_median_format <-
        stringr::str_extract(format, "(?<=median).*(?=min)")
      post_r_format <-
        stringr::str_extract(format, "(?<=max).*$")
    }

    # ---- core ----

    cc_core <- function(one_var){
      vc_s <- rlang::ensym(one_var)

      check_all_na <-
        all(is.na(.data[[one_var]]))

      if (check_all_na)
        message("var ", one_var, " is empty")

      r1 <-
        if (!check_all_na) {
          .data %>%
            summarise(
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

              across(c(median, q1, q3, min, max),
                     ~ pharmacocaen::cff(.x, dig = .env$digits)),

              value =
                if(sch_full){
                  paste0(
                    .data$median,
                    .env$post_median_format,
                    .data$q1,
                    .env$interq_format,
                    .data$q3,
                    .env$post_iqr_format,
                    .data$min,
                    .env$interr_format,
                    .data$max,
                    .env$post_r_format
                  )
                } else if(sch_median_iqr){
                  paste0(
                    .data$median,
                    .env$post_median_format,
                    .data$q1,
                    .env$interq_format,
                    .data$q3,
                    .env$post_iqr_format
                  )
                } else if(sch_median_range){
                  paste0(
                    .data$median,
                    .env$post_median_format,
                    .data$min,
                    .env$interr_format,
                    .data$max,
                    .env$post_r_format
                  )
                },
              n_missing =
                sum(is.na({{ vc_s }})),
              n_avail =
                sum(!is.na({{ vc_s }}))
            ) %>%
            select(var, level, value, n_avail)
        } else {
          .data %>%
            summarise(
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

      r1
    }

    # ---- apply cc_core ----

    purrr::map(
      vc,
      cc_core
    ) %>%
      purrr::list_rbind()
  }
