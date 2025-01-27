#' Compute disproportionality
#'
#' @description `r lifecycle::badge('stable')` Computes
#' bivariate (reporting) Odds-Ratio and Information Component for a drug-adr pair.
#'
#' @details Significance in pharmacovigilance
#' analysis is only defined if the lower bound of the confidence/credibility
#'  interval is above 1 (i.e. `low_ci > 1`, or `ic_tail > 0`).
#' Actually, the function computes an Odds-Ratio,
#' which is not necessarily a **reporting** Odds-Ratio.
#'
#' @returns A data.table, with ROR, IC, and their
#' confidence/credibility interval (at `1 - alpha`).
#' Significance of both (as `signif_or` and `signif_ic`, if `export_raw_values` is TRUE).
#' @param .data The data.table to compute from.
#' @param y A character vector, one or more variable to explain (usually an adr).
#' @param x A character vector, one or more explaining variable (usually a drug).
#' @param alpha Alpha risk.
#' @param na_format Character string to fill NA values in ror and ci legends.
#' @param dig Number of digits for rounding (this argument is passed to `cff`)
#' @param export_raw_values A logical. Should the raw values be exported?
#' @param min_n_obs A numeric, compute disproportionality only for pairs
#' with at least `min_n_obs` cases.
#' @keywords disproportionality
#' @returns A data.table with columns
#' \itemize{
#' \item `y` and `x`, same as input
#' \item `n_obs` the number of observed cases
#' \item `n_exp` the number of expected cases
#' \item `orl` the formatted Odds-Ratio
#' \item `or_ci` the formatted confidence interval
#' \item `ic` the Information Component
#' \item `ic_tail` the tail probability of the IC
#' \item `ci_level` the confidence interval level
#' \item Additional columns, if `export_raw_values` is `TRUE`:
#' \item `a`, `b`, `c`, `d` the counts in the contingency table
#' \item `std_er` the standard error of the log(OR)
#' \item `or` the Odds-Ratio
#' \item `low_ci` the lower bound of the confidence interval
#' \item `up_ci` the upper bound of the confidence interval
#' \item `signif_or` the significance of the Odds-Ratio
#' \item `signif_ic` the significance of the Information Component
#' }
#' @export
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @importFrom data.table data.table .N
#' @seealso [compute_or_mod()], [add_drug()], [add_adr()]
#' @examples
#' # Say you want to perform a disproportionality analysis between colitis and
#' # nivolumab among ICI cases
#'
#' demo <-
#'   demo_ |>
#'   add_drug(
#'     d_code = ex_$d_drecno,
#'     drug_data = drug_
#'   ) |>
#'   add_adr(
#'     a_code = ex_$a_llt,
#'     adr_data = adr_
#'   )
#'
#' demo |>
#'   compute_dispro(
#'     y = "a_colitis",
#'     x = "nivolumab"
#'   )
#'
#' # You don't have to use the pipe syntax, if you're not familiar
#'
#' compute_dispro(
#'     .data = demo,
#'     y = "a_colitis",
#'     x = "nivolumab"
#'   )
#'
#'
#' # Say you want to compute more than one univariate ror at a time.
#'
#' many_drugs <-
#'   names(ex_$d_drecno)
#'
#' demo |>
#'   compute_dispro(
#'     y = "a_colitis",
#'     x = many_drugs
#'   )
#'
#'
#' # could do the same with adrs
#'
#' many_adrs <-
#'   names(ex_$a_llt)
#'
#' demo |>
#' compute_dispro(
#'   y = many_adrs,
#'   x = many_drugs
#' )
#'
#' # Export raw values if you want to built plots, or other tables.
#'
#' demo |>
#'   compute_dispro(
#'     y = "a_colitis",
#'     x = "nivolumab",
#'     export_raw_values = TRUE
#'   )
#'
#' # Set a minimum number of observed cases to compute disproportionality
#'
#' demo |>
#'  compute_dispro(
#'  y = "a_colitis",
#'  x = "nivolumab",
#'  min_n_obs = 5
#'  )

compute_dispro <-
  function(.data,
           y,
           x,
           alpha = .05,
           na_format = "-",
           dig = 2,
           export_raw_values = FALSE,
           min_n_obs = 0) {
    z_val <- qnorm(1 - alpha / 2)

    # data mask
    dm <-
      data.frame(
        one_x = c(1, 1, 0, 0),
        one_y = c(1, 0, 1, 0)
      )

    # letters in the contingency table
    cases <-
      letters[1 : 4]

    var_to_export <-
      if(export_raw_values){
        c("y", "x", "n_obs", "n_exp", "or", "or_ci", "ic", "ic_tail", "ci_level",
          "a", "b", "c", "d", "std_er", "or_raw", "low_ci", "up_ci",
          "signif_or", "signif_ic")
      } else {
        c("y", "x", "n_obs", "n_exp", "or", "or_ci", "ic", "ic_tail", "ci_level")
      }

    # extract counts from source table
    c_or_abcd_core <-
      function(.data, one_y, one_x) {

        # rename variables
        var <- c(one_y, one_x) |>
          rlang::set_names(
            c("one_y", "one_x")
          )

        # contingency table
        ct <-
          .data |>
          dplyr::rename(dplyr::all_of(.env$var)) |>
          dplyr::group_by(.data$one_y, .data$one_x) |>
          dplyr::summarise(n_cas = dplyr::n(),
                           .groups = "keep") |>
          dplyr::collect()

        # data manage contingency table
        # starting with data mask
        dm |>
          dplyr::left_join(ct, by = c("one_y", "one_x")) |>
          dplyr::mutate(
            n_cas = ifelse(is.na(.data$n_cas), 0, .data$n_cas)
          ) |>
          dplyr::mutate(y = .env$one_y, x = .env$one_x) |>
          dplyr::relocate(dplyr::all_of(c("y", "x"))) |>
          # have a, b, c, d counts sorted before adding letters
          dplyr::arrange(
            dplyr::desc(.data$one_y),
            dplyr::desc(.data$one_x)) |>
          dplyr::mutate(
            cases = .env$cases
          ) |>
          dplyr::select(dplyr::all_of(c("y", "x", "cases", "n_cas"))) |>
          tidyr::pivot_wider(
            names_from  = dplyr::all_of("cases"),
            values_from = dplyr::all_of("n_cas")
          )
      }

    # all drugs and all adrs count tables
    ad_aa_counts <-
      x  |>
      purrr::map(
        function(one_x_) {
          y |>
            purrr::map(
              function(one_y_)
                .data |>
                c_or_abcd_core(
                  one_y = one_y_,
                  one_x = one_x_
                )
            )
        }
      ) |>
     unlist(recursive = FALSE) |>
      purrr::list_rbind()

    # compute disproportionality
    ad_aa_counts |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(c("a", "b", "c", "d")), ~ as.numeric(.x)),
        n_obs = .data$a,
        n_exp = (.data$a + .data$b) * # n drug
                (.data$a + .data$c) / # n event
                (.data$a + .data$b + .data$c + .data$d), # n pop
        std_er = sqrt((1 / .data$a) +
                        (1 / .data$b) +
                        (1 / .data$c) +
                        (1 / .data$d)
                      ),
        or_raw = .data$a * .data$d / (.data$b * .data$c),
        low_ci = .data$or_raw * exp(- .env$z_val * .data$std_er),
        up_ci  = .data$or_raw * exp(+ .env$z_val * .data$std_er),
        or = ifelse(
          .data$or_raw %in% c(0, Inf),
          .env$na_format,
          cff(
            num = .data$or_raw,
            dig = .env$dig,
            method = "num_only"
          )
        ),
        or_ci = ifelse(
          .data$low_ci %in% c(NaN, 0, Inf),
          .env$na_format,
          cff(
            low_ci = .data$low_ci,
            up_ci  = .data$up_ci,
            dig    = .env$dig,
            method = "ci"
          )
        ),
        ic = log((.data$a + .5) / (.data$n_exp + .5), base = 2),
        ic_tail = ic_tail(
          n_obs = .data$a,
          n_exp = .data$n_exp,
          p = .env$alpha / 2
        ),
        ci_level  = paste0((1 - .env$alpha) * 100, "%"),
        signif_or = ifelse(.data$low_ci  > 1, 1, 0),
        signif_ic = ifelse(.data$ic_tail > 0, 1, 0),
        # don't show results for pairs with less than min_n_obs
        dplyr::across(
          dplyr::all_of(
            c("n_exp", "ic", "ic_tail",
              "a", "b", "c", "d", "std_er", "or_raw", "low_ci", "up_ci",
              "signif_or", "signif_ic")
          ),
          function(num_col)
            dplyr::if_else(.data$a < .env$min_n_obs,
                    NA_real_,
                    num_col)
        ),
        dplyr::across(
          dplyr::all_of(
            c("or", "or_ci", "ci_level"
              )
          ),
          function(chr_col)
            dplyr::if_else(.data$a < .env$min_n_obs,
                           .env$na_format,
                           chr_col)
        )
      ) |>
      dplyr::select(dplyr::all_of(var_to_export))
  }
