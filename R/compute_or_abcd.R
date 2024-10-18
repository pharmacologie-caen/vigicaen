#' Compute (r)OR
#'
#' @description `r lifecycle::badge('stable')` compute_or_abcd() computes
#' bivariate (reporting) Odds-Ratio and Information Component for a drug-adr pair.
#'
#' @details Beware that input should be a data.table. Significance in pharmacovigilance
#' analysis is only defined if the lower bound of the confidence/credibility
#'  interval is above 1 (i.e. `low_ci > 1`, or `ic_tail > 0`).
#' Actually, the function computes an Odds-Ratio,
#' which is not necessarily a **reporting** Odds-Ratio.
#'
#' @returns A data.table, with ROR, IC, and their confidence interval (at `1 - alpha`).
#' Significance of both (as `signif_or` and `signif_ic`).
#' @param .data The data.table to compute from.
#' @param y A character vector, one or more variable to explain.
#' @param x A character vector, one or more explaining variable.
#' @param alpha Alpha risk.
#' @param na_format Character string to fill NA values in ror and ci legends.
#' @param dig Number of digits for rounding (this argument is passed to `cff`)
#' @keywords disproportionality
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
#'   compute_or_abcd(
#'     y = "a_colitis",
#'     x = "nivolumab"
#'   )
#'
#' # You don't have to use the pipe syntax, if you're not familiar
#'
#' compute_or_abcd(
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
#'   compute_or_abcd(
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
#' compute_or_abcd(
#'   y = many_adrs,
#'   x = many_drugs
#' )

compute_or_abcd <-
  function(.data,
           y,
           x,
           alpha = .05,
           na_format = "-",
           dig = 2) {
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
        n_exp = (.data$a + .data$b) * # n drug
                (.data$a + .data$c) / # n event
                (.data$a + .data$b + .data$c + .data$d), # n pop
        std_er = sqrt((1 / .data$a) +
                        (1 / .data$b) +
                        (1 / .data$c) +
                        (1 / .data$d)
                      ),
        or = .data$a * .data$d / (.data$b * .data$c),
        low_ci = .data$or * exp(- .env$z_val * .data$std_er),
        up_ci  = .data$or * exp(+ .env$z_val * .data$std_er),
        orl = ifelse(
          .data$or %in% c(0, Inf),
          .env$na_format,
          cff(
            num = .data$or,
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
        signif_ic = ifelse(.data$ic_tail > 0, 1, 0)
      )
  }
