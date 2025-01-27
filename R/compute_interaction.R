#' Compute interaction disproportionality
#'
#' @description `r lifecycle::badge('experimental')` Returns the information
#' component of interaction for a set of 3 variables,
#' usually 2 drugs and an adr.
#'
#' @details Significance is similar to usual disproportionality (see [compute_dispro()]).
#'
#' @returns A data.table, with Information Component (IC) of interaction,
#'  and its credibility interval (at `1 - alpha`).
#' Significance as `signif_ic`, if `export_raw_values` is TRUE).
#' @param .data The data.table to compute from.
#' @param y A character vector, one or more variable to explain.
#' @param x A character vector, one or more explaining variable.
#' @param z A character vector, one or more explaining variable.
#' @param alpha Alpha risk.
#' @param na_format Character string to fill NA values in ror and ci legends.
#' @param dig Number of digits for rounding (this argument is passed to `cff`)
#' @param export_raw_values A logical. Should the raw values be exported?
#' @param min_n_obs A numeric, compute disproportionality only for pairs
#' with at least `min_n_obs` cases.
#' @keywords disproportionality
#' @returns A data.table with columns
#' \itemize{
#' \item `y`, `x` and `z`, same as input
#' \item `n_obs` the number of observed cases
#' \item `n_exp` the number of expected cases
#' \item `ic` the Information Component
#' \item `ic_tail` the tail probability of the IC
#' \item `ci_level` the confidence interval level
#' \item Additional columns, if `export_raw_values` is `TRUE`:
#' \item `a`, `b`, `c`, `d` the counts in the contingency table
#' \item `signif_ic` the significance of the Information Component
#' \item Additional columns, if `export_raw_values` is `TRUE`:
#' \item `n_*` the counts of each setting
#' \item `signif_ic` the significance of the Information Component
#' }
#' @export
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @importFrom data.table data.table .N
#' @seealso [compute_dispro()], [compute_or_mod()], [add_drug()], [add_adr()]
#' @examples
#' # Interaction on reporting of colitis with ipilimumab and nivolumab
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
#'   compute_interaction(
#'     y = "a_colitis",
#'     x = "nivolumab",
#'     z = "ipilimumab"
#'   )
#'

compute_interaction <-
  function(.data,
           y,
           x,
           z,
           alpha = .05,
           na_format = "-",
           dig = 2,
           export_raw_values = FALSE,
           min_n_obs = 0) {

    z_val <- qnorm(1 - alpha / 2)

    # data mask
    dm <-
      data.frame(
        one_x = c(1, 1, 1, 1, 0, 0, 0, 0),
        one_y = c(1, 1, 0, 0, 1, 1, 0, 0),
        one_z = c(1, 0, 1, 0, 1, 0, 1, 0)
      )

    # letters in the contingency table
    cases <-
      c("n_xyz",
        "n_xy", "n_xz",
        "n_x",
        "n_yz",
        "n_y",
        "n_z",
        "n_none")
    # look at dm to know this order

    var_to_export <-
      if(export_raw_values){
        c("y", "x", "z", "n_obs", "n_exp", "ic", "ic_tail", "ci_level",
          cases,
          "signif_ic")
      } else {
        c("y", "x", "z", "n_obs", "n_exp", "ic", "ic_tail", "ci_level")
      }

    # extract counts from source table
    c_interaction_core <-
      function(.data, one_y, one_x, one_z) {

        # rename variables
        var <- c(one_y, one_x, one_z) |>
          rlang::set_names(
            c("one_y", "one_x", "one_z")
          )

        # contingency table
        ct <-
          .data |>
          dplyr::rename(dplyr::all_of(.env$var)) |>
          dplyr::group_by(.data$one_y, .data$one_x, .data$one_z) |>
          dplyr::summarise(n_cas = dplyr::n(),
                           .groups = "keep") |>
          dplyr::collect()

        # data manage contingency table
        # starting with data mask
        dm |>
          dplyr::left_join(ct, by = c("one_y", "one_x", "one_z")) |>
          dplyr::mutate(
            n_cas = ifelse(is.na(.data$n_cas), 0, .data$n_cas)
          ) |>
          dplyr::mutate(y = .env$one_y, x = .env$one_x, z = .env$one_z) |>
          dplyr::relocate(dplyr::all_of(c("y", "x", "z"))) |>
          # have a, b, c, d counts sorted before adding letters
          dplyr::arrange(
            dplyr::desc(.data$one_x),
            dplyr::desc(.data$one_y),
            dplyr::desc(.data$one_z)
            ) |>
          dplyr::mutate(
            cases = .env$cases
          ) |>
          dplyr::select(dplyr::all_of(c("y", "x", "z", "cases", "n_cas"))) |>
          tidyr::pivot_wider(
            names_from  = dplyr::all_of("cases"),
            values_from = dplyr::all_of("n_cas")
          )
      }

    # all drugs and all adrs count tables
    counts_xyz <-
      z |>
      purrr::map(function(one_z_)
        x  |>
          purrr::map(function(one_x_) {
            y |>
              purrr::map(function(one_y_)
                .data |>
                  c_interaction_core(
                    one_y = one_y_,
                    one_x = one_x_,
                    one_z = one_z_
                  ))
          })) |>
      unlist(recursive = FALSE) |>
      unlist(recursive = FALSE) |>
      purrr::list_rbind() |>
      dplyr::mutate(
        n_total = rowSums(dplyr::across(dplyr::all_of(cases)))
      )

    # compute disproportionality
    counts_xyz |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(cases), ~ as.numeric(.x)),
        n_obs = .data$n_xyz,
        n_exp =
          (.data$n_xy * .data$n_xz * .data$n_yz * .data$n_total) /
            (.data$n_x * .data$n_y * .data$n_z),

        ic = log((.data$n_obs + .5) / (.data$n_exp + .5), base = 2),
        ic_tail = ic_tail(
          n_obs = .data$n_obs,
          n_exp = .data$n_exp,
          p = .env$alpha / 2
        ),
        ci_level  = paste0((1 - .env$alpha) * 100, "%"),
        signif_ic = ifelse(.data$ic_tail > 0, 1, 0),
        # don't show results for pairs with less than min_n_obs
        dplyr::across(
          dplyr::all_of(
            c("n_exp", "ic", "ic_tail",
              cases,
               "signif_ic")
          ),
          function(num_col)
            dplyr::if_else(.data$n_xyz < .env$min_n_obs,
                           NA_real_,
                           num_col)
        ),
        dplyr::across(
          dplyr::all_of(
            c("ci_level"
            )
          ),
          function(chr_col)
            dplyr::if_else(.data$n_xyz < .env$min_n_obs,
                           .env$na_format,
                           chr_col)
        )
      ) |>
      dplyr::select(dplyr::all_of(var_to_export))
  }
