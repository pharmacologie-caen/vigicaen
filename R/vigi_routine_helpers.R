# Internal helpers for vigi_routine()
#
# These extract the two memory-saving "bypasses" that let vigi_routine() run on
# low-spec machines (see #158) into small, testable units. Rather than building
# drug/adr columns on the full demo table (which add_drug()/add_adr() would
# materialise in memory), vigi_routine() works from case-id sets and the 2x2
# counts directly. Keeping these as standalone helpers makes that logic
# unit-testable outside of the (graphics-producing) vigi_routine().

# Identify the case sets for a single drug-adr pair.
#
# Returns the unique UMCReportId vectors for the drug exposure, the adr, and
# their intersection (exposed-and-reacting cases). With `d_code_2`, the drug set
# is the intersection of the two drugs' exposed cases (dual-drug analysis).
#
# @param drug_data,adr_data The drug and adr tables.
# @param d_code,a_code Named length-one lists with the drug / adr codes.
# @param basis_sel Integer vector of drug `Basis` values to keep.
# @param d_code_2 Optional second drug code list (dual-drug analysis).
# @returns A list with `umc_drug`, `umc_adr`, `umc_cases`.
# (`.data` / `.env` pronouns are imported package-wide.)
# @noRd
vr_case_sets <- function(drug_data, adr_data, d_code, a_code, basis_sel,
                         d_code_2 = NULL) {

  umc_for_drecno <- function(drecno) {
    drug_data |>
      dplyr::filter(.data$DrecNo %in% drecno &
                      .data$Basis %in% basis_sel) |>
      dplyr::pull(.data$UMCReportId) |>
      unique()
  }

  umc_adr <-
    adr_data |>
    dplyr::filter(.data$MedDRA_Id %in% a_code[[1]]) |>
    dplyr::pull(.data$UMCReportId) |>
    unique()

  umc_drug <-
    if (!is.null(d_code_2)) {
      intersect(umc_for_drecno(d_code[[1]]), umc_for_drecno(d_code_2[[1]]))
    } else {
      umc_for_drecno(d_code[[1]])
    }

  umc_cases <- intersect(umc_drug, umc_adr)

  list(umc_drug = umc_drug, umc_adr = umc_adr, umc_cases = umc_cases)
}

# Information Component for a single 2x2 contingency table.
#
# Reproduces compute_dispro()'s IC computation directly from the a/b/c/d counts,
# so vigi_routine() does not need to add drug/adr columns to the full demo
# table. Kept identical to compute_dispro() so the IC matches.
#
# @param a,b,c,d Contingency-table counts.
# @param alpha Two-sided alpha for the IC credibility interval.
# @param min_n_obs Pairs with fewer than `min_n_obs` observed cases are blanked.
# @param na_format Replacement for `ci_level` when blanked.
# @returns A one-row data.frame with columns a, b, c, d, n_obs, n_exp, ic,
#   ic_tail, ci_level, signif_ic.
# @noRd
vr_ic_from_counts <- function(a, b, c, d,
                              alpha = 0.05,
                              min_n_obs = 0,
                              na_format = "-") {
  data.frame(a = a, b = b, c = c, d = d) |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(c("a", "b", "c", "d")), ~ as.numeric(.x)),
      n_obs = .data$a,
      n_exp = (.data$a + .data$b) * # n drug
        (.data$a + .data$c) / # n event
        (.data$a + .data$b + .data$c + .data$d), # n pop
      ic = log((.data$a + .5) / (.data$n_exp + .5), base = 2),
      ic_tail = ic_tail(
        n_obs = .data$a,
        n_exp = .data$n_exp,
        p = .env$alpha / 2
      ),
      ci_level  = paste0((1 - .env$alpha) * 100, "%"),
      signif_ic = ifelse(.data$ic_tail > 0, 1, 0),
      # don't show results for pairs with less than min_n_obs
      dplyr::across(
        dplyr::all_of(
          c("n_exp", "ic", "ic_tail",
            "a", "b", "c", "d",
            "signif_ic")
        ),
        function(num_col)
          dplyr::if_else(.data$a < .env$min_n_obs,
                         NA_real_,
                         num_col)
      ),
      dplyr::across(
        dplyr::all_of(
          c("ci_level"
          )
        ),
        function(chr_col)
          dplyr::if_else(.data$a < .env$min_n_obs,
                         .env$na_format,
                         chr_col)
      )
    )
}
