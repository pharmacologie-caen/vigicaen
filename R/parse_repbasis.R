# Parse and validate a `repbasis` string into drug `Basis` codes.
#
# `repbasis` selects which report bases to keep for a drug: "s" suspect (1),
# "c" concomitant (2), "i" interacting (3). Any combination is allowed (e.g.
# "sci", "s", "sc"). Previously this 5-line `grepl()` block was duplicated in
# `add_drug()`, `add_dose()` and `vigi_routine()` with no validation, so an
# empty or invalid `repbasis` (e.g. "" or "xyz") silently produced an empty
# `basis_sel` and filtered the drug table to zero rows.
#
# @param repbasis A length-1 character string combining "s"/"c"/"i".
# @param call The calling environment, for error reporting.
# @returns An integer vector of `Basis` codes (subset of `c(1, 2, 3)`).
# @noRd
parse_repbasis <- function(repbasis, call = rlang::caller_env()) {
  if (!rlang::is_string(repbasis) || !grepl("^[sci]+$", repbasis)) {
    cli::cli_abort(
      c("{.arg repbasis} must be a single string combining {.val s}, {.val c} \\
         and {.val i}.",
        "x" = "You supplied {.val {repbasis}}."),
      class = "invalid_repbasis",
      call = call
    )
  }

  c(
    if (grepl("s", repbasis)) 1,
    if (grepl("c", repbasis)) 2,
    if (grepl("i", repbasis)) 3
  )
}
