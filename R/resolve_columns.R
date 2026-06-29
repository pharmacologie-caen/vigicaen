# Resolve a column selection to character names
#
# Internal helper backing tidy-select support in [desc_facvar()] and
# [desc_cont()]. A plain character vector keeps the historical behaviour (it is
# returned untouched, then validated downstream by [check_columns_in_data()], so
# existing error classes are preserved). Any other expression (bare names,
# `where()`, `starts_with()`, integer positions, ...) is resolved through
# [tidyselect::eval_select()].
#
# The legacy-vs-tidyselect branch is decided by evaluating the captured
# expression on its own: if it yields a character vector, it is treated as a
# vector of column names (this also covers external vectors such as
# `vars <- c("a", "b"); desc_facvar(df, vars)`); otherwise tidy-select rules
# apply.
#
# @param .data A data.frame, data.table, or arrow object.
# @param var_quo A quosure capturing the user's column selection.
# @param call The execution environment used for error reporting.
# @returns A character vector of column names.
resolve_columns <- function(.data, var_quo, call = rlang::caller_env()) {

  selection <-
    tryCatch(
      rlang::eval_tidy(var_quo),
      error = function(cnd) NULL
    )

  # legacy path: a character vector is returned as-is, so downstream validation
  # (and its `columns_not_in_data` / `columns_not_numeric_integer` classes)
  # is left untouched.
  if (is.character(selection)) {
    return(selection)
  }

  # tidy-select path: build a zero-row, type-preserving prototype so predicate
  # helpers such as `where(is.numeric)` evaluate on the correct column types.
  proto <-
    if (inherits(.data, c("Table", "Dataset", "arrow_dplyr_query"))) {
      dplyr::collect(utils::head(.data, 0L))
    } else {
      utils::head(.data, 0L)
    }

  names(
    tidyselect::eval_select(var_quo, data = proto, error_call = call)
  )
}
