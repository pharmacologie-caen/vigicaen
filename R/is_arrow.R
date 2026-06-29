# Is `x` an out-of-memory arrow object?
#
# Internal predicate unifying the arrow-class checks used across the package,
# which were previously written inline in two inconsistent spellings (some sites
# omitted `"arrow_dplyr_query"`). Returns TRUE for arrow `Table`, `Dataset`, and
# `arrow_dplyr_query` objects (the latter being the result of chained dplyr verbs
# on arrow data before `collect()`/`compute()`).
#
# @param x An object.
# @returns A logical scalar.
# @noRd
is_arrow <- function(x) {
  inherits(x, c("Table", "Dataset", "arrow_dplyr_query"))
}
