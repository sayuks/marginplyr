assert_logical_scalar <- function(x) {
  nm <- deparse(substitute(x))
  if (!(isTRUE(x) || isFALSE(x))) {
    abort_marginplyr(
      sprintf("`%s` must be a logical scalar (`TRUE` or `FALSE`).", nm)
    )
  }
}

# NA_character_ is allowed
assert_string_scalar <- function(x) {
  nm <- deparse(substitute(x))
  if (!(is.character(x) && length(x) == 1)) {
    abort_marginplyr(
      sprintf("`%s` must be a character vector of length 1.", nm)
    )
  }
}

assert_nest_possible <- function(x) {
  nm <- deparse(substitute(x))
  valid_classes <- c("data.frame", "dtplyr_step")
  if (!inherits(x, valid_classes)) {
    abort_marginplyr(
      sprintf(
        "`%s` must be one of the following classes, which can be nested: %s",
        nm,
        toString(valid_classes)
      )
    )
  }
}

assert_lazy_table <- function(x) {
  nm <- deparse(substitute(x))
  invalid_lazy_table_names <- "RecordBatchReader"
  if (inherits(x, invalid_lazy_table_names)) {
    abort_marginplyr(
      sprintf(
        "`%s` must not be an object of the following classes: %s",
        nm,
        toString(invalid_lazy_table_names)
      )
    )
  }
}

# Required when dtplyr/data.table are optional Suggests rather than Imports.
# data.table fixes the spelling of this name, so it cannot follow the package's
# object naming convention.
.datatable.aware <- TRUE # nolint: object_name_linter

utils::globalVariables(".data")
