assert_logical_scalar <- function(x) {
  nm <- deparse(substitute(x))
  if (!(isTRUE(x) || isFALSE(x))) {
    stop(
      sprintf("`%s` must be a logical scalar (`TRUE` or `FALSE`).", nm)
    )
  }
}

# NA_character_ is allowed
assert_string_scalar <- function(x) {
  nm <- deparse(substitute(x))
  if (!(is.character(x) && length(x) == 1)) {
    stop(
      sprintf("`%s` must be a character vector of length 1.", nm)
    )
  }
}

assert_nest_possible <- function(x) {
  nm <- deparse(substitute(x))
  valid_classes <- c("data.frame", "dtplyr_step", NULL)
  if (!inherits(x, valid_classes)) {
    stop(
      sprintf(
        "`%s` must be one of the following classes, whitch can be nested: %s",
        nm,
        toString(valid_classes)
      )
    )
  }
}

assert_lazy_table <- function(x) {
  nm <- deparse(substitute(x))
  invalid_lazy_table_names <- c("RecordBatchReader", NULL)
  if (inherits(x, invalid_lazy_table_names)) {
    stop(
      sprintf(
        "`%s` must not be an object of the following classes: %s",
        nm,
        toString(invalid_lazy_table_names)
      )
    )
  }
}
