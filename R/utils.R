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

assert_data_frame <- function(x) {
  nm <- deparse(substitute(x))
  if (!is.data.frame(x)) {
    stop(
      sprintf("`%s` must be a data frame (not lazy table)", nm)
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
