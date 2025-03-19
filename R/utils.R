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
