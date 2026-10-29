# --assert checks the intended behavior and exits one on the investigated SHA.
args <- commandArgs(TRUE)
root <- if (length(args) && args[[1L]] != '--assert') args[[1L]] else '.'
pkgload::load_all(root, quiet = TRUE)
empty <- tibble::tibble(value = integer())
identifier <- matrix('set', 1L, 1L)
control <- nest_by_with_margins(empty, .id = 'set')
nonempty <- nest_by_with_margins(tibble::tibble(value = 1L), .id = identifier)
actual <- tryCatch(nest_by_with_margins(empty, .id = identifier), error = identity)
stopifnot(rlang::is_string(identifier), nrow(control) == 1L,
          identical(control$set, 1L), nrow(control$data[[1L]]) == 0L,
          nrow(nonempty) == 1L, identical(nonempty$set, 1L))
cat('ordinary ID: one row, ID 1L, zero-row cell\n')
cat('same dimensional ID with nonempty input: one row, ID 1L\n')
if (inherits(actual, 'error')) cat(conditionMessage(actual), '\n')
if ('--assert' %in% args) {
  stopifnot(!inherits(actual, 'error'))
  stopifnot(identical(actual, control))
}
