# Run from the inspected checkout. Observation mode exits zero; --assert is red
# until helper arguments respect the S3 methods dispatched for implicit classes.
args <- commandArgs(TRUE)
root <- if (length(args) > 0L && !startsWith(args[[1L]], '--')) args[[1L]] else '.'
script_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
evidence_dir <- if (length(script_arg)) dirname(normalizePath(sub('^--file=', '', script_arg[[1L]]))) else getwd()
pkgload::load_all(root, quiet = TRUE)
input <- tibble::tibble(region = c('east', 'west'), amount = c(1L, 1L))
selector <- matrix(1L)
format.matrix <- function(x, ...) {
  names(tidyselect::peek_data())[
    vapply(tidyselect::peek_data(), is.character, logical(1))
  ]
}
expected <- names(tidyselect::eval_select(quote(all_of(format(selector))), input))
fixed <- inspect_grouping(input, .by = all_of(format(selector)), .format = 'list')$fixed[[1L]]
rollup_result <- tryCatch(
  inspect_grouping(input, .grouping = rollup(all_of(format(selector))), .format = 'list'),
  error = identity
)
ordinary <- dplyr::summarise(input, n = dplyr::n(), .by = all_of(format(selector)))
summary <- summarize_with_margins(input, n = dplyr::n(), .by = all_of(format(selector)))
nested <- nest_with_margins(input, .by = all_of(format(selector)))
rowwise <- nest_by_with_margins(input, .by = all_of(format(selector)))
opaque <- function() do.call(format, list(selector))
control <- inspect_grouping(input, .by = all_of(opaque()), .format = 'list')$fixed[[1L]]
classed_selector <- structure(selector, class = c('matrix', 'array'))
explicit_control <- inspect_grouping(input, .by = all_of(format(classed_selector)), .format = 'list')$fixed[[1L]]
cat('implicit matrix is.object: ', is.object(selector), '\n', sep = '')
cat('ordinary tidyselect: ', paste(expected, collapse = ','), '\n', sep = '')
cat('inspected fixed keys: ', paste(fixed, collapse = ','), '\n', sep = '')
cat('rollup outcome: ', if (inherits(rollup_result, 'error')) conditionMessage(rollup_result) else 'success', '\n', sep = '')
cat('ordinary summary rows: ', nrow(ordinary), '; Margin summary rows: ', nrow(summary), '\n', sep = '')
cat('nest rows: ', nrow(nested), '; nest_by rows: ', nrow(rowwise), '\n', sep = '')
cat('explicit-class control: ', paste(explicit_control, collapse = ','), '\n', sep = '')
cat('opaque-wrapper control: ', paste(control, collapse = ','), '\n', sep = '')
stopifnot(identical(expected, 'region'), identical(control, expected), identical(explicit_control, expected), nrow(ordinary) == 2L)
if ('--assert' %in% args) {
  stopifnot(identical(fixed, expected), !inherits(rollup_result, 'error'), nrow(nested) == 2L, nrow(rowwise) == 2L, identical(summary, ordinary))
}
