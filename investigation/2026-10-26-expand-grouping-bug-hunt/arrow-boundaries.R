# Investigation evidence, 2026-10-26. Run from the repository root.
pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages({library(dplyr); library(arrow)})
probes <- list(
  dictionary = function() {
    input <- Table$create(g = call_function('dictionary_encode', Array$create(1:2)), payload = 1:2)
    stopifnot(nrow(suppressWarnings(collect(input))) == 2L)
    by_name <- suppressWarnings(inspect_grouping(input, .grouping = rollup(g)))
    by_type <- suppressWarnings(inspect_grouping(input, .grouping = rollup(where(is.factor))))
    stopifnot(identical(by_name, by_type))
    missing <- Scalar$create(call_function('dictionary_encode', Array$create(NA_integer_)))
    control <- suppressWarnings(collect(union_all(input, mutate(input, g = !!missing))))
    stopifnot(nrow(control) == 4L, sum(is.na(control$g)) == 2L)
    # Desired contract: a one-set plan copies every row and omits no dimension.
    result <- suppressWarnings(collect(expand_with_margins(input, .grouping = grouping_set(g), .margin_label = NULL, .id = 'set')))
    stopifnot(nrow(result) == 2L, all(result$set == 1L))
  },
  sorted_reinput = function() {
    input <- Table$create(f = c('x', 'y'), g = c('b', 'a'), payload = 1:2)
    first <- expand_with_margins(input, .grouping = rollup(g), .margin_label = NULL, .sort = 'last')
    stopifnot(nrow(collect(first)) == 4L)
    control <- collect(expand_with_margins(collapse(first), .grouping = rollup(f), .margin_label = NULL, .id = 'set'))
    stopifnot(nrow(control) == 8L, identical(as.integer(table(control$set)), c(4L, 4L)))
    # Desired contract: a lazy Margin result remains valid input for expansion.
    result <- collect(expand_with_margins(first, .grouping = rollup(f), .margin_label = NULL, .id = 'set'))
    stopifnot(nrow(result) == 8L)
  }
)
selected <- commandArgs(trailingOnly = TRUE)
if (length(selected) == 0L) selected <- names(probes)
failed <- FALSE
for (name in selected) {
  result <- tryCatch({probes[[name]](); TRUE}, error = identity)
  if (inherits(result, 'error')) {
    failed <- TRUE
    cat('FAIL:', name, '\n', conditionMessage(result), '\n', sep = '')
  } else cat('PASS: ', name, '\n', sep = '')
}
if (failed) quit(status = 1L)
