# Dated investigation evidence: 2026-10-26, source bfcdaf76a36d102132cd039c68ba8bc05a4fd465.
args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args)) args[[1L]] else '.'
pkgload::load_all(repo, quiet = TRUE)
x <- tibble::tibble(cols = c('x', 'y'), key = c(1, 1))
numeric_columns <- function() names(tidyselect::peek_data())[vapply(tidyselect::peek_data(), is.numeric, logical(1))]
selection <- function(data, cols = numeric_columns()) dplyr::select(data, base::c(cols))
fixed <- function(data, cols = numeric_columns()) inspect_grouping(data, .by = base::c(cols), .format = 'list')
grouped <- function(data, cols = numeric_columns()) inspect_grouping(data, .grouping = grouping_set(base::c(cols)), .format = 'list')
nested <- function(data, cols = numeric_columns()) nest_with_margins(data, .by = base::c(cols))
cat('dplyr selection: '); print(names(selection(x)))
cat('fixed inspection: '); print(fixed(x)$fixed)
cat('dimension inspection: '); print(grouped(x)$included)
cat('nest fixed keys: '); print(nested(x))
lazy <- dtplyr::lazy_dt(x)
cat('dtplyr dimensions: '); print(grouped(lazy)$included)
expected <- 'key'
stopifnot(identical(names(selection(x)), expected))
stopifnot(identical(fixed(x)$fixed, list(expected)))
stopifnot(identical(grouped(x)$included, list(expected)))
