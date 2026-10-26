# Investigation evidence, 2026-10-26. Run from the repository root.
pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages({library(dplyr); library(arrow)})
assert_mode <- '--assert' %in% commandArgs(trailingOnly = TRUE)
x <- tibble(f = c('x', 'y'), g = c('b', 'a'), v = 1:2)
canon <- function(z) {
  z <- as.data.frame(z)
  z[] <- lapply(z, as.character)
  z <- z[do.call(order, c(z, list(na.last = TRUE))), , drop = FALSE]
  row.names(z) <- NULL
  z
}
failures <- list(); total <- 0L; controls <- 0L
for (shape in c('table', 'dataset')) for (empty in c(FALSE, TRUE)) for (first_verb in c('expand', 'summary')) for (first_sort in c('none', 'first', 'last')) for (first_id in list(NULL, 'first_set')) {
  local <- if (empty) head(x, 0L) else x
  input <- Table$create(local)
  if (shape == 'dataset') input <- InMemoryDataset$create(input)
  make_first <- function(data) {
    if (first_verb == 'expand') {
      expand_with_margins(data, .by = f, .grouping = rollup(g), .margin_label = NULL, .id = first_id, .sort = first_sort)
    } else {
      summarize_with_margins(data, v = sum(v), .by = f, .grouping = rollup(g), .margin_label = NULL, .id = first_id, .sort = first_sort)
    }
  }
  first <- make_first(input)
  expected_first <- make_first(local)
  stopifnot(identical(canon(collect(first)), canon(expected_first)))
  one <- collect(expand_with_margins(first, .grouping = grouping_set(f), .margin_label = NULL, .id = 'second_set'))
  stopifnot(nrow(one) == nrow(expected_first), all(one$second_set == 1L))
  controls <- controls + 2L
  for (second_sort in c('none', 'first', 'last')) {
    expected <- expand_with_margins(expected_first, .grouping = rollup(f), .margin_label = NULL, .id = 'second_set', .sort = second_sort)
    # collapse() makes a lazy query boundary around the valid first result.
    # Arrow's method wraps the query and records its schema without collecting.
    control <- collect(expand_with_margins(collapse(first), .grouping = rollup(f), .margin_label = NULL, .id = 'second_set', .sort = second_sort))
    stopifnot(identical(canon(control), canon(expected)))
    controls <- controls + 1L
    got <- tryCatch(collect(expand_with_margins(first, .grouping = rollup(f), .margin_label = NULL, .id = 'second_set', .sort = second_sort)), error = identity)
    total <- total + 1L
    if (inherits(got, 'error')) {
      failures[[length(failures) + 1L]] <- list(shape = shape, empty = empty, first_verb = first_verb, first_sort = first_sort, first_id = first_id, second_sort = second_sort, message = conditionMessage(got))
    } else {
      stopifnot(identical(canon(got), canon(expected)))
    }
  }
}
cat('re-expansion cases:', total, '; passing controls:', controls, '; failures:', length(failures), '\n')
print(table(vapply(failures, `[[`, character(1), 'first_verb'), vapply(failures, `[[`, character(1), 'first_sort')))
if (length(failures)) cat('first failure:', failures[[1L]]$message, '\n')

if (assert_mode) stopifnot(length(failures) == 0L)
