pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
args <- commandArgs(trailingOnly = TRUE)
root <- 'investigation/2026-10-29-summary-spec-share-exhaustive/summary'
run <- function(expr) {
  warnings <- list()
  value <- tryCatch(withCallingHandlers(force(expr), warning = function(w) {
    warnings[[length(warnings) + 1L]] <<- list(class = class(w), message = conditionMessage(w))
    invokeRestart('muffleWarning')
  }), error = identity)
  list(value = value, warnings = warnings)
}
compare <- function(control, actual) {
  err1 <- inherits(control$value, 'error'); err2 <- inherits(actual$value, 'error')
  if (err1 && err2) return('both-refused')
  if (err1 != err2) return(if (err1) 'margin-only-success' else 'margin-only-error')
  if (isTRUE(all.equal(control$value, actual$value, check.attributes = TRUE))) 'equal' else 'value-difference'
}
shapes <- list(integer = 1L, double = 1, logical = TRUE, unspecified = NA,
  character = 'a', complex = 1i, raw = as.raw(1), list = list(1L),
  null = NULL, empty_int = integer(), empty_list = list(),
  int2 = 1:2, list2 = list(1L, 2L),
  matrix = matrix(1L, 1L, 1L), matrix_row = matrix(1:2, 1L, 2L),
  matrix2 = matrix(1:4, 2L, 2L), array = array(1:4, c(1L, 2L, 2L)),
  frame = data.frame(z = 1L), tibble = tibble(z = 1L),
  frame_empty = data.frame(z = integer()), frame_nocol = data.frame(row.names = 1L),
  frame2 = data.frame(z = 1:2), frame_disjoint = data.frame(w = 1L),
  packed = tibble(z = tibble(v = 1L)), listcol = tibble(z = list(1L)),
  date = as.Date('2026-10-29'), posix = as.POSIXct('2026-10-29', tz = 'UTC'),
  difftime = as.difftime(1, units = 'secs'), factor = factor('a'),
  ordered = ordered('a'), list_of = vctrs::list_of(1L),
  expression = expression(x), symbol = quote(x), call = quote(sum(x)))
rows <- list()
for (i in seq_along(shapes)) for (j in seq_along(shapes)) for (named in c(FALSE, TRUE)) {
  data <- data.frame(g = 1:2, x = 1:2)
  value <- function(x) if (x[[1L]] == 1L) shapes[[i]] else shapes[[j]]
  control <- if (named) run(summarise(data, out = value(x), .by = g)) else run(summarise(data, value(x), .by = g))
  actual <- if (named) run(summarize_with_margins(data, out = value(x), .by = g)) else run(summarize_with_margins(data, value(x), .by = g))
  result <- compare(control, actual)
  rows[[length(rows) + 1L]] <- data.frame(first = names(shapes)[i], second = names(shapes)[j], named = named, result = result)
  if (!(result %in% c('equal', 'both-refused'))) {
    cat('DIFF', names(shapes)[i], names(shapes)[j], named, result, '\n')
    print(control$value); print(actual$value)
  }
}
results <- bind_rows(rows)
write.csv(results, file.path(root, 'shape-results.csv'), row.names = FALSE)
print(table(results$result)); cat('Shape comparisons:', nrow(results), '\n')
