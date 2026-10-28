# Dated investigation evidence: 2026-10-28, source ac6ad79ff6026abe5fc45538d6dfefadf03a8ca3.
pkgload::load_all('.', quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
input <- tibble(.SD = c('a', 'b'), value = 1:2)
original <- input
root <- dtplyr::lazy_dt(input)
filtered <- filter(root, TRUE)
stopifnot(identical(collect(filtered), input))
stopifnot(identical(
  inspect_grouping(root, .by = where(is.character)),
  inspect_grouping(input, .by = where(is.character))
))
stopifnot(identical(
  inspect_grouping(filtered, .by = all_of('.SD')),
  inspect_grouping(input, .by = all_of('.SD'))
))
cat('PASS source collection, root typed inspection, filtered name-only inspection\n')

failures <- 0L
checks <- 0L
verify <- function(label, actual, expected) {
  checks <<- checks + 1L
  value <- tryCatch(force(actual), error = identity)
  if (inherits(value, 'error')) {
    failures <<- failures + 1L
    cat('FAIL', label, ':', conditionMessage(value), '\n')
  } else {
    stopifnot(isTRUE(all.equal(value, expected)))
    cat('PASS', label, '\n')
  }
}
for (empty in c(FALSE, TRUE)) {
  x <- if (empty) input[0, ] else input
  lazy <- dtplyr::lazy_dt(x)
  steps <- list(filtered = filter(lazy, TRUE))
  for (name in names(steps)) {
    source <- steps[[name]]
    reference <- collect(source)
    for (fixed in c(FALSE, TRUE)) {
      verify(paste(name, 'typed inspection', empty, fixed),
        if (fixed) inspect_grouping(source, .by = where(is.character)) else
          inspect_grouping(source, .grouping = grouping_set(where(is.character))),
        if (fixed) inspect_grouping(reference, .by = where(is.character)) else
          inspect_grouping(reference, .grouping = grouping_set(where(is.character))))
    }
    for (verb in list(nest_with_margins, nest_by_with_margins)) {
      for (keep in c(FALSE, TRUE)) {
        actual <- tryCatch(collect(verb(source, .keep = keep)), error = identity)
        checks <- checks + 1L
        if (inherits(actual, 'error')) {
          failures <- failures + 1L
          cat('FAIL', name, 'nest', empty, keep, ':', conditionMessage(actual), '\n')
        } else {
          expected <- verb(reference, .keep = keep)
          stopifnot(nrow(actual) == nrow(expected), identical(names(actual), names(expected)))
          stopifnot(isTRUE(all.equal(lapply(actual$data, as.list), lapply(expected$data, as.list))))
        }
      }
    }
  }
}
# An observed predicate count proves that typed inspection must not run it.
source_table <- data.table::as.data.table(list(safe = c('a', 'b'), value = 1:2))
data.table::setnames(source_table, 'safe', '.SD')
source_copy <- data.table::copy(source_table)
reads <- 0L
tick <- function(x) { reads <<- reads + length(x); rep(TRUE, length(x)) }
stopifnot(identical(tick(1:2), c(TRUE, TRUE)), reads == 2L)
reads <- 0L
mutable <- filter(dtplyr::lazy_dt(source_table, immutable = FALSE), tick(value))
verify('mutable typed inspection', inspect_grouping(mutable, .by = where(is.character)),
  inspect_grouping(input, .by = where(is.character)))
stopifnot(reads == 0L, identical(source_table, source_copy), identical(input, original))
cat('checks', checks, 'failures', failures, '; zero predicate rows read; source unchanged\n')
quit(status = as.integer(failures > 0L))
