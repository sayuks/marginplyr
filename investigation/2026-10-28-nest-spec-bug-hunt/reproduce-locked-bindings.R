# Dated investigation evidence: 2026-10-28, source ac6ad79ff6026abe5fc45538d6dfefadf03a8ca3.
pkgload::load_all('.', quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
minimal <- tibble(.SDall = 1L)
stopifnot(identical(collect(dtplyr::lazy_dt(minimal)), minimal))
renamed <- rename(dtplyr::lazy_dt(minimal), safe = .SDall)
control <- collect(nest_with_margins(renamed))
stopifnot(identical(control$data[[1L]]$safe, 1L))
cat('PASS root collection and safe-rename nesting controls\n')

canonical <- function(result, key) {
  result <- ungroup(result)
  list(
    names = names(result),
    outer = as.list(result[setdiff(names(result), key)]),
    cells = lapply(result[[key]], function(cell) list(n = nrow(cell), values = as.list(cell)))
  )
}
checks <- 0L
failures <- 0L
for (name in c('.SDall', '.xSD', '.iSD')) {
  for (n in c(0L, 1L, 3L)) for (role in c('payload', 'fixed', 'dimension', 'id', 'key')) {
    for (keep in c(FALSE, TRUE)) for (verb_name in c('nest_with_margins', 'nest_by_with_margins')) {
      input <- tibble(g = rep('a', n), value = seq_len(n))
      if (role == 'payload') names(input)[2L] <- name
      if (role %in% c('fixed', 'dimension')) names(input)[1L] <- name
      original <- input
      verb <- get(verb_name)
      key <- if (role == 'key') name else 'data'
      run <- function(source) collect(verb(source,
        .by = if (role == 'fixed') all_of(name),
        .grouping = if (role == 'dimension') rollup(all_of(name)),
        .id = if (role == 'id') name else NULL,
        .key = key, .keep = keep))
      expected <- run(input)
      source <- dtplyr::lazy_dt(input)
      actual <- tryCatch(run(source), error = identity)
      checks <- checks + 1L
      if (inherits(actual, 'error')) {
        failures <- failures + 1L
        cat('FAIL', name, n, role, keep, verb_name, ':', conditionMessage(actual), '\n')
      } else {
        stopifnot(isTRUE(all.equal(canonical(actual, key), canonical(expected, key))))
      }
      stopifnot(identical(input, original), identical(collect(source), original))
    }
  }
}
cat('checks', checks, 'failures', failures, '; all successful outputs matched local controls\n')
quit(status = as.integer(failures > 0L))
