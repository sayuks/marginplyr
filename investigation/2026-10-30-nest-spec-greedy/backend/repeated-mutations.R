# Dated random sequential-mutation schema checks, including repeated names.
pkgload::load_all('.', quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
set.seed(30102917)
observe <- function(expr) {
  warnings <- character()
  value <- tryCatch(withCallingHandlers(force(expr), warning = function(cnd) {
    warnings <<- c(warnings, conditionMessage(cnd))
    invokeRestart('muffleWarning')
  }), error = identity)
  list(value = value, warnings = warnings)
}
counts <- c(cases = 0L, upstream_errors = 0L, typed_refusals = 0L,
            typed_matches = 0L, typed_mismatches = 0L, metadata_warnings = 0L,
            additional_metadata_warnings = 0L,
            source_mutations = 0L)
constants <- list(1L, 1, TRUE, 'x', NA_integer_, NA_real_, Inf, NaN)
for (case in seq_len(600L)) {
  size <- sample(c(0L, 1L, 5L), 1L)
  source <- data.table::data.table(g = rep(c('a', 'b'), length.out = size), v = seq_len(size), p = as.double(seq_len(size)))
  snapshot <- serialize(source, NULL)
  root <- dtplyr::lazy_dt(source)
  available <- c('g', 'v', 'p')
  updates <- list()
  for (index in seq_len(sample(2:8, 1L))) {
    destination <- sample(c('v', 'p', 'z', 'w', 'k'), 1L)
    mode <- sample(c('constant', 'copy', 'arithmetic', 'remove'), 1L, prob = c(0.2, 0.3, 0.4, 0.1))
    expression <- switch(mode,
      constant = constants[[sample(seq_along(constants), 1L)]],
      copy = as.name(sample(available, 1L)),
      arithmetic = rlang::call2(sample(c('+', '-', '*', '/'), 1L), as.name(sample(available, 1L)), constants[[sample(seq_len(2L), 1L)]]),
      remove = NULL)
    updates <- c(updates, stats::setNames(list(expression), destination))
    available <- if (is.null(expression)) setdiff(available, destination) else union(available, destination)
  }
  q <- observe(mutate(root, !!!updates))
  if (inherits(q$value, 'error')) next
  counts[['cases']] <- counts[['cases']] + 1L
  actual <- observe(collect(q$value))
  if (inherits(actual$value, 'error')) {
    counts[['upstream_errors']] <- counts[['upstream_errors']] + 1L
    next
  }
  inspected <- observe(inspect_grouping(q$value, .by = where(is.character), .format = 'list'))
  counts[['metadata_warnings']] <- counts[['metadata_warnings']] + length(inspected$warnings)
  counts[['additional_metadata_warnings']] <- counts[['additional_metadata_warnings']] + sum(!inspected$warnings %in% actual$warnings)
  if (length(inspected$warnings)) {cat('WARNING CASE', case, 'SIZE', size, '\n'); print(updates); cat('SOURCE WARNINGS', paste(actual$warnings, collapse = ' | '), '\nMETADATA WARNINGS', paste(inspected$warnings, collapse = ' | '), '\n'); print(inspected$value)}
  if (inherits(inspected$value, 'marginplyr_error')) {
    counts[['typed_refusals']] <- counts[['typed_refusals']] + 1L
  } else {
    expected <- names(actual$value)[vapply(actual$value, is.character, logical(1))]
    if (!inherits(inspected$value, 'error') && identical(inspected$value$fixed[[1L]], expected)) {
      counts[['typed_matches']] <- counts[['typed_matches']] + 1L
    } else {
      counts[['typed_mismatches']] <- counts[['typed_mismatches']] + 1L
      cat('CASE', case, '\n'); print(updates); print(inspected$value); print(expected)
    }
  }
  if (!identical(serialize(source, NULL), snapshot)) counts[['source_mutations']] <- counts[['source_mutations']] + 1L
}
print(counts)
stopifnot(counts[['typed_mismatches']] == 0L, counts[['additional_metadata_warnings']] == 0L, counts[['source_mutations']] == 0L)
