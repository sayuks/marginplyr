# Dated 2026-10-31; a fresh root environment per completed comparison.
pkgload::load_all('.', quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
normalize <- function(x) {
  if (inherits(x, 'dtplyr_step')) x <- collect(x)
  x <- ungroup(x)
  values <- lapply(seq_len(nrow(x)), function(i) {
    cell <- as_tibble(x$data[[i]], .name_repair = 'minimal')
    list(keys = as.list(x[i, setdiff(names(x), 'data'), drop = FALSE]),
         cell = as.list(cell), n = nrow(cell))
  })
  sort(vapply(values, function(x) paste(as.character(serialize(x, NULL)), collapse = ''), character(1)))
}
observe <- function(expr) {
  warnings <- character()
  value <- tryCatch(withCallingHandlers(force(expr), warning = function(cnd) {
    warnings <<- c(warnings, conditionMessage(cnd))
    invokeRestart('muffleWarning')
  }), error = identity)
  list(value = value, warnings = warnings)
}
records <- list()
for (n in c(0L, 1L, 4L)) for (kind in c('character', 'integer', 'factor')) {
  g <- switch(kind,
    character = rep(c('b', 'a', 'b', NA_character_), length.out = n),
    integer = rep(c(2L, 1L, 2L, NA_integer_), length.out = n),
    factor = factor(rep(c('b', 'a', 'b', NA_character_), length.out = n), levels = c('c', 'a', 'b')))
  source <- data.table::data.table(g = g, row_id = seq_len(n))
  before <- serialize(source, NULL)
  for (spec_name in c('single', 'rollup')) for (label in c('text', 'missing')) {
    spec <- if (spec_name == 'single') grouping_set(g) else rollup(g)
    margin_label <- if (label == 'text') 'Total' else NULL
    for (verb in c('nest', 'nest_by')) for (keep in c(FALSE, TRUE)) for (order in c('none', 'last', 'first')) {
      fun <- if (verb == 'nest') nest_with_margins else nest_by_with_margins
      run <- function(x) fun(x, .grouping = spec, .keep = keep,
                             .margin_label = margin_label, .id = 'set', .sort = order)
      expected <- normalize(run(as_tibble(source)))
      for (helper in c('min', 'seq_len')) {
        env <- new.env(parent = globalenv())
        env$source <- source
        input <- evalq(dtplyr::lazy_dt(source), env)
        baseline <- observe(normalize(run(input)))
        stopifnot(!inherits(baseline$value, 'error'), length(baseline$warnings) == 0L,
                  identical(baseline$value, expected))
        reads <- 0L
        env[[helper]] <- if (helper == 'min') function(...) {reads <<- reads + 1L; 0L} else function(length.out) {reads <<- reads + 1L; integer()}
        result <- observe(normalize(run(input)))
        status <- if (inherits(result$value, 'error')) 'error' else if (identical(result$value, expected)) 'match' else 'wrong_keys'
        stopifnot(length(result$warnings) == 0L, identical(serialize(source, NULL), before))
        if (n > 0L) stopifnot(status == 'wrong_keys', reads > 0L) else stopifnot(status == 'match')
        records[[length(records) + 1L]] <- data.frame(n, kind, spec = spec_name, label,
          verb, keep, order, helper, status, reads)
      }
    }
  }
}
results <- bind_rows(records)
write.csv(results, 'investigation/2026-10-31-nest-spec-greedy/backend/display-helper-results.csv', row.names = FALSE)
print(as.data.frame(table(results$helper, results$status)), row.names = FALSE)
cat('Completed comparisons: ', nrow(results), '\n', sep = '')
cat('Every unmasked baseline matched explicit collected-input local nesting.\n')
cat('All source snapshots unchanged; no warning or unexpected error.\n')
