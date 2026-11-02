# Dated independent probe; no implementation or permanent regression test.
pkgload::load_all('.', quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
observe <- function(expr) tryCatch(force(expr), error = identity)
run <- function(helper) {
  e <- new.env(parent = globalenv())
  e$input <- tibble(g = c(2L, 1L, 2L), row = 1:3)
  evalq(q <- dtplyr::lazy_dt(input), e)
  e$as.character <- helper
  evalq({
    list(
      source = collect(q),
      native = collect(count(q, g)),
      nest = collect(nest_with_margins(q, .grouping = rollup(g), .id = 'set')),
      rowwise = nest_by_with_margins(q, .grouping = rollup(g), .id = 'set'),
      expand = collect(expand_with_margins(q, .grouping = rollup(g), .id = 'set')),
      summary = collect(summarize_with_margins(q, n = n(), .grouping = rollup(g), .id = 'set'))
    )
  }, e)
}
base <- run(base::as.character)
masked <- run(function(x, ...) rep('CORRUPTED', length(x)))
for (name in names(base)) {
  cat(name, ': ')
  if ('g' %in% names(base[[name]])) {
    cat(paste(base[[name]]$g, collapse = ', '), '=>', paste(masked[[name]]$g, collapse = ', '), '\n')
  }
}
stopifnot(identical(base$source, masked$source), identical(base$native, masked$native))
for (name in c('nest', 'rowwise')) {
  stopifnot(identical(lapply(base[[name]]$data, as.list), lapply(masked[[name]]$data, as.list)))
}
if ('--assert' %in% commandArgs(TRUE)) {
  stopifnot(identical(base$nest$g, masked$nest$g))
}
