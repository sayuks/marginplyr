# Controlled first-render condition at public SQLite label-check boundary.
# It exercises diagnostic condition classification without editing package code.
args <- commandArgs(trailingOnly = TRUE)
audited <- identical(args[[1L]], 'on')
kind <- if (length(args) >= 2L) args[[2L]] else 'warning_interrupt'
pkgload::load_all('.', quiet = TRUE)
con <- DBI::dbConnect(RSQLite::SQLite(), ':memory:')
remote <- dplyr::copy_to(con, data.frame(g = 'a', v = 1), 'compound_render_source')
options(marginplyr.audit_sql = audited)
classes <- switch(kind,
  warning_interrupt = c('warning', 'interrupt', 'condition'),
  error_interrupt = c('interrupt', 'error', 'condition'),
  warning_error = c('warning', 'error', 'condition'),
  all = c('warning', 'interrupt', 'error', 'condition'))
leaf <- structure(list(message = 'compound renderer cancellation', call = NULL), class = classes)
original <- dbplyr::sql_render
calls <- 0L
testthat::local_mocked_bindings(sql_render = function(...) {
  calls <<- calls + 1L
  if (calls == 1L) stop(leaf)
  original(...)
}, .package = 'dbplyr', .env = globalenv())
caught <- NULL
result <- tryCatch({
  q <- summarize_with_margins(remote, total = sum(v, na.rm = TRUE),
    .grouping = rollup(g), .check_margin_label = TRUE)
  'returned'
}, interrupt = function(cnd) {
  caught <<- cnd
  'interrupted'
}, error = function(cnd) 'errored')
if (identical(result, 'interrupted')) stopifnot(identical(caught, leaf))
cat('AUDIT', args[[1L]], 'KIND', kind, 'RESULT', result, 'SQL_RENDER_CALLS', calls, '\n')
cat('ORIGINAL_INTERRUPT', identical(caught, leaf), '\n')
if (audited) {
  rows <- last_sent_queries(details = TRUE)
  print(rows[c('purpose', 'status')])
  print(rows$conditions[[1L]])
}
DBI::dbDisconnect(con)
if (kind %in% c('warning_interrupt', 'error_interrupt', 'all') && result != 'interrupted') quit(status = 1L)
