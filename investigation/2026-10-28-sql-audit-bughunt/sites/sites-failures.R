script <- sub('^--file=', '', grep('^--file=', commandArgs(FALSE), value = TRUE))
source(file.path(dirname(normalizePath(script)), 'backend-harness.R'))
args <- commandArgs(TRUE)
with_backend_harness(args, function(drivers, config, metadata) {
  cap <- start_backend_query_capture()
  on.exit(stop_backend_query_capture(cap), add = TRUE)
  results <- list()
  verdicts <- get('share_dialect_verdicts', asNamespace('marginplyr'))
  old_cache <- as.list(verdicts, all.names = TRUE)
  on.exit({
    rm(list = ls(verdicts, all.names = TRUE), envir = verdicts)
    list2env(old_cache, envir = verdicts)
  }, add = TRUE)
  normalize_sql <- function(sql) gsub('marginplyr_share_[A-Za-z0-9]+', 'marginplyr_share_NAME', sql)
  run_pair <- function(label, invoke, before = function() NULL, after = function() NULL, warning_level = 0) {
    pair <- list()
    for (audited in c(FALSE, TRUE)) {
      options(marginplyr.audit_sql = audited, warn = warning_level)
      rm(list = ls(verdicts, all.names = TRUE), envir = verdicts)
      before()
      cap$sql <- character()
      warnings <- list()
      value <- withCallingHandlers(tryCatch(invoke(), error = identity),
        warning = function(cnd) {
          warnings[[length(warnings) + 1L]] <<- list(classes = class(cnd), message = conditionMessage(cnd))
        })
      sent <- cap$sql
      record <- if (audited) last_sent_queries(details = TRUE) else NULL
      after()
      public <- if (inherits(value, 'condition')) list(classes = class(value), message = conditionMessage(value)) else list(classes = class(value))
      if (audited) stopifnot(identical(unname(record$sql[record$purpose != 'result']), unname(sent)))
      pair[[as.character(audited)]] <- list(value = public, sent = normalize_sql(sent), warnings = warnings, record = record)
    }
    stopifnot(identical(pair[['FALSE']][c('value', 'sent', 'warnings')], pair[['TRUE']][c('value', 'sent', 'warnings')]))
    cat(label, 'PASS', '\n')
    print(pair[['TRUE']]$record[c('purpose', 'status')])
    print(lapply(pair[['TRUE']]$record$conditions, as.data.frame))
    results[[label]] <<- pair
  }
  for (backend in names(drivers)) {
    con <- drivers[[backend]]
    input <- dplyr::copy_to(con, data.frame(g = c('a','b'), v = 1:2), paste0('sites_fail_', backend), temporary = TRUE)
    label_call <- function(x) summarize_with_margins(x, total = sum(v, na.rm = TRUE), .grouping = rollup(g), .check_margin_label = TRUE)
    broken <- dplyr::mutate(input, g = !!dbplyr::sql('marginplyr_missing_function(g)'))
    run_pair(paste(backend, 'unimplemented_expression', sep='/'), function() label_call(broken))
    DBI::dbRemoveTable(con, paste0('sites_fail_', backend))
    run_pair(paste(backend, 'dropped_table', sep='/'), function() label_call(input))
  }
  pg <- drivers$pg
  input <- dplyr::copy_to(pg, data.frame(g = 'a', v = 1), 'sites_pg_conditions', temporary = TRUE)
  DBI::dbExecute(pg, "CREATE FUNCTION pg_temp.marginplyr_warn(t text) RETURNS text LANGUAGE plpgsql AS 'BEGIN RAISE WARNING ''live backend warning''; RETURN t; END'")
  warned <- dplyr::mutate(input, g = !!dbplyr::sql('pg_temp.marginplyr_warn(g)'))
  for (warning_level in c(-1, 0, 1, 2)) run_pair(paste0('pg/live_warning/', warning_level), function() summarize_with_margins(warned, total = sum(v, na.rm = TRUE), .grouping = rollup(g), .check_margin_label = TRUE), warning_level = warning_level)
  DBI::dbExecute(pg, "CREATE FUNCTION pg_temp.marginplyr_pause(t text) RETURNS text LANGUAGE plpgsql AS 'BEGIN PERFORM pg_sleep(0.1); RETURN t; END'")
  paused <- dplyr::mutate(input, g = !!dbplyr::sql('pg_temp.marginplyr_pause(g)'))
  run_pair('pg/live_statement_timeout', function() summarize_with_margins(paused, total = sum(v, na.rm = TRUE), .grouping = rollup(g), .check_margin_label = TRUE),
    before = function() DBI::dbExecute(pg, 'SET statement_timeout=10'), after = function() DBI::dbExecute(pg, 'SET statement_timeout=0'))
  share_call <- function() summarize_with_margins(input, total = sum(v, na.rm = TRUE), share = share_of_total(total), .grouping = rollup(g))
  run_pair('pg/read_only_transaction_probe', share_call,
    before = function() {DBI::dbBegin(pg); DBI::dbExecute(pg, 'SET TRANSACTION READ ONLY')},
    after = function() {stopifnot(DBI::dbGetQuery(pg, 'SELECT 1 AS intact')$intact == 1L); DBI::dbRollback(pg)})
  run_pair('pg/aborted_transaction_probe', share_call,
    before = function() {DBI::dbBegin(pg); tryCatch(DBI::dbExecute(pg, 'SELECT marginplyr_missing_function()'), error = function(cnd) NULL)},
    after = function() DBI::dbRollback(pg))
  saveRDS(list(metadata=metadata, cases=results), file.path(config$output, 'sites-failures.rds'))
  cat('TOTAL FAILURE/WARNING/TRANSACTION PAIRS', length(results), '\n')
  stopifnot(length(results) == 13L)
})
