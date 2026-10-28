pkgload::load_all('.', quiet = TRUE)
args <- commandArgs(TRUE)
script <- sub('^--file=', '', grep('^--file=', commandArgs(FALSE), value = TRUE))
source(file.path(dirname(normalizePath(script)), 'backend-harness.R'))
with_backend_harness(args, function(drivers, config, metadata) {
  cap <- start_backend_query_capture()
  on.exit(stop_backend_query_capture(cap), add = TRUE)
  results <- list()
  collect_case <- function(label, make_call) {
    pair <- list()
    for (audit in c(FALSE, TRUE)) {
      options(marginplyr.audit_sql = audit)
      cap$sql <- character()
      delivered <- list()
      value <- withCallingHandlers(tryCatch(make_call(), error = identity),
        warning = function(cnd) {
          delivered[[length(delivered) + 1L]] <<- list(class(cnd), conditionMessage(cnd))
          invokeRestart('muffleWarning')
        })
      sent <- cap$sql
      record <- if (audit) last_sent_queries(details = TRUE) else NULL
      if (audit) stopifnot(identical(unname(record$sql[record$purpose != 'result']), unname(sent)))
      later_conditions <- list()
      collected <- withCallingHandlers(tryCatch(if (inherits(value, 'tbl_lazy')) dplyr::collect(value) else value, error = identity),
        warning = function(cnd) {
          later_conditions[[length(later_conditions) + 1L]] <<- list(class(cnd), conditionMessage(cnd))
          invokeRestart('muffleWarning')
        })
      serial <- function(x) {
        if (inherits(x, 'condition')) return(list(class = class(x), message = conditionMessage(x)))
        if (is.data.frame(x) && nrow(x)) {
          keys <- vapply(seq_len(nrow(x)), function(i) {
            row <- x[i, , drop = FALSE]
            rownames(row) <- NULL
            paste(as.integer(serialize(row, NULL, version = 3)), collapse = ',')
          }, character(1))
          x <- x[order(keys), , drop = FALSE]
          rownames(x) <- NULL
        }
        x
      }
      pair[[as.character(audit)]] <- list(value = serial(collected), sent = sent, delivered = delivered, later_conditions = later_conditions, record = record)
      if (audit) stopifnot(identical(record, last_sent_queries(details = TRUE)))
    }
    eq <- identical(pair[['FALSE']][c('value', 'sent', 'delivered', 'later_conditions')], pair[['TRUE']][c('value', 'sent', 'delivered', 'later_conditions')])
    stopifnot(eq)
    outcome <- pair[['TRUE']]$value
    cat(label, if (is.list(outcome) && 'message' %in% names(outcome)) paste('ERROR', outcome$message) else 'OK', '\n')
    results[[label]] <<- pair
  }
  special <- c('normal', '.env', '.data', 'sum', 'if_else', 'as.character', 'NA', 'NULL', 'NaN', 'Inf', 'TRUE', 'FALSE', 'T', 'F', '.', '..', '..g', '.N', '.I', '.BY', 'x\nq', 'x\tq', 'a`"b', "a'b", 'x\\q', '地域', 'a b', 'x;y--', 'purpose', 'sql', 'source', 'status', 'conditions', 'result', 'g..1')
  labels <- c('Total', '', "a'b", 'a`"b', '\n', '\t', '地域', 'NULL', 'NaN', 'Inf', '1', '1.0', 'TRUE')
  for (backend in names(drivers)) {
    con <- drivers[[backend]]
    for (column in special) for (source_mode in c('safe', 'colliding')) {
      source_values <- if (source_mode == 'safe') c('a', 'b', 'c') else c('a', 'b', 'Total')
      data <- setNames(data.frame(source_values, c(1, 2, 3)), c(column, 'v'))
      input <- dplyr::copy_to(con, data, paste0('sites_n_', length(results)), temporary = TRUE)
      for (verb in c('summarize', 'expand', 'inspect')) {
        grouping <- rollup(dplyr::all_of(column))
        collect_case(paste(backend, 'name', source_mode, column, verb, sep='/'), function() {
          switch(verb,
            summarize = summarize_with_margins(input, total = sum(v, na.rm = TRUE), .grouping = grouping, .check_margin_label = TRUE),
            expand = expand_with_margins(input, .grouping = grouping, .check_margin_label = TRUE),
            inspect = inspect_grouping(input, .grouping = grouping))
        })
      }
    }
    for (label in labels) for (source_mode in c('safe', 'colliding')) {
      source_values <- if (source_mode == 'safe') c('safe-a', 'safe-b') else c('a', label)
      input <- dplyr::copy_to(con, data.frame(g = source_values, v = c(1,2)), paste0('sites_l_', length(results)), temporary = TRUE)
      collect_case(paste(backend, 'label', source_mode, label, sep='/'), function() summarize_with_margins(input, total = sum(v, na.rm = TRUE), .grouping = rollup(g), .margin_label = label, .check_margin_label = TRUE))
    }
    base <- dplyr::copy_to(con, data.frame(g = c(1,2), v = c(1,2)), paste0('sites_base_', backend), temporary = TRUE)
    for (type in c('integer', 'double', 'boolean', 'date', 'datetime', 'blob')) {
      expr <- switch(type,
        integer = 'CAST(g AS INTEGER)', double = 'CAST(g AS DOUBLE PRECISION)',
        boolean = if(backend == 'sqlite') 'g = 1' else 'CAST(g = 1 AS BOOLEAN)',
        date = if(backend == 'sqlite') "date('2026-01-01')" else "CAST('2026-01-01' AS DATE)",
        datetime = if(backend == 'sqlite') "datetime('2026-01-01 12:00:00')" else "CAST('2026-01-01 12:00:00' AS TIMESTAMP)",
        blob = if(backend == 'pg') "decode('ab', 'hex')" else "X'ab'")
      input <- dplyr::mutate(base, g = !!dbplyr::sql(expr))
      collect_case(paste(backend, 'type', type, sep='/'), function() summarize_with_margins(input, total = sum(v, na.rm = TRUE), .grouping = rollup(g), .check_margin_label = TRUE))
    }
  }
  saveRDS(list(metadata=metadata, cases=results), file.path(config$output, 'sites-matrix.rds'))
  vals <- lapply(results, function(pair) pair[['TRUE']])
  cat('TOTAL PAIRS', length(results), '\n')
  cat('INTERNAL SENDS PER AUDIT SETTING', sum(vapply(vals, function(z) length(z$sent), integer(1))), '\n')
  print(table(unlist(lapply(vals, function(z) z$record$status))))
  print(table(unlist(lapply(vals, function(z) z$record$purpose))))
  stopifnot(length(results) == 726L,
    all(unlist(lapply(vals, function(z) z$record$status[z$record$purpose != 'result'])) == 'returned'),
    all(unlist(lapply(vals, function(z) z$record$status[z$record$purpose == 'result'])) == 'prepared'),
    all(unlist(lapply(vals, function(z) vapply(z$record$conditions, nrow, integer(1)))) == 0L))
})
