# Dated live SQL and one-shot Arrow inspection/admission probes.
pkgload::load_all('.', quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
observe <- function(expr) tryCatch(force(expr), error = identity)
arrow_checks <- 0L
reader_rows <- 0L
for (case in c('distinct', 'head', 'grouped_head', 'join_union')) {
  input <- tibble(g = c('b', 'a', 'b'), v = c(2L, 1L, 2L))
  reader <- arrow::RecordBatchReader$create(arrow::RecordBatch$create(input))
  source <- select(reader, everything())
  table <- arrow::Table$create(input)
  q <- switch(case,
    distinct = distinct(source),
    head = head(arrange(source, v), 1L),
    grouped_head = summarize(group_by(source, g), v = sum(v, na.rm = TRUE)),
    join_union = union_all(source, select(inner_join(table, source, by = c('g', 'v')), g, v)))
  for (format in c('list', 'text')) for (typed in c(FALSE, TRUE)) {
    plan <- if (typed) inspect_grouping(q, .grouping = rollup(where(is.character)), .format = format) else inspect_grouping(q, .grouping = rollup(g), .format = format)
    stopifnot(!inherits(plan, 'error'))
    arrow_checks <- arrow_checks + 1L
  }
  for (fun in list(nest_with_margins, nest_by_with_margins)) {
    result <- observe(fun(q, .grouping = rollup(g)))
    stopifnot(inherits(result, 'marginplyr_error'))
    arrow_checks <- arrow_checks + 1L
  }
  retained <- as_tibble(as.data.frame(reader))
  stopifnot(identical(retained, input))
  reader_rows <- reader_rows + nrow(retained)
}
cat('Arrow boundary calls', arrow_checks, '; readers preserved 4; retained source rows', reader_rows, '\n')
for (engine in c('sqlite', 'duckdb')) {
  connection <- if (engine == 'sqlite') DBI::dbConnect(RSQLite::SQLite(), ':memory:') else DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  source <- copy_to(connection, tibble(g = c('a', 'b', 'a'), v = c(1L, 2L, 1L)), 'schema_boundary_input', temporary = TRUE)
  graphs <- list(
    distinct = distinct(source),
    grouped = group_by(source, g),
    windowed = mutate(arrange(source, desc(v)), position = row_number()),
    nested_limit = head(arrange(head(arrange(source, v), 2L), desc(v)), 1L),
    grouped_summary = summarize(group_by(source, g), v = sum(v, na.rm = TRUE)),
    self_union = union_all(distinct(source), distinct(source)))
  statements <- character()
  trace('dbSendQuery', where = asNamespace('DBI'), print = FALSE,
        tracer = quote({statements <<- c(statements, as.character(statement))}))
  checks <- 0L
  for (name in names(graphs)) {
    q <- graphs[[name]]
    for (fun in list(nest_with_margins, nest_by_with_margins)) {
      before <- length(statements)
      error <- observe(fun(q, .grouping = grouping_set()))
      stopifnot(inherits(error, 'marginplyr_error'), length(statements) == before)
      checks <- checks + 1L
    }
    for (format in c('list', 'text')) for (typed in c(FALSE, TRUE)) {
      before <- length(statements)
      plan <- observe(if (typed) inspect_grouping(q, .grouping = grouping_set(where(is.integer)), .format = format) else inspect_grouping(q, .grouping = grouping_set(v), .format = format))
      if (engine == 'sqlite' && typed) stopifnot(inherits(plan, 'marginplyr_error'), length(statements) == before)
      else stopifnot(!inherits(plan, 'error'), length(statements) == before + as.integer(engine == 'duckdb'))
      checks <- checks + 1L
    }
    for (invalid in list(function(x) inspect_grouping(x, .format = 'invalid'), function(x) inspect_grouping(x, .duplicates = 'invalid'), function(x) inspect_grouping(x, .grouping = rollup()), function(x) inspect_grouping(x, .by = all_of('absent')))) {
      before <- length(statements)
      error <- observe(invalid(q))
      stopifnot(inherits(error, 'error'), length(statements) == before)
      checks <- checks + 1L
    }
  }
  stopifnot(!length(statements) || all(grepl('LIMIT 0', statements, fixed = TRUE)))
  untrace('dbSendQuery', where = asNamespace('DBI'))
  cat('SQL', engine, 'boundary calls', checks, '; statements', length(statements), '; every statement LIMIT 0\n')
  DBI::dbDisconnect(connection, shutdown = engine == 'duckdb')
}
