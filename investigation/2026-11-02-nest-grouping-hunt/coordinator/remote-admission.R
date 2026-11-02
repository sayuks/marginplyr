# Dated remote refusal/inspection probe, not package tests.
pkgload::load_all('.', quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
options(marginplyr.audit_sql = TRUE)
observe <- function(expr) tryCatch(force(expr), error = identity)
count <- 0L
for (engine in c('RSQLite', 'duckdb')) {
  con <- if (engine == 'RSQLite') DBI::dbConnect(RSQLite::SQLite(), ':memory:') else
    DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  input <- tibble(g = c('a','b'), row = 1:2)
  root <- copy_to(con, input, 'probe', temporary = TRUE)
  queries <- list(root, select(root, renamed = g, row),
                  filter(root, row < 0L),
                  union_all(root, root), inner_join(root, root, by=c('g','row')))
  statements <- character()
  trace('dbSendQuery', where=asNamespace('DBI'), print=FALSE,
        tracer=quote({ statements <<- c(statements, as.character(statement)) }))
  for (q in queries) {
    for (verb in list(nest_with_margins, nest_by_with_margins)) {
      forced <- FALSE
      before <- length(statements)
      result <- observe(verb(q, .grouping = { forced <- TRUE; stop('must remain unforced') }))
      stopifnot(inherits(result, 'marginplyr_error'), !forced,
                length(statements)==before, nrow(last_sent_queries())==0L)
      count <- count+1L
    }
    before <- length(statements)
    invalid <- observe(inspect_grouping(q, .grouping = grouping_set(all_of('absent'))))
    stopifnot(inherits(invalid, 'error'), length(statements)==before)
    count <- count+1L
    plan <- inspect_grouping(q, .grouping = grouping_set(everything()), .format='list')
    stopifnot(identical(plan$included, list(colnames(q))))
    record <- last_sent_queries(details=TRUE)
    stopifnot(length(statements)-before == as.integer(engine=='duckdb'),
              nrow(record)==as.integer(engine=='duckdb'))
    if (nrow(record)) stopifnot(all(grepl('LIMIT 0', record$sql, fixed=TRUE)))
    count <- count+1L
  }
  untrace('dbSendQuery', where=asNamespace('DBI'))
  cat(engine, ':', length(queries)*4L, 'calls;', length(statements), 'statements, all LIMIT 0\n')
  stopifnot(all(grepl('LIMIT 0', statements, fixed=TRUE)))
  DBI::dbDisconnect(con, shutdown=identical(engine,'duckdb'))
}
for (shape in c('reader','query','union','join','empty')) {
  input <- tibble(g=c('a','b'), row=1:2)
  reader <- arrow::RecordBatchReader$create(arrow::RecordBatch$create(input))
  query <- select(reader, everything())
  q <- switch(shape, reader=reader, query=query, union=union_all(query, query),
              join=inner_join(query, arrow::Table$create(input), by=c('g','row')),
              empty=filter(query,row<0L))
  for (verb in list(nest_with_margins,nest_by_with_margins)) {
    forced <- FALSE
    result <- observe(verb(q, .by={forced<-TRUE;stop('must remain unforced')}))
    stopifnot(inherits(result,'marginplyr_error'),!forced,nrow(last_sent_queries())==0L)
    count <- count+1L
  }
  for (typed in c(FALSE,TRUE)) {
    plan <- if (typed) inspect_grouping(q,.grouping=rollup(where(is.character)),.format='list') else
      inspect_grouping(q,.grouping=rollup(g),.format='list')
    stopifnot(identical(plan$included,list('g',character())),nrow(last_sent_queries())==0L)
    count <- count+1L
  }
  stopifnot(identical(as_tibble(as.data.frame(reader)),input))
}
cat('Arrow: 20 calls; five readers, all ten rows preserved; total',count,'calls\n')
