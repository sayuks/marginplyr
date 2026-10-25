# Recorded investigation: 2026-10-25, c62ecdb01d31ca5c2c9ab8baed0dda970dd844e5.
# Run in a fresh R process from the repository root. This is an investigation
# harness, not a package test or implementation. See manifest.txt for limits.

# Optional postgres uses libpq PG* environment variables and an existing server.
# With no arguments, only in-memory SQLite and DuckDB are used.
pkgload::load_all('.', quiet = TRUE)
library(dplyr)
# Reads the internal verdict environment to establish the same cold or primed
# cache state for each audit setting before measuring construction.
share_verdicts <- utils::getFromNamespace('share_dialect_verdicts', 'marginplyr')
run_backend <- function(kind) {
  con <- switch(kind,
    sqlite = DBI::dbConnect(RSQLite::SQLite(), ':memory:'),
    duckdb = DBI::dbConnect(duckdb::duckdb(shared_home=FALSE), dbdir = ':memory:'),
    postgres = DBI::dbConnect(RPostgres::Postgres(), dbname=Sys.getenv('PGDATABASE', 'postgres'))
  )
  on.exit(DBI::dbDisconnect(con), add=TRUE)
  remote <- copy_to(con, data.frame(g=c('a','b'), h=c('x','y'), v=c(2,6)), 'audit_live')
  scenarios <- c('inspect','label','collision','share','share_cached')
  for (scenario in scenarios) {
    results <- lapply(c(FALSE, TRUE), function(audited) {
      options(marginplyr.audit_sql=audited)
      rm(list=ls(share_verdicts), envir=share_verdicts)
      if (scenario=='share_cached') try(summarize_with_margins(remote, amount=sum(v,na.rm=TRUE), share=share_of_total(amount), .grouping=rollup(g)), silent=TRUE)
      if(kind=='postgres') DBI::dbBegin(con)
      queries <- character()
      execute <- DBI::dbExecute
      getquery <- DBI::dbGetQuery
      sendquery <- DBI::dbSendQuery
      testthat::local_mocked_bindings(dbExecute=function(conn, statement, ...) {
        queries <<- c(queries, as.character(statement))
        execute(conn, statement, ...)
      }, dbGetQuery=function(conn, statement, ...) {
        queries <<- c(queries, as.character(statement))
        getquery(conn, statement, ...)
      }, dbSendQuery=function(conn, statement, ...) {
        queries <<- c(queries, as.character(statement))
        sendquery(conn, statement, ...)
      }, .package='DBI')
      warnings <- list()
      result <- withCallingHandlers(tryCatch(switch(scenario,
        inspect=inspect_grouping(remote, .grouping=rollup(g)),
        label=summarize_with_margins(remote, total=sum(v,na.rm=TRUE), .grouping=rollup(g), .check_margin_label=TRUE),
        collision=summarize_with_margins(mutate(remote,g='Total'), total=sum(v,na.rm=TRUE), .grouping=rollup(g), .check_margin_label=TRUE),
        share=summarize_with_margins(remote, amount=sum(v,na.rm=TRUE), share=share_of_total(amount), .grouping=rollup(g)),
        share_cached=summarize_with_margins(remote, amount=sum(v,na.rm=TRUE), share=share_of_total(amount), .grouping=rollup(g))
      ),error=identity), warning=function(cnd) {warnings[[length(warnings)+1L]] <<- list(class=class(cnd),message=conditionMessage(cnd)); invokeRestart('muffleWarning')})
      recorded <- if(audited) last_sent_queries(details=TRUE) else NULL
      statements <- queries
      value <- if(inherits(result,'tbl_sql')) tryCatch(collect(result),error=identity) else result
      if(is.data.frame(value) && 'g' %in% names(value)) value <- arrange(value,g)
      if(kind=='postgres') {
        stopifnot(RPostgres::postgresIsTransacting(con))
        stopifnot(DBI::dbGetQuery(con,'SELECT 1 AS ok')$ok==1L)
        DBI::dbRollback(con)
      }
      normalize <- function(x) gsub('marginplyr_share_[[:alnum:]]+', 'marginplyr_share_ID',x)
      list(sql=normalize(statements), value=if(inherits(value,'condition')) list(class=class(value),message=conditionMessage(value)) else value,
           warnings=warnings, rows=recorded)
    })
    off <- results[[1]]; on <- results[[2]]
    cat('\nBACKEND',kind,'SCENARIO',scenario,'SQL_IDENTICAL',identical(off$sql,on$sql),'VALUE_IDENTICAL',identical(off$value,on$value),'WARNINGS_IDENTICAL',identical(off$warnings,on$warnings),'DBI_CALL_COUNT',length(on$sql),'\n')
    if(!is.null(on$rows)) print(on$rows[c('purpose','source','status')])
    if(!identical(off$sql,on$sql)) {print(off$sql); print(on$sql)}
  }
}
backends <- commandArgs(TRUE)
if(!length(backends)) backends <- c('sqlite','duckdb')
for(kind in backends) run_backend(kind)
