args <- commandArgs(TRUE)
backend <- args[[1]]
audit <- identical(args[[2]], 'on')
outfile <- args[[3]]
pkgload::load_all(quiet=TRUE)
options(marginplyr.audit_sql = audit)
con <- switch(backend, sqlite = DBI::dbConnect(RSQLite::SQLite(), ':memory:'), duckdb = DBI::dbConnect(duckdb::duckdb(shared_home=FALSE)), postgres = DBI::dbConnect(RPostgres::Postgres(), host=args[[4]], port=as.integer(args[[5]]), dbname='postgres'))
on.exit(DBI::dbDisconnect(con), add=TRUE)
src <- dplyr::copy_to(con, data.frame(g=c('a','b','a'), h=c('x','x','y'), v=c(1,2,3)), 'hunt_source', temporary=TRUE)
TRACE_SQL <- character()
verdicts <- getFromNamespace('share_dialect_verdicts', 'marginplyr')
trace('dbSendQuery', where=asNamespace('DBI'), tracer=quote({ TRACE_SQL <<- c(TRACE_SQL, as.character(statement)) }), print=FALSE)
trace('dbExecute', where=asNamespace('DBI'), tracer=quote({ TRACE_SQL <<- c(TRACE_SQL, as.character(statement)) }), print=FALSE)
run <- function(label, expr, cold=FALSE) {
  if(cold) rm(list=ls(verdicts),envir=verdicts)
  TRACE_SQL <<- character()
  cond <- list()
  value <- withCallingHandlers(tryCatch(force(expr), error=identity, interrupt=identity), condition=function(cnd) {
    if(inherits(cnd,c('warning','error','interrupt'))) cond[[length(cond)+1L]] <<- list(class=class(cnd),message=conditionMessage(cnd))
  })
  sql <- TRACE_SQL
  rec <- if(audit) marginplyr::last_sent_queries(details=TRUE) else NULL
  result <- if(inherits(value, 'condition')) list(class=class(value),message=conditionMessage(value)) else if(inherits(value,'tbl_lazy')) list(class=class(value), vars=as.character(dplyr::tbl_vars(value))) else value
  list(label=label, sql=sql, conditions=cond, result=result, record=rec)
}
res <- list(
run('inspect', inspect_grouping(src, .grouping=rollup(g))),
run('inspect_typed', inspect_grouping(src, .grouping=rollup(where(is.character)))),
run('label_good', summarize_with_margins(src, n=dplyr::n(), .grouping=rollup(g,h), .check_margin_label=TRUE)),
run('label_collision', summarize_with_margins(dplyr::mutate(src,g='Total'), n=dplyr::n(), .grouping=rollup(g), .check_margin_label=TRUE)),
run('label_missing', summarize_with_margins(src, n=dplyr::n(), .grouping=rollup(g), .margin_label=NA_character_, .check_margin_label=TRUE)),
run('label_emptycols', summarize_with_margins(src, n=dplyr::n(), .check_margin_label=TRUE)),
run('expand', expand_with_margins(src, .grouping=rollup(g,h), .check_margin_label=TRUE)),
run('summarize', summarize_with_margins(src, n=sum(v,na.rm=TRUE), .grouping=rollup(g))),
run('share_cold', summarize_with_margins(src, total=sum(v,na.rm=TRUE), share=share_of_total(total), .grouping=rollup(g)), TRUE),
run('share_cache', summarize_with_margins(src, total=sum(v,na.rm=TRUE), share=share_of_total(total), .grouping=rollup(g))),
run('local', inspect_grouping(data.frame(g='a',v=1), .grouping=rollup(g)))
)
if(backend=='postgres') {
 DBI::dbBegin(con)
 res[[length(res)+1L]] <- run('share_transaction_cold', summarize_with_margins(src,total=sum(v,na.rm=TRUE),share=share_of_total(total),.grouping=rollup(g)),TRUE)
 stopifnot(RPostgres::postgresIsTransacting(con), identical(DBI::dbGetQuery(con,'SELECT 9 AS z')$z,9L))
 DBI::dbRollback(con)
}
saveRDS(res,outfile)
for(x in res) {cat(x$label, 'SQL', length(x$sql), 'ROWS', if(audit)nrow(x$record) else NA_integer_,'\n');if(audit) print(x$record[c('purpose','status')])}
