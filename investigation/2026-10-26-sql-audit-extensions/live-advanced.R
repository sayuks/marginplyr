main <- function(args) {
  stopifnot(length(args) >= 3L)
  stopifnot(args[[1L]] %in% c('sqlite', 'duckdb', 'postgres'),
            args[[2L]] %in% c('on', 'off'))
  if (file.exists(args[[3L]])) stop('Output already exists')
backend <- args[[1]]; audited <- identical(args[[2]],'on'); outfile <- args[[3]]
pkgload::load_all(quiet=TRUE)
options(marginplyr.audit_sql=audited)
con <- switch(backend, sqlite=DBI::dbConnect(RSQLite::SQLite(), ':memory:'), duckdb=DBI::dbConnect(duckdb::duckdb(shared_home=FALSE)), postgres=DBI::dbConnect(RPostgres::Postgres(),host=args[[4]],port=as.integer(args[[5]]),dbname='postgres'))
on.exit(DBI::dbDisconnect(con))
src <- dplyr::copy_to(con, data.frame(g=c('a', NA, 'b','a'), h=c('q','x','y','x'), v=c(1,NA,3,4), w=1:4), 'source',temporary=TRUE)
weird <- dplyr::copy_to(con, setNames(data.frame(c('a','b','a'), c(1,2,3), c(4,5,6), c('p','q','r')),c('グループ','a`"b','.env','result')), 'weird',temporary=TRUE)
inputs <- list(
 plain=src,
 subset=dplyr::filter(dplyr::mutate(src, v=v*2), w>1),
 grouped=dplyr::group_by(src,h),
 arranged=dplyr::arrange(src,g,w),
 window=dplyr::mutate(dplyr::arrange(src,w),v=cumsum(dplyr::coalesce(v,0))),
 window_unordered=dplyr::mutate(src,v=cumsum(dplyr::coalesce(v,0))),
 join=dplyr::left_join(src,dplyr::transmute(src,w,extra=h),by='w'),
 union=dplyr::union_all(src,src),
 literal=dplyr::tbl(con,dbplyr::sql('SELECT g, h, v, w FROM source')),
 head=utils::head(src,2),
 empty=dplyr::filter(src,w<0)
)
TRACE_SQL <<- character()
trace('dbSendQuery',where=asNamespace('DBI'),tracer=quote({ TRACE_SQL <<- c(TRACE_SQL,as.character(statement)) }),print=FALSE)
trace('dbExecute',where=asNamespace('DBI'),tracer=quote({ TRACE_SQL <<- c(TRACE_SQL,as.character(statement)) }),print=FALSE)
on.exit({untrace('dbSendQuery',where=asNamespace('DBI'));untrace('dbExecute',where=asNamespace('DBI'))}, add=TRUE)
verdicts <- getFromNamespace('share_dialect_verdicts','marginplyr')
run <- function(label, expr) {
 rm(list=ls(verdicts),envir=verdicts)
 TRACE_SQL <<- character(); set.seed(440)
 seed <- .Random.seed
 warnings <- character(); errors <- NULL
 value <- withCallingHandlers(tryCatch(force(expr),error=identity),warning=function(cnd) {warnings <<- c(warnings,conditionMessage(cnd));invokeRestart('muffleWarning')})
 sent <- TRACE_SQL; construction_seed <- identical(seed,.Random.seed)
 record <- if(audited) last_sent_queries(details=TRUE)
 render <- if(inherits(value,'tbl_lazy')) tryCatch(as.character(dbplyr::sql_render(value)),error=function(cnd) list(classes=class(cnd),message=conditionMessage(cnd))) else NULL
 data <- if(inherits(value,'tbl_lazy')) tryCatch(dplyr::collect(value),error=function(cnd) list(classes=class(cnd),message=conditionMessage(cnd))) else if(inherits(value,'condition')) list(classes=class(value),message=conditionMessage(value)) else value
 if(audited) stopifnot(identical(record,last_sent_queries(details=TRUE)))
 list(label=label, sent=sent, warnings=warnings, seed_unchanged=construction_seed, render=render, data=data, record=record)
}
res <- list()
for(name in names(inputs)) {
 x<-inputs[[name]]
 for(verb in c('inspect','summarize','expand','share','nest','nestby')) {
   res[[length(res)+1L]] <- run(paste(name,verb,sep=':'),switch(verb,
     inspect=inspect_grouping(x,.grouping=rollup(dplyr::where(is.character))),
     summarize=summarize_with_margins(x,total=sum(v,na.rm=TRUE),.grouping=rollup(g,h),.check_margin_label=TRUE,.sort='last',.id='set'),
     expand=expand_with_margins(x,.grouping=rollup(g,h),.check_margin_label=TRUE,.sort='last',.id='set'),
     share=summarize_with_margins(x,total=sum(v,na.rm=TRUE),share=share_of_total(total),.grouping=rollup(g),.check_margin_label=TRUE,.check_share_source=TRUE),
     nest=nest_with_margins(x,.grouping=rollup(g),.check_margin_label=TRUE),
     nestby=nest_by_with_margins(x,.grouping=rollup(g),.check_margin_label=TRUE)))
 }
}
for(verb in c('summarize','expand','inspect'))res[[length(res)+1L]]<-run(paste('weird',verb,sep=':'),switch(verb,
 summarize=summarize_with_margins(weird,total=sum(.data[['a`"b']],na.rm=TRUE),.grouping=rollup(dplyr::all_of('グループ')),.check_margin_label=TRUE,.sort='last'),
 expand=expand_with_margins(weird,.grouping=rollup(dplyr::all_of('グループ')),.check_margin_label=TRUE,.sort='last'),
 inspect=inspect_grouping(weird,.grouping=rollup(dplyr::all_of('グループ')))))
if(backend=='postgres') {
 DBI::dbBegin(con)
 res[[length(res)+1L]] <- run('transaction:share_cold',summarize_with_margins(src,total=sum(v,na.rm=TRUE),share=share_of_total(total),.grouping=rollup(g),.check_margin_label=TRUE))
 stopifnot(RPostgres::postgresIsTransacting(con),identical(DBI::dbGetQuery(con,'SELECT 9 AS z')$z,9L))
 DBI::dbRollback(con)
}
saveRDS(res,outfile)
cat('Completed',backend,if(audited)'on'else'off',length(res),'cases\n')

}
main(commandArgs(TRUE))
