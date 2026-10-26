# Run from the checkout root in a fresh R process.
# Rscript deep-matrix.R <sqlite|duckdb|postgres> <off|on> <fresh-output.rds> [pg-socket pg-port]
# PostgreSQL expects a disposable postgres database. No persistent user table is used.
main <- function(args) {
  stopifnot(length(args)>=3L, args[[1]] %in% c('sqlite','duckdb','postgres'), args[[2]] %in% c('off','on'), !file.exists(args[[3]]))
  pkgload::load_all(quiet=TRUE)
  backend <- args[[1]]; audited <- identical(args[[2]],'on')
  options(marginplyr.audit_sql=audited)
  con <- switch(backend, sqlite=DBI::dbConnect(RSQLite::SQLite(),':memory:'), duckdb=DBI::dbConnect(duckdb::duckdb(shared_home=FALSE)), postgres=DBI::dbConnect(RPostgres::Postgres(),host=args[[4]],port=as.integer(args[[5]]),dbname='postgres'))
  on.exit(DBI::dbDisconnect(con), add=TRUE)
  df <- data.frame(g=c('a','b','a',NA,'Total'),h=c('x','x','y','y','x'), v=c(1,2,3,NA,5), w=1:5, flag=c(TRUE,FALSE,NA,TRUE,FALSE), date=as.Date('2020-01-01')+1:5)
  S <- dplyr::copy_to(con, df,'deep_source',temporary=TRUE)
  weird_df <- setNames(data.frame(c('a','b','a'),c('x','y','x'),c(1,2,3),c(4,5,6)),c('TRUE','FALSE','.env','..marginplyr_order_1'))
  W <- dplyr::copy_to(con,weird_df,'deep_weird',temporary=TRUE)
  inputs <- list(base=S, empty=dplyr::filter(S,FALSE), recursive=summarize_with_margins(S,v=sum(v,na.rm=TRUE),.grouping=rollup(g,h),.margin_label=NULL), summarized=dplyr::summarize(dplyr::group_by(S,g,h),v=sum(v,na.rm=TRUE),.groups='drop'))
  script <- sub('^--file=', '', grep('^--file=', commandArgs(), value=TRUE)[[1L]])
  case_sets <- dget(file.path(dirname(script), 'cases.R'))
  cases <- case_sets$cases; extra <- case_sets$extra; weird <- case_sets$weird
  TRACE_SQL <<- character()
  trace('dbSendQuery',where=asNamespace('DBI'),tracer=quote({TRACE_SQL <<- c(TRACE_SQL,as.character(statement))}),print=FALSE)
  trace('dbExecute',where=asNamespace('DBI'),tracer=quote({TRACE_SQL <<- c(TRACE_SQL,as.character(statement))}),print=FALSE)
  on.exit({untrace('dbSendQuery',where=asNamespace('DBI'));untrace('dbExecute',where=asNamespace('DBI'))},add=TRUE)
  verdicts <- getFromNamespace('share_dialect_verdicts','marginplyr')
  capture <- function(expr) {
    cond <- list()
    value <- withCallingHandlers(tryCatch(force(expr),error=identity),condition=function(cnd) {if(inherits(cnd,c('warning','message','error')))cond[[length(cond)+1L]]<<-list(class=class(cnd),message=conditionMessage(cnd));if(inherits(cnd,'warning')&&!is.null(findRestart('muffleWarning')))invokeRestart('muffleWarning');if(inherits(cnd,'message')&&!is.null(findRestart('muffleMessage')))invokeRestart('muffleMessage')})
    list(value=value,conditions=cond)
  }
  run <- function(input,label,expr) {
    S <- input; TRACE_SQL <<- character(); set.seed(780)
    out <- capture(eval(expr)); sent <- TRACE_SQL
    rec <- if(audited)last_sent_queries(details=TRUE)
    final <- if(inherits(out$value,'tbl_lazy'))capture(as.character(dbplyr::sql_render(out$value))) else NULL
    collected <- if(inherits(out$value,'tbl_lazy'))capture(dplyr::collect(out$value)) else NULL
    simplify <- function(v)if(inherits(v,'condition'))list(class=class(v),message=conditionMessage(v))else if(inherits(v,'tbl_lazy'))list(class=class(v),vars=dplyr::tbl_vars(v))else v
    if(audited)stopifnot(identical(rec,last_sent_queries(details=TRUE)))
    list(label=label,sent=sent,seed=.Random.seed,value=simplify(out$value),conditions=out$conditions,render=if(!is.null(final))list(value=simplify(final$value),conditions=final$conditions),collected=if(!is.null(collected))list(value=simplify(collected$value),conditions=collected$conditions),record=rec)
  }
  results <- list()
  for(nm in names(inputs)) {
    rm(list=ls(verdicts),envir=verdicts)
    for(case in names(cases))results[[paste(nm,case,sep='/')]]<-run(inputs[[nm]],paste(nm,case,sep='/'),cases[[case]])
  }
  for(case in names(extra))results[[paste('extra',case,sep='/')]]<-run(inputs$base,paste('extra',case,sep='/'),extra[[case]])
  for(case in names(weird))results[[paste('weird',case,sep='/')]]<-run(W,paste('weird',case,sep='/'),weird[[case]])
  if(backend=='postgres') {
    DBI::dbBegin(con)
    rm(list=ls(verdicts),envir=verdicts)
    results[['transaction/share_cold']] <- run(inputs$base,'transaction/share_cold',quote(summarize_with_margins(S,total=sum(v,na.rm=TRUE),part=share_of_total(total),.grouping=rollup(g,h),.margin_label=NULL)))
    stopifnot(RPostgres::postgresIsTransacting(con), identical(DBI::dbGetQuery(con,'SELECT 9 AS z')$z,9L))
    DBI::dbRollback(con)
  }
  saveRDS(results,args[[3]])
  cat(backend,args[[2]],length(results),'cases completed\n')
}
main(commandArgs(TRUE))
