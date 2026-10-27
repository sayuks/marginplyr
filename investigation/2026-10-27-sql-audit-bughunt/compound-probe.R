# Controlled backend conditions through the public share probe; no driver is claimed to emit these.
main <- function() {
 old <- options(c('warn', 'marginplyr.audit_sql'))
 on.exit(options(old), add=TRUE)
pkgload::load_all('.', quiet = TRUE)
probe <- function(classes, terminator) {
 con<-DBI::dbConnect(RSQLite::SQLite(),':memory:');on.exit(DBI::dbDisconnect(con))
 remote<-dplyr::copy_to(con,data.frame(g='a',v=1),'src')
 leaf<-structure(list(message='compound probe cancellation',call=NULL),class=classes)
 collections<-0L
 testthat::local_mocked_bindings(collect=function(x,...) {
  collections<<-collections+1L
  if(terminator=='stop') stop(leaf)
  if(terminator=='signal') signalCondition(leaf)
  if(terminator=='warning') warning(leaf)
  data.frame(p=0)
 },.package='dplyr')
 runs<-lapply(c(FALSE,TRUE),function(a) {
  cache<-getFromNamespace('share_dialect_verdicts', 'marginplyr')
  rm(list=ls(cache,all.names=TRUE),envir=cache)
  options(marginplyr.audit_sql=a)
  delivered<-list()
  outcome<-tryCatch(withCallingHandlers(summarize_with_margins(remote,total=sum(v),share=share_of_total(total),.grouping=rollup(g)),condition=function(cnd) {delivered[[length(delivered)+1L]]<<-cnd}),error=identity,interrupt=identity)
  list(outcome=outcome,delivered=delivered)
 })
 # Ignore independently captured calls on the final public validation refusal;
 # compare class/text as well as every originally delivered leaf identity.
 equiv<-identical(class(runs[[1]]$outcome),class(runs[[2]]$outcome)) && identical(conditionMessage(runs[[1]]$outcome),conditionMessage(runs[[2]]$outcome)) && identical(runs[[1]]$delivered,runs[[2]]$delivered)
 rows<-last_sent_queries(FALSE,TRUE)
 cat('classes=',paste(classes,collapse=','),' terminator=',terminator,' equal=',equiv,' status=',paste(rows$status,collapse=','),' handling=',paste(unlist(lapply(rows$conditions,`[[`,'handling')),collapse=','),'\n',sep='')
 stopifnot(equiv)
 invisible(list(runs=runs,rows=rows))
}
for (warning_level in c(-1L, 0L, 1L, 2L)) {
 options(warn = warning_level)
 cat('warning_level=', warning_level, '\n', sep='')
 for(classes in list(c('warning','interrupt','condition'),c('interrupt','warning','condition'),c('warning','error','interrupt','condition'),c('interrupt','error','warning','condition'))) for(t in c('stop','signal','warning')) probe(classes,t)
}

cat('Compound-condition off/on pairs: 48; failures: 0\n')
}
main()
