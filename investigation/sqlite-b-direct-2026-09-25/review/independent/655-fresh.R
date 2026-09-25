args <- commandArgs(TRUE)
pkgload::load_all(args[[1L]], quiet=TRUE)
Sys.unsetenv('TESTTHAT')
audited <- args[[2L]]=='on'
explicit <- args[[3L]]=='explicit'
shape <- args[[4L]]
action <- args[[5L]]
options(warn=2,marginplyr.audit_sql=audited)
capture_error<-function(expr)tryCatch(list(value=expr),error=function(e)list(error=conditionMessage(e),class=class(e)))
run_case<-function() {
 con<-DBI::dbConnect(RSQLite::SQLite(),':memory:')
 on.exit(DBI::dbDisconnect(con))
 src<-dplyr::copy_to(con,data.frame(g=c('a','b','b'),v=c(1,NA_real_,2)),'source')
 before<-DBI::dbReadTable(con,'source')
 verbosity<-getOption('rlib_warning_verbosity')
 result<-capture_error(if(explicit) {
  summarize_with_margins(src,z=sum(v,na.rm=TRUE),.grouping=rollup(g),.margin_label=NULL,.sort=if(shape=='sorted')'last' else 'none',.id='set')
 } else {
  summarize_with_margins(src,z=sum(v),.grouping=rollup(g),.margin_label=NULL,.sort=if(shape=='sorted')'last' else 'none',.id='set')
 })
 record<-if(audited)last_sent_queries() else NULL
 if(is.null(result$error))result<-capture_error(if(action=='compute')dplyr::collect(dplyr::compute(result$value)) else dplyr::collect(result$value))
 list(result=result,record=record,verbosity_restored=identical(verbosity,getOption('rlib_warning_verbosity')),source_preserved=identical(before,DBI::dbReadTable(con,'source')))
}
saveRDS(run_case(),args[[6L]])
