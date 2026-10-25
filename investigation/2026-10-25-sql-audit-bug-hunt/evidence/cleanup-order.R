# Recorded investigation: 2026-10-25, c62ecdb01d31ca5c2c9ab8baed0dda970dd844e5.
# Run in a fresh R process from the repository root. This is an investigation
# harness, not a package test or implementation. See manifest.txt for limits.

# Sixteen driver-operation/unwind combinations use a mocked dplyr collect().
pkgload::load_all('.',quiet=TRUE)
con<-DBI::dbConnect(RSQLite::SQLite(),':memory:')
remote<-dplyr::copy_to(con,data.frame(g='a',v=1),'src')
check<-function() summarize_with_margins(remote,total=sum(v),.grouping=rollup(g),.check_margin_label=TRUE)
throw<-function(kind, label) switch(kind,error=stop(label),warning=warning(label,call.=FALSE),interrupt=stop(structure(list(message=label,call=NULL),class=c('interrupt','condition'))), return=invisible(1))
run<-function(audit,body,cleanup) {
  options(marginplyr.audit_sql=audit)
  testthat::local_mocked_bindings(collect=function(...) {
    on.exit(throw(cleanup,'cleanup'))
    throw(body,'body')
    data.frame(g=0)
  },.package='dplyr')
  delivered<-character()
  out<-tryCatch(withCallingHandlers(check(),warning=function(cnd) {delivered<<-c(delivered,conditionMessage(cnd));invokeRestart('muffleWarning')}),error=identity,interrupt=identity)
  list(class=class(out),message=if(inherits(out,'condition'))conditionMessage(out) else NULL,warnings=delivered)
}
for(body in c('return','warning','error','interrupt')) for(cleanup in c('return','warning','error','interrupt')) {
  off<-run(FALSE,body,cleanup);on<-run(TRUE,body,cleanup)
  cat('BODY', body, 'CLEANUP', cleanup, 'BEHAVIOR_IDENTICAL', identical(off,on), '\n')
  if(!identical(off,on)) {cat('DIFF',body,cleanup,'\n');print(off);print(on)}
  rows<-last_sent_queries(FALSE,TRUE)
  cat(body,'+',cleanup,'status=',rows$status,'conditions=',paste(rows$conditions[[1]]$message,collapse=','),'\n')
}
DBI::dbDisconnect(con)
