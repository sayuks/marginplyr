# Run this diagnostic in a disposable Rscript process; it loads this checkout.
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
source(file.path(dirname(normalizePath(sub("^--file=", "", script_arg))),
                 "reproduction-setup.R"))
pkgload::load_all(hunt_checkout, quiet = TRUE)
con<-DBI::dbConnect(RSQLite::SQLite(),':memory:')
remote<-dplyr::copy_to(con,data.frame(g='a',v=1),'source')
leaf<-structure(list(message='resumable interruption',call=NULL),class=c('interrupt','condition'))
results<-list()
for (signal in c(FALSE,TRUE)) for (finish in c('return','restart','error','interrupt')) {
 run<-function(audited) {
  options(marginplyr.audit_sql=audited)
  testthat::with_mocked_bindings(
   tryCatch(withRestarts(withCallingHandlers(
    summarize_with_margins(remote,total=sum(v),.grouping=rollup(g),.check_margin_label=TRUE),
    interrupt=function(cnd) {if(identical(cnd,leaf))invokeRestart('resume_signal')}),
    leave_driver=function()'escaped'),error=function(cnd)class(cnd),interrupt=function(cnd)class(cnd)),
   collect=function(...) {
    if(signal)withRestarts(signalCondition(leaf),resume_signal=function()NULL)
    switch(finish,return=data.frame(g=0),restart=invokeRestart('leave_driver'),error=stop('later error'),interrupt=stop(structure(list(message='later interruption',call=NULL),class=c('later_interrupt','interrupt','condition'))))
   },.package='dplyr')
 }
 off<-run(FALSE);on<-run(TRUE)
 # Successful lazy query objects contain independently allocated environments.
 same<-if(finish=='return')identical(class(off),class(on)) else identical(off,on)
 stopifnot(same)
 rows<-last_sent_queries(FALSE,TRUE)
 expected<-switch(finish,return='returned',restart='unknown',error='errored',interrupt='interrupted')
 results[[length(results)+1L]]<-data.frame(signal=signal,finish=finish,status=rows$status,expected=expected)
}
DBI::dbDisconnect(con)
result<-do.call(rbind,results);print(result,row.names=FALSE)
saveRDS(result,hunt_output('resumed-interrupt-controls.rds'))
stopifnot(identical(which(result$status != result$expected), 6L),
          result$signal[[6L]], identical(result$finish[[6L]], "restart"),
          identical(result$status[[6L]], "interrupted"))
quit(status = as.integer(any(result$status != result$expected)), save = "no")
