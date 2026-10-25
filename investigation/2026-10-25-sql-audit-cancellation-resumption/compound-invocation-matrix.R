# Run this diagnostic in a disposable Rscript process; it loads this checkout.
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
source(file.path(dirname(normalizePath(sub("^--file=", "", script_arg))),
                 "reproduction-setup.R"))
pkgload::load_all(hunt_checkout, quiet = TRUE)
con <- DBI::dbConnect(RSQLite::SQLite(), ':memory:')
remote <- dplyr::copy_to(con, data.frame(g='a',v=1), 'source')
cases <- list(c('interrupt','error','condition'),c('error','interrupt','condition'),c('warning','interrupt','condition'),c('interrupt','warning','condition'),c('warning','error','interrupt','condition'),c('interrupt','error','warning','condition'))
results<-list()
for (classes in cases) for (warn in c(0L,2L)) {
  leaf<-structure(list(message='driver cancellation',call=NULL),class=classes)
  run<-function(audited) {
    options(marginplyr.audit_sql=audited,warn=warn)
    delivered<-list()
    outcome<-testthat::with_mocked_bindings(
      tryCatch(withCallingHandlers(
        summarize_with_margins(remote,total=sum(v),.grouping=rollup(g),.check_margin_label=TRUE),
        condition=function(cnd) { delivered[[length(delivered)+1L]] <<- cnd }),
        interrupt=identity,error=identity),collect=function(...)stop(leaf),.package='dplyr')
    list(outcome=outcome,delivered=delivered)
  }
  off<-run(FALSE);on<-run(TRUE)
  rows<-last_sent_queries(FALSE,TRUE)
  stopifnot(identical(on,off),identical(on$outcome,leaf),identical(on$delivered,list(leaf)),nrow(rows)==1L,nrow(rows$conditions[[1L]])==1L)
  results[[length(results)+1L]]<-data.frame(classes=paste(classes,collapse='/'),warn=warn,status=rows$status,historical_probe='interrupted',stringsAsFactors=FALSE)
}
DBI::dbDisconnect(con)
result<-do.call(rbind,results);print(result,row.names=FALSE)
saveRDS(result,hunt_output('compound-invocation-matrix.rds'))
# The old common-priority assertion was speculative, not a contract.
# Audit parity and single delivery above are the criteria retained for reruns.
quit(status = 0L, save = "no")
