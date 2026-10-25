# Recorded investigation: 2026-10-25, c62ecdb01d31ca5c2c9ab8baed0dda970dd844e5.
# Run in a fresh R process from the repository root. This is an investigation
# harness, not a package test or implementation. See manifest.txt for limits.

# This reuses driver-boundary doubles; it does not connect to a server.
pkgload::load_all('.', quiet=TRUE)
library(dplyr)
library(testthat)
source('tests/testthat/helper-share-dialect-verdicts.R')
skip_if_suggest_absent <- function(...) invisible(NULL)
parsed <- parse('tests/testthat/test-postgres-share-probe.R')
for(expr in parsed) if(is.call(expr) && identical(expr[[1L]],as.name('<-')) && as.character(expr[[2L]]) %in% c('postgres_probe_fixture','postgres_probe_summary')) eval(expr)

run <- function(audited, failure, nth, kind, warn2=FALSE) {
 options(marginplyr.audit_sql=audited, warn=if(warn2) 2L else 0L)
 fixture <- postgres_probe_fixture()
 execute <- DBI::dbExecute
 calls <- 0L
 leaf <- switch(kind,
  error=errorCondition('injected control error',class='control_failure'),
  warning=warningCondition('injected control warning',class='control_warning'),
  interrupt=structure(list(message='injected control interrupt',call=NULL),class=c('control_interrupt','interrupt','condition'))
 )
 testthat::local_mocked_bindings(dbExecute=function(conn,statement,...) {
   if(startsWith(statement,failure)) {
     calls <<- calls+1L
     if(calls==nth) {
       if(kind=='warning') warning(leaf) else stop(leaf)
     }
   }
   execute(conn,statement,...)
 },.package='DBI')
 delivered <- list()
 result <- tryCatch(withCallingHandlers(postgres_probe_summary(fixture$source),warning=function(cnd) {
   delivered[[length(delivered)+1L]] <<- list(class=class(cnd),message=conditionMessage(cnd))
   if(!warn2) invokeRestart('muffleWarning')
 }),error=identity,interrupt=identity)
 rows <- if(audited) last_sent_queries(FALSE,TRUE) else NULL
 normalize <- function(x) gsub('marginplyr_share_[[:alnum:]]+','marginplyr_share_ID',x)
 list(result=if(inherits(result,'condition')) list(class=class(result),message=conditionMessage(result)) else class(result),warnings=delivered,
      statements=normalize(fixture$statements),rows=rows)
}
cases <- expand.grid(target=c('savepoint','rollback_before_control','rollback_cleanup','release'),kind=c('error','warning','interrupt'),warn2=c(FALSE,TRUE),stringsAsFactors=FALSE)
for(i in seq_len(nrow(cases))) {
 c <- cases[i,]
 failure <- switch(c$target,savepoint='SAVEPOINT',rollback_before_control='ROLLBACK',rollback_cleanup='ROLLBACK',release='RELEASE')
 nth <- if(c$target=='rollback_cleanup') 2L else 1L
 off <- run(FALSE,failure,nth,c$kind,c$warn2)
 on <- run(TRUE,failure,nth,c$kind,c$warn2)
 eq <- identical(off[1:3],on[1:3])
 cat(c$target,c$kind,'WARN2',c$warn2,'BEHAVIOR_IDENTICAL',eq,'\n')
 print(on$rows[c('purpose','status')])
 if(!eq) {print(off[1:3]); print(on[1:3])}
}
