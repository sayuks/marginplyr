# Controlled fault injection, not proof of ordinary native interrupt frequency.
# An active setter stores the real invoking transition then raises one interrupt.
# No repository file or backend invocation is altered.
pkgload::load_all('.',quiet=TRUE)
options(marginplyr.audit_sql=TRUE)
mode <- commandArgs(trailingOnly=TRUE)
if(!length(mode)) mode <- 'entry_after_store'
stopifnot(mode %in% c('entry_after_store','exit_before_store'))
con <- DBI::dbConnect(RSQLite::SQLite(),':memory:')
remote <- dplyr::copy_to(con,data.frame(g='a',v=1),'status_scope_source')
leaf <- structure(list(message='controlled interrupt after invoking store',call=NULL),
                  class=c('controlled_status_interrupt','interrupt','condition'))
original <- dbplyr::sql_render
installed <- FALSE
run <- function() {
  testthat::local_mocked_bindings(sql_render=function(query,...) {
    answer <- original(query,...)
    if (!installed) {
      installed <<- TRUE
      record <- getFromNamespace('current_sent_query_scope', 'marginplyr')()$record
      store <- new.env(parent=emptyenv()); store$value <- record$rows; store$armed <- TRUE
      rm('rows',envir=record)
      makeActiveBinding('rows',function(value) {
        if(missing(value)) return(store$value)
        terminal <- any(store$value$status=='invoking') && any(value$status=='returned')
        if(store$armed && mode=='exit_before_store' && terminal) {
          store$armed <- FALSE
          stop(leaf)
        }
        store$value <- value
        if(store$armed && mode=='entry_after_store' && any(value$status=='invoking')) {
          store$armed <- FALSE
          stop(leaf)
        }
        invisible(value)
      },record)
    }
    answer
  }, .package='dbplyr')
  tryCatch(summarize_with_margins(remote,total=sum(v,na.rm=TRUE),.grouping=rollup(g),.check_margin_label=TRUE),interrupt=identity)
}
caught <- run()
rows <- last_sent_queries(FALSE,TRUE)
stopifnot(identical(caught,leaf),nrow(rows)==1L)
cat('Injection mode:',mode,'\n')
cat('Caught original interrupt:',identical(caught,leaf),'\n')
cat('Post-unwind status:',rows$status,'; conditions:',nrow(rows$conditions[[1L]]),'\n')
DBI::dbDisconnect(con)
if(any(rows$status=='invoking')) quit(status=1L)
