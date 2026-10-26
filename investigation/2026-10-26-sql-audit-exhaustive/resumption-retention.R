args <- commandArgs(TRUE)
if (length(args) != 1L || file.exists(args[[1L]])) stop('Supply one fresh RDS output path.')
pkgload::load_all('.', quiet = TRUE)
options(marginplyr.audit_sql = TRUE)
con <- DBI::dbConnect(RSQLite::SQLite(), ':memory:')
remote <- dplyr::copy_to(con, data.frame(g = 'a', v = 1), 'retention_source')
child <- dbplyr::lazy_frame(h = 'b', z = 2, .name = 'nested_source')
create <- function(prefix, ending, depth) {
 refs <- list()
 nested <- function(level) {
   holder <- new.env(parent=emptyenv())
   refs[[length(refs)+1L]] <<- rlang::new_weakref(holder)
   if (level == 0L) return(invisible(NULL))
   grouping <- function() {
     if (level > 1L) nested(level - 1L)
     rollup(h)
   }
   before <- last_sent_queries(FALSE,TRUE)
   summarize_with_margins(child,total=sum(z,na.rm=TRUE),.grouping=grouping())
   stopifnot(identical(last_sent_queries(FALSE,TRUE),before))
 }
 testthat::local_mocked_bindings(collect=function(...) {
   frame <- environment()
   payload <- new.env(parent=emptyenv())
   leaf <- structure(list(message='recoverable signal',call=NULL,payload=payload),class=c('retention_interrupt','interrupt','condition'))
   refs[[length(refs)+1L]] <<- rlang::new_weakref(frame)
   refs[[length(refs)+1L]] <<- rlang::new_weakref(payload)
   withRestarts({
     if(prefix=='native') rlang::interrupt() else signalCondition(leaf)
   }, resume_operation=function() NULL)
   # Record an independent view while the operation is active; holding that
   # tibble must neither expose subsequent stores nor retain this driver frame.
   held <- last_sent_queries(FALSE,TRUE)
   nested(depth)
   stopifnot(identical(last_sent_queries(FALSE,TRUE),held))
   switch(ending,
     error=stop('later driver error'),
     interrupt=stop(structure(list(message='later driver interrupt',call=NULL),class=c('interrupt','condition'))),
     escape=invokeRestart('escape_operation','escaped'))
   data.frame(g=0)
 },.package='dplyr')
 delivered <- list()
 result <- tryCatch(withRestarts(withCallingHandlers(
   summarize_with_margins(remote,total=sum(v,na.rm=TRUE),.grouping=rollup(g),.check_margin_label=TRUE),
   interrupt=function(cnd) {
     delivered[[length(delivered)+1L]] <<- list(classes=class(cnd),message=conditionMessage(cnd))
     if(!is.null(findRestart('resume_operation'))) invokeRestart('resume_operation')
   }
 ),escape_operation=identity),error=identity,interrupt=identity)
 rows <- last_sent_queries(FALSE,TRUE)
 expected <- switch(ending,normal='returned',error='errored',interrupt='interrupted',escape='unknown')
 if(!identical(rows$status,expected)) print(list(prefix=prefix,ending=ending,depth=depth,status=rows$status,result=result))
 stopifnot(identical(rows$status,expected),nrow(rows$conditions[[1]])==if(ending %in% c('error','interrupt')) 2L else 1L)
 list(refs=refs,status=rows$status,conditions=rows$conditions[[1]],expected=expected)
}
cases <- expand.grid(prefix=c('signal','native'),ending=c('normal','error','interrupt','escape'),depth=c(0L,1L,3L),stringsAsFactors=FALSE)
observations <- list(); all_refs <- list()
for(i in seq_len(nrow(cases))) {
 x <- as.list(cases[i,])
 result <- do.call(create,x)
 all_refs <- c(all_refs,result$refs)
 observations[[i]] <- list(case=x,status=result$status,conditions=result$conditions,weakrefs=length(result$refs))
 rm(result)
 for(j in seq_len(3L)) invisible(gc())
 stopifnot(all(vapply(all_refs,function(ref) is.null(rlang::wref_key(ref)),logical(1))))
}
saveRDS(list(cases=cases,observations=observations,weakrefs=length(all_refs)),args[[1L]])
cat('PASS',nrow(cases),'recovery/ending/nesting cases;',length(all_refs),'cleared weakrefs while latest record retained\n')
DBI::dbDisconnect(con)
