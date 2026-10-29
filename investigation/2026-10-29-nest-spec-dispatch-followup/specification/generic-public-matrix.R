args <- commandArgs(TRUE)
root <- if (length(args) > 0L && !startsWith(args[[1L]], '--')) args[[1L]] else '.'
script_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
evidence_dir <- if (length(script_arg)) dirname(normalizePath(sub('^--file=', '', script_arg[[1L]]))) else getwd()
pkgload::load_all(root, quiet = TRUE)
input <- tibble::tibble(region=c('east','west'), amount=c(1L,1L))
results <- list(); index<-0L
capture <- function(expr,env) tryCatch(eval(expr,env),error=function(cnd)cnd)
for(generic in c('format','unique','sort')) for(shape in c('matrix','array','character','numeric','integer','logical')) for(backend in c('local','dtplyr')) {
 env <- rlang::env(.parent=globalenv(), data=if(backend=='local')input else dtplyr::lazy_dt(input), calls=0L)
 env$selector <- switch(shape,matrix=matrix(1L),array=array(1L,c(1L,1L,1L)),character='selector',numeric=1,integer=1L,logical=TRUE)
 method <- eval(quote(function(x,...){calls <<- calls+1L; names(tidyselect::peek_data())[vapply(tidyselect::peek_data(),is.character,logical(1))]}),env)
 assign(paste0(generic,'.',shape),method,envir=env)
 selection <- substitute(tidyselect::all_of(G(selector)),list(G=as.name(generic)))
 q <- rlang::new_quosure(selection,env)
 direct <- names(tidyselect::eval_select(q,input));stopifnot(identical(direct,'region'))
 for(consumer in c('inspect_grouping','summarize_with_margins','nest_with_margins','nest_by_with_margins')) {
  env$calls <- 0L
  call <- rlang::call2(consumer,rlang::sym('data'),.by=q)
  if(consumer=='inspect_grouping')call$.format <- 'list'
  if(consumer=='summarize_with_margins')call$n <- quote(dplyr::n())
  value <- capture(call,env)
  if(inherits(value,'dtplyr_step'))value <- dplyr::collect(value)
  passed <- if(inherits(value,'error'))FALSE else if(consumer=='inspect_grouping')identical(value$fixed[[1L]],direct)else nrow(value)==2L && 'region' %in% names(value)
  index<-index+1L;results[[index]]<-data.frame(generic=generic,shape=shape,backend=backend,role='fixed',consumer=consumer,passed=passed,calls=env$calls,outcome=if(inherits(value,'error'))conditionMessage(value)else paste(names(value),collapse=','))
 }
 for(constructor in c('grouping_set','grouping_sets','rollup','cube','grouping_spec')) {
  env$calls <- 0L
  spec <- rlang::call2(constructor,q)
  call <- rlang::call2('inspect_grouping',rlang::sym('data'),.grouping=spec,.format='list')
  value <- capture(call,env)
  passed <- !inherits(value,'error')&&identical(value$included[[1L]],direct)
  index<-index+1L;results[[index]]<-data.frame(generic=generic,shape=shape,backend=backend,role='dimension',consumer=constructor,passed=passed,calls=env$calls,outcome=if(inherits(value,'error'))conditionMessage(value)else paste(value$included[[1L]],collapse=','))
 }
}
results <- do.call(rbind,results)
write.csv(results,file.path(evidence_dir, 'generic-public-matrix.csv'),row.names=FALSE)
print(aggregate(cbind(cases=rep(1L,nrow(results)),failures=as.integer(!results$passed))~role+consumer+backend,data=results,FUN=sum))
cat('CASES',nrow(results),'FAILURES',sum(!results$passed),'PASS',sum(results$passed),'\n')
