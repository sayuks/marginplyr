args <- commandArgs(TRUE)
root <- if (length(args) > 0L && !startsWith(args[[1L]], '--')) args[[1L]] else '.'
script_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
evidence_dir <- if (length(script_arg)) dirname(normalizePath(sub('^--file=', '', script_arg[[1L]]))) else getwd()
pkgload::load_all(root, quiet = TRUE)
x <- tibble::tibble(g=c('a','b'), v=1:2)
for (generic in c('format','unique','sort','as.character','as.vector','length','is.na','is.numeric','names')) {
 for (shape in c('matrix','character','numeric','integer','logical','array')) {
  env <- rlang::env(.parent = globalenv(), x=x)
  env$object <- switch(shape,matrix=matrix(1),array=array(1,c(1,1,1)),character='name',numeric=1,integer=1L,logical=TRUE)
  env$calls <- 0L
  method <- eval(quote(function(x, ...) {calls <<- calls + 1L; names(tidyselect::peek_data())[vapply(tidyselect::peek_data(),is.character,logical(1))]}), env)
  assign(paste0(generic,'.',shape),method,envir=env)
  expr <- substitute(tidyselect::all_of(G(object)),list(G=as.name(generic)))
  direct <- tryCatch(names(tidyselect::eval_select(rlang::new_quosure(expr,env),x)),error=function(e)paste0('ERROR: ',conditionMessage(e)))
  direct_calls <- env$calls
  env$calls <- 0L
  actual <- tryCatch(inspect_grouping(x,.by=!!rlang::new_quosure(expr,env),.format='list')$fixed[[1]],error=function(e)paste0('ERROR: ',conditionMessage(e)))
  out <- data.frame(generic=generic,shape=shape,is_object=is.object(env$object),method_called=direct_calls,expected=paste(direct,collapse=','),actual=paste(actual,collapse=','),actual_calls=env$calls,match=identical(direct,actual))
  if(!exists('results')) results <- out else results <- rbind(results,out)
 }
}
write.csv(results,file.path(evidence_dir, 'generic-dispatch-sweep.csv'),row.names=FALSE)
print(aggregate(cbind(cases = rep(1L, nrow(results)), failures = as.integer(!results$match)) ~ generic, data = results, FUN = sum))
cat('CASES', nrow(results), 'FAILURES', sum(!results$match), 'PASS', sum(results$match), '\n')
