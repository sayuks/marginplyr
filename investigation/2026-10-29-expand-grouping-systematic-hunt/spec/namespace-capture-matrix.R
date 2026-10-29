args <- commandArgs(TRUE)
pkgload::load_all(if(length(args)) args[[1L]] else '.', quiet=TRUE)
output_dir <- Sys.getenv('MARGINPLYR_HUNT_OUTPUT', unset = file.path(tempdir(), 'marginplyr-expand-hunt-spec'))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
data <- tibble::tibble(region=c('east','west'), amount=1:2)
constructors <- c('grouping_set','grouping_sets','rollup','cube','grouping_spec')
wrap <- function(x,n) { for(i in seq_len(n)) x <- rlang::call2('(',x); x }
observations <- list()
for(backend in c('local','dtplyr')) for(constructor in constructors) for(head in c('bare','::',':::','get','index')) for(depth in 0:2) for(result in c('selection','spec')) for(consumer in c('inspect','expand')) {
 env <- rlang::env(globalenv(),calls=0L)
 env$choose <- function(...) {
  env$calls <- env$calls+1L
  column <- if(env$calls==1L) 'region' else 'amount'
  if(result=='selection') column else grouping_set(dplyr::all_of(column))
 }
 assign(constructor,env$choose,env)
 env[['::']] <- env[[':::']] <- function(package,name) env$choose
 env$fs <- list(env$choose)
 head_expr <- switch(head,bare=rlang::sym(constructor),
  '::'=rlang::call2('::',rlang::sym('marginplyr'),rlang::sym(constructor)),
  ':::'=rlang::call2(':::',rlang::sym('marginplyr'),rlang::sym(constructor)),
  get=rlang::call2('get',constructor), index=quote(fs[[1L]]))
 expr <- wrap(rlang::call2(wrap(head_expr,depth)),depth)
 q <- rlang::new_quosure(expr,env)
 s <- grouping_sets(!!q,grouping_set())
 input <- if(backend=='local') data else dtplyr::lazy_dt(data)
 get_result <- function(s) if(consumer=='inspect') inspect_grouping(input,.grouping=s,.format='list') else dplyr::collect(expand_with_margins(input,.grouping=s,.margin_label=NULL,.id='set'))
 expected <- get_result(grouping_sets(grouping_set(region),grouping_set()))
 env$calls <- 0L
 actual <- tryCatch(get_result(s),error=identity)
 recognized <- head %in% c('bare','::',':::')
 expected_rejection <- result=='spec' && !recognized
 ok <- if(expected_rejection) inherits(actual,'marginplyr_error') && grepl('constructor call',conditionMessage(actual),fixed=TRUE) else identical(actual,expected)
 if(expected_rejection) ok <- inherits(actual,'marginplyr_error') && grepl('Assign',conditionMessage(actual),fixed=TRUE)
 observations[[length(observations)+1L]] <- data.frame(backend,constructor,head,depth,result,consumer,calls=env$calls,match=ok,condition=if(inherits(actual,'error'))class(actual)[[1L]] else '',stringsAsFactors=FALSE)
}
out <- do.call(rbind,observations)
write.csv(out,file.path(output_dir,'namespace-capture-matrix.csv'),row.names=FALSE)
print(with(out,table(head,result,match)))
print(with(out,table(head,calls)))
cat('observations=',nrow(out),'\n',sep='')
control <- !(out$head %in% c('::',':::') & out$result=='selection')
stopifnot(all(out$match[control]),all(out$calls[control]==1L))
