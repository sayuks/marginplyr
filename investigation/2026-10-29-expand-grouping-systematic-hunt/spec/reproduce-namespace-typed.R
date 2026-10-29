args <- commandArgs(TRUE)
repo <- if(length(args) && args[[1L]] != '--assert') args[[1L]] else '.'
pkgload::load_all(repo,quiet=TRUE)
data <- tibble::tibble(region=c('east','west'),amount=1:2)
observed <- list()
for(kind in c('leaf','argument')) {
 env <- rlang::env(globalenv(),calls=0L)
 env$typed <- function(...) {
  env$calls <- env$calls+1L
  names(tidyselect::peek_data())[vapply(tidyselect::peek_data(),is.character,logical(1))]
 }
 env[['::']] <- function(package,name) env$typed
 expr <- if(kind=='leaf') quote(dplyr::all_of('amount')) else quote(all_of(base::names('amount')))
 q <- rlang::new_quosure(expr,env)
 expected <- names(tidyselect::eval_select(q,data))
 env$calls <- 0L
 actual <- inspect_grouping(data,.by=!!q,.format='list')$fixed[[1L]]
 cat(kind,': expected=',paste(expected,collapse=','),' actual=',paste(actual,collapse=','),' calls=',env$calls,'\n',sep='')
 observed[[kind]] <- identical(expected,actual)
}
if('--assert' %in% args) stopifnot(all(unlist(observed)))
