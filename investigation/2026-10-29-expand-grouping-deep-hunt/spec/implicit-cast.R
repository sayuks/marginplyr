args <- commandArgs(TRUE)
pkgload::load_all(if (length(args)) args[[1]] else '.', quiet=TRUE)
data <- data.frame(region=c('E','W'),amount=1:2)
observe <- function(fn)tryCatch(suppressWarnings(fn()),error=identity)
cases <- 0L
for (kind in c('matrix','array','numeric','integer','character','logical')) {
  value <- switch(kind,matrix=matrix(1L),array=array(1L,c(1L,1L,1L)),
                  numeric=1,integer=1L,character='region',logical=TRUE)
  method <- paste0('vec_cast.integer.',kind)
  calls <- 0L
  assign(method,function(x,to,...) {
    calls <<- calls+1L
    which(vapply(tidyselect::peek_data(),is.character,logical(1)))
  },.GlobalEnv)
  q <- rlang::new_quosure(value,environment())
  direct <- observe(function()tidyselect::eval_select(q,data))
  for (consumer in c('fixed','set')) {
    actual <- observe(function() {
      if (consumer=='fixed')inspect_grouping(data,.by=!!q,.format='list') else
        inspect_grouping(data,.grouping=grouping_set(!!q),.format='list')
    })
    if (inherits(direct,'error')) {
      stopifnot(inherits(actual,'error'),identical(class(actual),class(direct)))
    } else {
      answer <- if(consumer=='fixed')actual$fixed[[1L]] else actual$included[[1L]]
      stopifnot(identical(answer,names(direct)))
    }
    cases <- cases+1L
  }
  stopifnot(calls==0L)
  cat(kind,'is.object:',is.object(value),'cast calls:',calls,
      'direct:',if(inherits(direct,'error'))class(direct)[[1L]] else 'selected region','\n')
  rm(list=method,envir=.GlobalEnv)
}
cat('PASS excluded implicit-cast lead:',cases,'matching package comparisons; no custom cast invoked\n')
