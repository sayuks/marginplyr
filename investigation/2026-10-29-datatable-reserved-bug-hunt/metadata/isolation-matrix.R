# A dated typed-inspection check for special-name inputs and shared graphs.
pkgload::load_all('.', quiet=TRUE)
out_dir <- 'investigation/2026-10-29-datatable-reserved-bug-hunt/metadata'
candidates <- c('.N','.I','.SD','.GRP','.NGRP','.BY','.EACHI','.NATURAL',
 '.ROW','.SDall','.xSD','.iSD','..value')
derivations <- c('filter','select_special','select_safe','rename_other',
 'rename_special','cache_rename','cache_arrange','shared_union')
record <- list()
for (column in candidates) for (size in c(0L,3L)) for (immutable in c(TRUE,FALSE)) {
 for (derive in derivations) {
  make <- function() {
   source <- data.table::data.table(special=rep(c('b','a','b'),length.out=size),
     value=seq_len(size),flag=rep(TRUE,size))
   data.table::setnames(source,'special',column)
   root <- dtplyr::lazy_dt(source,immutable=immutable)
   x <- dplyr::filter(root,probe(value))
   step <- switch(derive,filter=x,
    select_special=dplyr::select(x,dplyr::all_of(c(column,'value'))),
    select_safe=dplyr::select(x,value,flag),
    rename_other=dplyr::rename(x,amount=value),
    rename_special=dplyr::rename(x,ordinary=dplyr::all_of(column)),
    cache_rename=dplyr::rename(dplyr::compute(x),amount=value),
    cache_arrange=dplyr::arrange(dplyr::compute(x),value),
    shared_union={ cached <- dplyr::compute(x); dplyr::union_all(cached,cached) })
   list(source=source,step=step)
  }
  seen <- integer()
  probe <- function(x) { seen <<- c(seen,length(x)); x>0L }
  obj <- make()
  control <- tryCatch(list(value=dplyr::collect(obj$step)),error=function(e)list(error=conditionMessage(e)))
  obj <- make()
  before <- data.table::copy(obj$source)
  seen <- integer()
  for (predicate in list(is.character,is.integer)) {
   actual <- tryCatch(list(value=inspect_grouping(obj$step,
    .grouping=grouping_set(where(predicate)))),error=function(e)list(error=conditionMessage(e)))
   expected <- if(!is.null(control$error)) control else tryCatch(list(value=inspect_grouping(control$value,
    .grouping=grouping_set(where(predicate)))),error=function(e)list(error=conditionMessage(e)))
   status <- if(!is.null(control$error)) 'upstream-error'
    else if(!is.null(actual$error)) 'inspection-error'
    else if(!identical(actual$value,expected$value)) 'plan-mismatch' else 'pass'
   record[[length(record)+1L]] <- data.frame(column,size,immutable,derive,
    predicate=if(identical(predicate,is.character)) 'character' else 'integer',status,
    evaluated=length(seen)>0L,source_changed=!identical(obj$source,before),
    error=if(is.null(actual$error)) '' else actual$error)
  }
 }
 cat('Completed',column,size,immutable,'\n');flush.console()
}
result <- do.call(rbind,record)
write.csv(result,file.path(out_dir,'isolation-results.csv'),row.names=FALSE)
print(table(result$status)); print(subset(result,evaluated | source_changed | status %in% c('inspection-error','plan-mismatch')))
