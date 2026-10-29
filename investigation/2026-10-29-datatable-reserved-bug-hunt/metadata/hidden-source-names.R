# Public-API replay: a discarded source name re-enters typed metadata reads.
pkgload::load_all('.',quiet=TRUE)
out_dir <- 'investigation/2026-10-29-datatable-reserved-bug-hunt/metadata'
assert <- '--assert' %in% commandArgs(trailingOnly=TRUE)
attempt <- function(expr) tryCatch(list(value=force(expr)),
 error=function(e)list(error=conditionMessage(e),class=class(e)[[1L]]))
normalize <- function(x) {
 if(inherits(x,'dtplyr_step')) x <- dplyr::collect(x)
 x <- dplyr::ungroup(x)
 if('data' %in% names(x)) x$data <- lapply(x$data,tibble::as_tibble)
 x
}
operations <- list(
 inspect_named=function(x)inspect_grouping(x,.by=flag,.grouping=rollup(value)),
 inspect_typed_dimension=function(x)inspect_grouping(x,.by=flag,.grouping=rollup(where(is.integer))),
 inspect_typed_fixed=function(x)inspect_grouping(x,.by=where(is.logical),.grouping=rollup(value)),
 summarize=function(x)summarize_with_margins(x,total=sum(value),.by=flag,.grouping=rollup(value),.sort='last'),
 expand=function(x)expand_with_margins(x,.by=flag,.grouping=rollup(value),.sort='last'),
 nest=function(x)nest_with_margins(x,.by=flag,.grouping=rollup(value),.keep=TRUE,.sort='last'),
 nest_by=function(x)nest_by_with_margins(x,.by=flag,.grouping=rollup(value),.keep=TRUE,.sort='last'))
rows <- list()
for(column in c('..1','...','..2')) for(size in c(0L,1L,3L)) for(derive in c('drop','rename')) {
 source <- data.table::data.table(special=rep(c('a','b','a'),length.out=size),value=seq_len(size),flag=rep(TRUE,size))
 data.table::setnames(source,'special',column)
 before <- data.table::copy(source)
 root <- dtplyr::lazy_dt(source)
 step <- if(derive=='drop')dplyr::select(root,value,flag) else dplyr::rename(root,ordinary=dplyr::all_of(column))
 collected <- dplyr::collect(step)
 stopifnot(!any(names(collected) %in% c('..1','...','..2')))
 # Direct local selection from the original minimally repaired source is valid.
 local_drop <- dplyr::select(tibble::as_tibble(source,.name_repair='minimal'),value,flag)
 stopifnot(identical(local_drop,tibble::tibble(value=seq_len(size),flag=rep(TRUE,size))))
 for(operation in names(operations)) {
  run <- operations[[operation]]
  expected <- attempt(normalize(run(collected)))
  actual <- attempt(normalize(run(step)))
  stopifnot(is.null(expected$error))
  status <- if(!is.null(actual$error))'error' else if(!identical(actual$value,expected$value))'different' else 'pass'
  rows[[length(rows)+1L]] <- data.frame(column,size,derive,operation,status,
   error_class=if(is.null(actual$class))'' else actual$class,
   error=if(is.null(actual$error))'' else actual$error,
   source_changed=!identical(source,before))
 }
}
result <- do.call(rbind,rows)
write.csv(result,file.path(out_dir,'hidden-source-results.csv'),row.names=FALSE)
print(with(result,table(derive,operation,status)))
stopifnot(!any(result$source_changed))
cat('Cases:',nrow(result),'; all ordinary collected/local controls succeeded; source changes: 0\n')
cat('Observed failure classes:',paste(unique(result$error_class[result$status=='error']),collapse=', '),'\n')
if(assert && any(result$status!='pass')) stop('Discarded source names must not invalidate safe visible input schemas.',call.=FALSE)
