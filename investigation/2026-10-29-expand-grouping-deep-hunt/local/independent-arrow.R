pkgload::load_all('.',quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
for (part in c('sid','..marginplyr_arrow_dimension_1','..marginplyr_sort_1')) {
  path <- tempfile(); dir.create(path)
  for (j in 1:2) { p <- file.path(path,paste0(part,'=',j)); dir.create(p);arrow::write_parquet(data.frame(g=10L-j,rid=j),file.path(p,'part.parquet')) }
  args <- list(sources=path,partitioning=arrow::hive_partition(!!!setNames(list(arrow::int32()),part)),factory_options=list(selector_ignore_prefixes=character()))
  full <- do.call(arrow::open_dataset,args)
  narrow <- do.call(arrow::open_dataset,c(args,list(schema=arrow::schema(g=arrow::int32(),rid=arrow::int32()))))
  check <- function(x) collect(expand_with_margins(x,.grouping=rollup(g),.margin_label=NULL,.id=if(part=='..marginplyr_sort_1')NULL else 'sid',.sort='last'))
  control <- check(select(full,g,rid)); actual <- check(narrow)
  src <- arrange(collect(narrow),rid); stopifnot(identical(src$g,c(9L,8L)))
  if(part=='sid') stopifnot(any(actual$sid != ifelse(is.na(actual$g),2L,1L)))
  if(part=='..marginplyr_arrow_dimension_1') stopifnot(all(actual$g%in%c(1L,2L)))
  if(part=='..marginplyr_sort_1') stopifnot(identical(actual$g,c(9L,NA_integer_,8L,NA_integer_)))
  stopifnot(identical(control$g,c(8L,9L,NA_integer_,NA_integer_)))
  cat('ARROW',part,'actual g:',actual$g,' actual sid:',if ('sid' %in% names(actual)) actual[['sid']] else NULL,'\n')
  unlink(path,recursive=TRUE)
}
