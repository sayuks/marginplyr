Sys.setenv(TESTTHAT_PARALLEL='false')
a <- commandArgs(TRUE)
assert <- '--assert' %in% a
a <- setdiff(a,'--assert')
pkgload::load_all(if(length(a)) a[[1]] else '.',quiet=TRUE)
targets <- c('base::min','base::seq_len','vctrs::vec_slice','base::list','data.table::as.data.table')
results <- list()
for(target in targets) for(verb_name in c('nest_with_margins','nest_by_with_margins')) {
 root <- new.env(parent=globalenv())
 root$source <- data.frame(district=c('north','south'),weight=c(41L,67L))
 input <- evalq(dtplyr::lazy_dt(source),root)
 verb <- get(verb_name)
 query <- nest_with_margins(input,.grouping=grouping_set(district),.margin_label=NULL)
 expected <- if(verb_name=='nest_with_margins')dplyr::collect(query) else verb(input,.grouping=grouping_set(district),.margin_label=NULL)
 before <- serialize(root$source,NULL)
 root$selected <- target
 root$calls <- character()
 root$`::` <- evalq(function(pkg,name) {
  package <- as.character(substitute(pkg)); member <- as.character(substitute(name))
  qualified <- paste(package,member,sep='::')
  original <- getExportedValue(package,member)
  if(!identical(qualified,selected))return(original)
  calls <<- c(calls,qualified)
  switch(qualified,
   'base::min'=function(...)0L,
   'base::seq_len'=function(length.out)integer(),
   'vctrs::vec_slice'=function(x,i,...) {if(is.character(x))rep('SHADOW',length(i))else getExportedValue('vctrs','vec_slice')(x,i,...)},
   'base::list'=function(...) {x<-getExportedValue('base','list')(...);if('weight'%in%names(x))x$weight<-rep(-311L,length(x$weight));x},
   'data.table::as.data.table'=function(x,...) {if('weight'%in%names(x))x$weight<-rep(-311L,length(x$weight));getExportedValue('data.table','as.data.table')(x,...)}
  )
 },root)
 observed <- tryCatch(if(verb_name=='nest_with_margins')dplyr::collect(query)else verb(input,.grouping=grouping_set(district),.margin_label=NULL),error=identity)
 affected_calls <- length(root$calls)
 root$calls <- character()
 native <- dplyr::collect(input)
 native_nest <- dplyr::collect(tidyr::nest(input,data=weight))
 native_count <- dplyr::collect(dplyr::summarise(input,n=dplyr::n(),.by=district))
 control_calls <- length(root$calls)
 stopifnot(identical(native$weight,c(41L,67L)),identical(unlist(lapply(native_nest$data,`[[`,'weight')),c(41L,67L)),identical(native_count$n,c(1L,1L)),control_calls==0L,identical(before,serialize(root$source,NULL)))
 rm('::',envir=root)
 restored <- if(verb_name=='nest_with_margins')dplyr::collect(query)else verb(input,.grouping=grouping_set(district),.margin_label=NULL)
 stopifnot(identical(restored,expected))
 matched <- identical(observed,expected)
 values <- if(inherits(observed,'error'))conditionMessage(observed) else paste(paste(observed$district,collapse=','),paste(unlist(lapply(observed$data,`[[`,'weight')),collapse=','),sep=' | ')
 results[[length(results)+1L]] <- data.frame(target,verb=verb_name,affected_calls,control_calls,matched,values)
 stopifnot(affected_calls>0L,!matched)
}
r <- do.call(rbind,results)
print(r,row.names=FALSE)
if(length(a)>1L)write.csv(r,file.path(a[[2]],'namespace-independent.csv'),row.names=FALSE)
cat('All 10 selective namespace boundaries changed package-generated Nest results; native and restored controls passed.\n')
if(assert)stopifnot(all(r$matched))
