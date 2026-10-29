# Investigated: 2026-10-29 against ee4321789ca60b6259c53df2852f1574aaede584.
# Run from the repository root. --assert requires desired warning-free behavior.
pkgload::load_all('.',quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
observe<-function(f){warnings<-list();value<-tryCatch(withCallingHandlers(f(),warning=function(w){warnings[[length(warnings)+1L]]<<-list(class=class(w),message=conditionMessage(w),call=conditionCall(w));invokeRestart('muffleWarning')}),error=identity);list(value=value,warnings=warnings)}
source<-data.table::data.table(g=c('a','b'),v=1:2);before<-data.table::copy(source)
root<-dtplyr::lazy_dt(source)
mutated<-mutate(root,new=v+1L);cached<-compute(mutated);q<-union_all(cached,cached)
ctrl<-observe(function()collect(q));stopifnot(!inherits(ctrl$value,'error'),!length(ctrl$warnings),nrow(ctrl$value)==4L)
raw<-observe(function()inspect_grouping(q,.grouping=rollup(g)));stopifnot(!length(raw$warnings))
typed<-observe(function()inspect_grouping(q,.grouping=rollup(where(is.character))))
nest_build<-observe(function()nest_with_margins(q,.grouping=rollup(g)))
nest_collect<-observe(function()collect(nest_build$value))
by<-observe(function()nest_by_with_margins(q,.grouping=rollup(g)))
stopifnot(!inherits(typed$value,'error'),!inherits(nest_collect$value,'error'),!inherits(by$value,'error'),!length(nest_collect$warnings),identical(source,before))
cat('ordinary collect warnings:',length(ctrl$warnings),'\nname-only inspection warnings:',length(raw$warnings),'\ntyped inspection warnings:',length(typed$warnings),'\nnesting construction warnings:',length(nest_build$warnings),'\nnesting collection warnings:',length(nest_collect$warnings),'\nrowwise nesting warnings:',length(by$warnings),'\nsource unchanged:',identical(source,before),'\n')
if(length(typed$warnings)){cat('warning class:',paste(typed$warnings[[1L]]$class,collapse=', '),'\nwarning call:',deparse(typed$warnings[[1L]]$call),'\nwarning message:',typed$warnings[[1L]]$message,'\n')}
proxy_roots<-list()
trace('dt_eval',where=asNamespace('dtplyr'),print=FALSE,tracer=quote({proxy_roots[[length(proxy_roots)+1L]]<<-vapply(utils::getFromNamespace('dt_sources','dtplyr')(x),nrow,integer(1))}))
invisible(observe(function()inspect_grouping(q,.grouping=rollup(where(is.character)))))
untrace('dt_eval',where=asNamespace('dtplyr'))
stopifnot(length(proxy_roots)>0L,all(unlist(proxy_roots)==0L));cat('traced metadata root sizes:',paste(unlist(proxy_roots),collapse=','),'\n')
cat('schema-only generated query:\n')
proxy_fn<-getFromNamespace('zero_row_dtplyr_proxy_input','marginplyr');show_query(head(proxy_fn(q),0L))
for(shape in c('uncached-union','cached-only','separate-caches-same-root','separate-roots','cache-before-mutate','cached-set','self-join')){
 alternate<-switch(shape,'uncached-union'=union_all(mutated,mutated),'cached-only'=cached,'separate-caches-same-root'=union_all(compute(mutated),compute(mutated)),'separate-roots'=union_all(cached,compute(mutate(dtplyr::lazy_dt(data.table::copy(source)),new=v+1L))),'cache-before-mutate'=union_all(mutate(compute(root),new=v+1L),mutate(compute(root),new=v+1L)),'cached-set'=union(cached,cached),'self-join'=left_join(cached,cached,by='g'))
 control<-observe(function()collect(alternate));got<-observe(function()inspect_grouping(alternate,.grouping=rollup(where(is.character))))
 stopifnot(!inherits(control$value,'error'),!inherits(got$value,'error'),!length(control$warnings),identical(source,before))
 cat(shape,'ordinary warnings',length(control$warnings),'typed metadata warnings',length(got$warnings),'\n')
}
if('--assert'%in%commandArgs(trailingOnly=TRUE))stopifnot(!length(typed$warnings),!length(nest_build$warnings),!length(by$warnings))
