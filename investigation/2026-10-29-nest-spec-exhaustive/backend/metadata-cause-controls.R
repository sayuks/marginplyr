# Investigated: 2026-10-29 against ee4321789ca60b6259c53df2852f1574aaede584.
pkgload::load_all('.',quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
observe<-function(f){w<-character();v<-tryCatch(withCallingHandlers(f(),warning=function(c){w<<-c(w,conditionMessage(c));invokeRestart('muffleWarning')}),error=identity);list(v=v,w=w)}
rewrite<-getFromNamespace('zero_row_dtplyr_proxy_input','marginplyr');fields<-getFromNamespace('dtplyr_step_input_fields','marginplyr');sources<-getFromNamespace('dt_sources','dtplyr');selfref<-getFromNamespace('selfrefok','data.table')
as_dt<-getS3method('as.data.table','dtplyr_step',envir=asNamespace('data.table'))
# An experimental schema-only graph that retains the existing cache and its
# already-safe arithmetic expression; this is evidence rather than a repair.
keep_cache<-function(step){inputs<-fields(step);for(field in inputs)step[[field]]<-keep_cache(step[[field]]);if(!length(inputs))step[['parent']]<-head(step[['parent']],0L);step}
s<-data.table::data.table(g=1:2,v=3:4);old<-data.table::copy(s);r<-dtplyr::lazy_dt(s);l<-compute(mutate(r,new=v+1L));rr<-compute(mutate(r,new='a'));q<-inner_join(l,rr,by='g')
root_refs <- function(step) { ins <- fields(step); if(!length(ins))return(list(step[['parent']])); do.call(c,lapply(ins,function(field)root_refs(step[[field]]))) }
z<-rewrite(q);cat('isolated roots before dt_sources selfref:',vapply(root_refs(z),selfref,integer(1)),'\n');cat('dt_sources merged same-name root selfref:',vapply(sources(z),selfref,integer(1)),'\n')
for(kind in c('package-rewrite','retain-cache-locals','restore-copy-flag')){
 ans<-observe(function(){z<-if(kind=='retain-cache-locals')keep_cache(q)else rewrite(q);if(kind=='restore-copy-flag')z[['needs_copy']]<-TRUE;as_dt(head(z,0L))})
 stopifnot(!inherits(ans$v,'error'),identical(s,old));cat(kind,'warnings',length(ans$w),'new.y type',typeof(ans$v$new.y),'\n')
}
for(rows in c(2L,3L))for(shape in c('cached-shared','swapped-cached','uncached','distinct-root','cached-only','collected')){
 s<-data.table::data.table(g=seq_len(rows),v=seq_len(rows)+2L);old<-data.table::copy(s);r<-dtplyr::lazy_dt(s);l<-mutate(r,new=v+1L);rr<-mutate(r,new='a')
 a<-compute(l);b<-compute(rr)
 step<-switch(shape,'cached-shared'=inner_join(a,b,by='g'),'swapped-cached'=inner_join(b,a,by='g'),'uncached'=inner_join(l,rr,by='g'),'distinct-root'=inner_join(a,compute(mutate(dtplyr::lazy_dt(data.table::copy(s)),new='a')),by='g'),'cached-only'=b,'collected'=collect(inner_join(a,b,by='g')))
 real<-observe(function()collect(step));stopifnot(!inherits(real$v,'error'),!length(real$w))
 expected<-names(real$v)[vapply(real$v,is.character,logical(1))];p<-observe(function()inspect_grouping(step,.by=where(is.character),.format='list'));stopifnot(!inherits(p$v,'error'),identical(s,old))
 cat('rows',rows,'shape',shape,'expected char',paste(expected,collapse=','),'actual char',paste(p$v$fixed[[1L]],collapse=','),'warnings',length(p$w),'\n')
}
# No caller warning policy is changed permanently by this experiment.
strict<-local({old_options<-options(warn=2L);on.exit(options(old_options));
 s<-data.table::data.table(g='a',v=1L);r<-dtplyr::lazy_dt(s);a<-compute(mutate(r,new=v+1L));q<-union_all(a,a)
 consumers<-list(inspect=function()inspect_grouping(q,.by=where(is.character)),nest=function()nest_with_margins(q),nest_by=function()nest_by_with_margins(q))
 lapply(consumers,function(f)tryCatch(f(),error=identity))})
for(nm in names(strict)){e<-strict[[nm]];cat('warn=2',nm,'class',paste(class(e),collapse=','),'diagnostic',if(inherits(e,'error'))conditionMessage(e)else'success','\n')}
