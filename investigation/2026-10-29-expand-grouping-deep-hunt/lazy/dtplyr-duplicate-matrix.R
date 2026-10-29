# Investigated 2026-10-29 against ee43217 main; no package edits.
suppressPackageStartupMessages(library(dplyr))
dir.create('/tmp/lazy-deep-hunt', showWarnings = FALSE)
pkgload::load_all('.',quiet=TRUE)
observe<-function(f){warnings<-character();value<-tryCatch(withCallingHandlers(f(),warning=function(cnd){warnings<<-c(warnings,conditionMessage(cnd));invokeRestart('muffleWarning')}),error=identity);list(value=value,warnings=warnings)}
normal<-function(x){if(inherits(x,'dtplyr_step'))x<-collect(x);if(inherits(x,'error'))return(x);x<-as.data.frame(x);x<-x[do.call(order,c(lapply(x,as.character),list(na.last=TRUE))),,drop=FALSE];rownames(x)<-NULL;x}
rows<-list();i<-0L
for(n in c(0L,1L,3L))for(derivation in c('factor','row_type','callback')){
source<-data.table::data.table(g=seq_len(n)+10L,v=seq_len(n));before<-data.table::copy(source);q<-dtplyr::lazy_dt(source)
seen<-integer();probe<-function(x){seen<<-c(seen,length(x));factor(x)}
input<-switch(derivation,factor=mutate(q,g=v,g=factor(g)),row_type=mutate(q,g=v,g=if(n()>0L)rep('text',n())else integer()),callback=mutate(q,g=v,g=probe(g)))
actual_input<-collect(input);stopifnot(identical(source,before));seen<-integer()
# Controls: an ordinary uncollected backend mutation runs no callback, and a
# single unsafe assignment is refused without invoking it.
ordinary<-mutate(input,z=1L);stopifnot(!length(seen));single<-mutate(q,g=probe(v));single_obs<-observe(function()expand_with_margins(single,.grouping=rollup(g)))
stopifnot(inherits(single_obs$value,'marginplyr_error'),!length(seen),!length(single_obs$warnings))
# Name-only inspection requires no typed proxy and remains valid.
stopifnot(identical(inspect_grouping(input,.grouping=rollup(g)),inspect_grouping(actual_input,.grouping=rollup(g))),!length(seen))
consumers<-list(typed_numeric=function(x)inspect_grouping(x,.grouping=grouping_set(where(is.numeric)),.format='list'),typed_character=function(x)inspect_grouping(x,.grouping=grouping_set(where(is.character)),.format='list'),typed_levels=function(x)inspect_grouping(x,.grouping=grouping_set(where(function(x)identical(levels(x),as.character(seq_len(n))))),.format='list'),fixed_typed=function(x)inspect_grouping(x,.by=where(is.character),.grouping=grouping_set(v),.format='list'))
for(consumer in names(consumers)){
seen<-integer();got<-observe(function()consumers[[consumer]](input));callback_sizes<-seen;expected<-observe(function()consumers[[consumer]](actual_input));same<-identical(got$value,expected$value)
i<-i+1L;rows[[i]]<-data.frame(n,derivation,consumer,label='',sort='',status=if(inherits(got$value,'error'))'error'else if(!same)'plan_difference'else'pass',callback_sizes=paste(callback_sizes,collapse=','),warnings=length(got$warnings),message=if(inherits(got$value,'error'))conditionMessage(got$value)else'',source_unchanged=identical(source,before))
}
for(label in c('null','text'))for(sort in c('none','first','last')){
lab<-if(label=='null')NULL else 'Total';seen<-integer();got<-observe(function()collect(expand_with_margins(input,.grouping=rollup(g),.margin_label=lab,.sort=sort,.id='sid')));callback_sizes<-seen;expected<-observe(function()expand_with_margins(actual_input,.grouping=rollup(g),.margin_label=lab,.sort=sort,.id='sid'))
status<-if(inherits(got$value,'error'))'error'else if(!identical(normal(got$value),normal(expected$value)))'value_or_type_difference'else'pass'
i<-i+1L;rows[[i]]<-data.frame(n,derivation,consumer='expand',label,sort,status,callback_sizes=paste(callback_sizes,collapse=','),warnings=length(got$warnings),message=if(inherits(got$value,'error'))conditionMessage(got$value)else'',source_unchanged=identical(source,before))
}
stopifnot(identical(source,before))
}
results<-do.call(rbind,rows);write.csv(results,'/tmp/lazy-deep-hunt/dtplyr-duplicate-results.csv',row.names=FALSE)
cat('CASES',nrow(results),'\n');print(table(results$status));print(with(results,table(derivation,n,status)));stopifnot(all(results$source_unchanged))
