pkgload::load_all('.',quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
checks<-0L;seen<-integer();probe<-function(x){seen<<-c(seen,length(x));if(!length(x))stop('empty predicate executed');x>0L}
for(left_mutable in c(FALSE,TRUE))for(right_mutable in c(FALSE,TRUE))for(shape in c('root','cache','cache-arrange','cache-rename','cache2','join','union','nested')){
 l<-data.table::data.table(g=c('a','b'),v=1:2);r<-data.table::data.table(g=c('a','c'),v=3:4);oldl<-data.table::copy(l);oldr<-data.table::copy(r)
 lq<-dtplyr::lazy_dt(l,immutable=!left_mutable);rq<-dtplyr::lazy_dt(r,immutable=!right_mutable)
 lq<-filter(lq,probe(v));rq<-filter(rq,probe(v));lc<-compute(lq);rc<-compute(rq)
 q<-switch(shape,root=lq,cache=lc,'cache-arrange'=arrange(lc,v),'cache-rename'=rename(lc,value=v),cache2=arrange(compute(lc),v),join=left_join(arrange(lc,v),rc,by='g'),union=union_all(lc,rc),nested=left_join(union_all(lc,rc),rename(compute(rc),w=v),by='g'))
 for(repeat_i in 1:2){seen<-integer();p<-inspect_grouping(q,.grouping=rollup(where(is.character)));stopifnot(identical(p$included,c('(g)','()')),length(seen)==0L,identical(l,oldl),identical(r,oldr));checks<-checks+1L}
 contributing_mutable<-left_mutable || (shape%in%c('join','union','nested')&&right_mutable)
 for(fun in list(nest_with_margins,nest_by_with_margins)){
 seen<-integer();res<-tryCatch(fun(q,.grouping=rollup(g),.keep=TRUE),error=identity)
 if(contributing_mutable)stopifnot(inherits(res,'marginplyr_error'),grepl('immutable = FALSE',conditionMessage(res),fixed=TRUE),length(seen)==0L) else if(identical(fun,nest_with_margins))stopifnot(inherits(res,'dtplyr_step'),length(seen)==0L) else stopifnot(is.data.frame(res),length(seen)>0L,all(seen>0L))
 stopifnot(identical(l,oldl),identical(r,oldr));checks<-checks+1L
 }
}
cat('Cached/mutable graph boundary checks',checks,'all passed; source snapshots preserved.\n')
# A name-only inspection must not execute a row-transform callback even when its
# source graph is cached twice and includes both join/set inputs.
unsafe_checks<-0L;seen<-integer();callback<-function(x){seen<<-c(seen,length(x));x}
root<-dtplyr::lazy_dt(data.frame(g=c('a','b'),v=1:2));unsafe<-mutate(root,value=callback(v))
for(q in list(unsafe,arrange(compute(unsafe),v),compute(rename(compute(unsafe),vv=value)),left_join(compute(unsafe),root,by='g'),union_all(compute(unsafe),compute(unsafe)))){
 seen<-integer();p<-inspect_grouping(q,.grouping=rollup(g));stopifnot(nrow(p)==2L,length(seen)==0L);unsafe_checks<-unsafe_checks+1L
 for(fun in list(function(x)inspect_grouping(x,.grouping=rollup(where(is.character))),function(x)nest_with_margins(x,.grouping=rollup(g)),function(x)nest_by_with_margins(x,.grouping=rollup(g)))){
 e<-tryCatch(fun(q),error=identity);stopifnot(inherits(e,'marginplyr_error'),grepl("Can't safely determine column metadata",conditionMessage(e),fixed=TRUE),length(seen)==0L);unsafe_checks<-unsafe_checks+1L
 }
}
cat('Cached unsafe-metadata boundary checks',unsafe_checks,'all passed; callback executions 0.\n')
