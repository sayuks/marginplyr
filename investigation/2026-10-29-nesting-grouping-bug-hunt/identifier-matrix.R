pkgload::load_all('.',quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
records <- list()
normalize <- function(z,id) {
 if(inherits(z,'dtplyr_step')) z<-tibble::as_tibble(z,.name_repair='minimal')
 z<-as.data.frame(dplyr::ungroup(z));names(z)[names(z)==id]<-'set';if(sort=='none' && nrow(z)>0L) z<-z[do.call(order,unname(as.list(z[setdiff(names(z),'data')]))),,drop=FALSE];z<-as.list(z);z$data<-lapply(z$data,function(cell)list(rows=nrow(cell),values=as.list(cell)));z
}
for(id in c('...','..1','..2','..0','..01')) for(size in c(0L,3L)) for(plan in c('none','fixed','rollup','duplicate_drop')) for(keep in c(FALSE,TRUE)) for(sort in c('none','last','first')) for(verb in list(nest_with_margins,nest_by_with_margins)) {
 x<-tibble(g=c('a','b','a')[seq_len(size)],v=seq_len(size));original<-x
 run<-function(input,identifier) verb(input,.by=if(plan=='fixed') all_of('g'),.grouping=switch(plan,none=NULL,fixed=NULL,rollup=rollup(g),duplicate_drop=grouping_sets(grouping_set(g),grouping_set(g),grouping_set())),.id=identifier,.key='data',.keep=keep,.sort=sort,.duplicates=if(plan=='duplicate_drop')'drop' else 'error',.margin_label=NULL)
 safe_local<-run(x,'set');actual_local<-run(x,id)
 stopifnot(identical(normalize(safe_local,'set'),normalize(actual_local,id)),identical(x,original))
 source<-data.table::as.data.table(x);before<-data.table::copy(source)
 lazy<-dtplyr::lazy_dt(source)
 safe_dt<-run(lazy,'set');stopifnot(identical(normalize(safe_dt,'set'),normalize(safe_local,'set')),identical(source,before))
 e<-tryCatch({result<-run(lazy,id);stopifnot(identical(normalize(result,id),normalize(safe_local,'set')));NULL},error=identity)
 stopifnot(identical(source,before))
 records[[length(records)+1L]]<-list(id=id,size=size,plan=plan,keep=keep,sort=sort,verb=if(identical(verb,nest_with_margins))'nest'else'nest_by',failed=!is.null(e),error_class=if(is.null(e))NA_character_ else class(e)[1],message=if(is.null(e))NA_character_ else conditionMessage(e))
}
out<-dplyr::bind_rows(records)
print(count(out,id,failed),n=Inf)
cat('Cases:',nrow(out),'failures:',sum(out$failed),'\n')
cat('Local and ordinary-name dtplyr controls all matched; sources all unchanged\n')

print(count(out,id,error_class),n=Inf)
if('--assert' %in% commandArgs(TRUE) && any(out$failed)) stop('BUG REPRODUCED: dtplyr nesting output identifiers are rejected internally.')
