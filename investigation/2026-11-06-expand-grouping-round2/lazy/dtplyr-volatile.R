Sys.setenv(TESTTHAT_PARALLEL='false')
output <- dirname(normalizePath(sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())])))
pkgload::load_all('.',quiet=TRUE)
library(dplyr,warn.conflicts=FALSE)
cap<-function(x)tryCatch(force(x),error=function(e)e)
canon<-function(x){x<-as_tibble(x);x<-select(x,sort(names(x)));arrange(x,across(everything()))}
forms<-list(filter=identity,head=function(x)head(x,4L),slice=function(x)slice(x,seq_len(3L)),sample=function(x)slice_sample(x,n=3L),arrange=function(x)arrange(x,desc(rid)),cache=function(x)compute(x,name='volatile_cache'),cache_order=function(x)arrange(compute(x,name='volatile_cache'),desc(rid)),group_cache=function(x)group_by(compute(x,name='volatile_cache'),f),group_filter=function(x)group_by(filter(x,rid>1L),f),union=function(x)union_all(x,x),join=function(x)left_join(x,select(x,rid,p),by='rid'))
families<-list(rollup=list(spec=rollup(g,h),sets=list(c('g','h'),'g',character())),composite=list(spec=rollup(grouping_set(g,h)),sets=list(c('g','h'),character())),repeated=list(spec=grouping_sets(grouping_set(),grouping_set(g),grouping_set()),sets=list(character(),'g',character())))
res<-list();kk<-0L
for(n in c(0L,1L,8L))for(form in names(forms))for(fam in names(families))for(sort in c('none','first','last')) {
 counter<-new.env();counter$n<-0L
 predicate<-function(v){counter$n<-counter$n+1L;runif(length(v))>0.3}
 source<-data.table::data.table(g=rep(c('b','a'),4L)[seq_len(n)],h=rep(c(2L,1L),4L)[seq_len(n)],f=rep(c('q','p'),4L)[seq_len(n)],rid=seq_len(n),p=seq_len(n)*3L)
 before<-data.table::copy(source)
 q<-forms[[form]](filter(dtplyr::lazy_dt(source),predicate(rid)))
 set.seed(8791);eager<-as_tibble(data.table::as.data.table(q)); native_calls<-counter$n;counter$n<-0L
 actual<-cap(if(length(group_vars(q))) expand_with_margins(q,.grouping=families[[fam]]$spec,.duplicates='keep',.id='sid',.margin_label=NULL,.sort=sort) else expand_with_margins(q,.by=f,.grouping=families[[fam]]$spec,.duplicates='keep',.id='sid',.margin_label=NULL,.sort=sort))
 constructed_calls<-counter$n
 set.seed(8791);actual<-cap(if(inherits(actual,'error'))actual else as_tibble(data.table::as.data.table(actual)))
 calls<-counter$n
 sets<-families[[fam]]$sets
 expected<-bind_rows(lapply(seq_along(sets),function(i){b<-eager;if(!'g'%in%sets[[i]])b$g<-rep(NA_character_,nrow(b));if('h'%in%unlist(sets) && !'h'%in%sets[[i]])b$h<-rep(NA_integer_,nrow(b));b$sid<-rep(as.integer(i),nrow(b));b}))
 status<-if(inherits(actual,'error') && grepl('safely determine column metadata',conditionMessage(actual),fixed=TRUE))'safe-metadata-refusal' else if(inherits(actual,'error'))'error' else if(identical(canon(actual),canon(expected)) && calls==native_calls && constructed_calls==0L && identical(source,before))'match' else 'mismatch'
 kk<-kk+1L;res[[kk]]<-data.frame(n,form,fam,sort,status,native_calls,calls,constructed_calls,error=if(inherits(actual,'error'))conditionMessage(actual)else'')
}
r<-bind_rows(res);write.csv(r,file.path(output,'dtplyr-volatile.csv'),row.names=FALSE);print(count(r,status));print(unique(r[r$status!='match',c('form','status','error')]),row.names=FALSE)
stopifnot(all(r$status %in% c('match','safe-metadata-refusal')))
