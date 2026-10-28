pkgload::load_all('.',quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
set.seed(5871)
comparisons <- 0L
refusals <- 0L
failures <- 0L
canon<-function(x,key){x<-ungroup(x);ord<-order(x$set,vapply(x[[key]],function(z)paste(sort(z$rowid),collapse=','),character(1)));x<-x[ord,];list(o=as.list(x[setdiff(names(x),key)]),d=lapply(x[[key]],function(z){z<-as_tibble(z);z<-z[order(z$rowid),];list(n=nrow(z),v=as.list(z))}))}
for(case in 1:250){
 n<-sample(0:9,1);a<-sample(c('a','b','Total',NA),n,TRUE);b<-sample(c('x','y','Total',NA),n,TRUE);f<-sample(c(1L,2L,NA),n,TRUE)
 if(case%%3==1)a<-factor(a,levels=c('b','a','Total',NA),exclude=NULL)
 if(case%%3==2)b<-I(as.integer(factor(b)))
 x<-tibble(f=f,g1=a,g2=b,rowid=seq_len(n),payload=factor(a,levels=c('a','b','Total',NA),exclude=NULL))
 nms<-sample(c('g1','.N','.SD','.I','.BY','T','F','.GRP','.NGRP','normal key'),2);names(x)[2:3]<-nms
 fixname<-sample(c('f','.SD','.BY','.GRP'),1); if(fixname%in%nms)fixname<-'f';names(x)[1]<-fixname
 form<-sample(1:5,1);g<-switch(form,rollup(all_of(nms)),cube(all_of(nms)),rollup(all_of(nms[1]),all_of(nms[2])),cube(all_of(nms[1]),all_of(nms[2])),grouping_sets(grouping_set(all_of(nms[1])),grouping_set(all_of(nms[2])),grouping_set()))
 useby<-sample(c(FALSE,TRUE),1);keep<-sample(c(FALSE,TRUE),1);key<-sample(c('data','.SD','.N','T','F','.rows'),1);if(key%in%c(nms,fixname))key<-'data'
 sort<-sample(c('none','first','last'),1);label<-list(NULL,NA_character_,'Total')[[sample(1:3,1)]]
 run<-function(input,verb)verb(input,.by=if(useby)all_of(fixname),.grouping=g,.id='set',.key=key,.keep=keep,.sort=sort,.margin_label=label,.check_margin_label=FALSE) |>collect()
 verb<-if(case%%2)nest_with_margins else nest_by_with_margins
 local<-tryCatch(run(x,verb),error=identity);actual<-tryCatch(run(dtplyr::lazy_dt(x),verb),error=identity)
 if(inherits(local,'error')) {
  stopifnot(inherits(local,'marginplyr_error'),grepl('already a factor level',conditionMessage(local),fixed=TRUE),inherits(actual,'marginplyr_error'),identical(conditionMessage(local),conditionMessage(actual)))
  refusals <- refusals+1L
  next
 }
 comparisons <- comparisons+1L
 if(inherits(actual,'error')) { failures <- failures+1L;cat('FAIL',case,conditionMessage(actual),'\n');next }
 eq <- all.equal(canon(local,key),canon(actual,key))
 if(!isTRUE(eq)) { failures <- failures+1L;cat('FAIL',case,paste(eq,collapse=' / '),'\n') }
}
cat('comparisons',comparisons,'expected factor-level refusals',refusals,'failures',failures,'\n')
stopifnot(failures==0L,comparisons+refusals==250L)
