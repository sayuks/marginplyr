suppressPackageStartupMessages(library(dplyr))
pkgload::load_all(if (length(commandArgs(TRUE))) commandArgs(TRUE)[1L] else getwd(), quiet=TRUE)
options(dplyr.summarise.inform=FALSE)
set.seed(1221)
sc<-DBI::dbConnect(RSQLite::SQLite(),':memory:'); dc<-DBI::dbConnect(duckdb::duckdb(shared_home=FALSE),dbdir=':memory:')
keysame<-function(df,row,cols) { z<-rep(TRUE,nrow(df)); for(k in cols) z<-z & vctrs::vec_equal(df[[k]],df[[k]][row],na_equal=TRUE);z }
ratio<-function(n,d,own=FALSE) if(own) 1 else if(is.na(n)||is.na(d)||d==0) NA_real_ else as.double(n)/as.double(d)
fail<-function(...) {cat('FAIL',...,'\n');stop('share mismatch')}
for(i in 1:24) {
 n<-sample(0:30,1); dat<-data.frame(f=sample(c('A','B',NA),n,TRUE),g=sample(c('a','b',NA),n,TRUE),h=sample(c('x','y',NA),n,TRUE),x=sample(c(-3,-1,0,1,2,NA),n,TRUE))
 for(kind in c('rollup','repeated','cube','arbitrary','root')) {
  grouping<-switch(kind,rollup=rollup(g,h), repeated=rollup(g,h,h),cube=cube(g,h),arbitrary=grouping_sets(grouping_set(),grouping_set(h),grouping_set(g,h),grouping_set(g)),root=grouping_set())
  sets<-switch(kind,rollup=list(c('g','h'),'g',character()),repeated=list(c('g','h'),c('g','h'),'g',character()),cube=list(c('g','h'),'g','h',character()),arbitrary=list(character(),'h',c('g','h'),'g'),root=list(character()))
  root<-which(lengths(sets)==0)
  for(dups in if(kind=='repeated') 'keep' else c('drop','keep')) {
   for(b in c('local','dt','sqlite','duck')) {
    d<-switch(b,local=dat,dt=dtplyr::lazy_dt(dat),sqlite=copy_to(sc,dat,'facts',overwrite=TRUE),duck=copy_to(dc,dat,'facts',overwrite=TRUE))
    q<-summarize_with_margins(d,value=sum(x),count=n(),t=share_of_total(value),ct=share_of_total(count),!!!if(kind %in% c('rollup','repeated')) rlang::exprs(p=share_of_parent(value)) else list(),.by=f,.grouping=grouping,.duplicates=dups,.id='set',.margin_label=NULL,.check_share_source=FALSE)
    r<-as.data.frame(if(b=='local') q else collect(q))
    for(j in seq_len(nrow(r))) {
     rootrow<-which(keysame(r,j,'f') & r$set==root); if(length(rootrow)!=1) fail(i,kind,b,'root n',length(rootrow))
     expected<-ratio(r$value[j],r$value[rootrow],r$set[j]==root)
     if(!isTRUE(all.equal(r$t[j],expected))) {print(r); fail(i,kind,b,'total',j,expected,r$t[j])}
     expected<-ratio(r$count[j],r$count[rootrow],r$set[j]==root)
     if(!isTRUE(all.equal(r$ct[j],expected))) fail(i,kind,b,'count total',j)
     if('p'%in%names(r)) {
      target<-if(r$set[j]==length(sets)) NA_integer_ else which(seq_along(sets)>r$set[j] & lengths(sets)<length(sets[[r$set[j]]]))[1]
      if(is.na(target)) expected<-1 else {
       prow<-which(keysame(r,j,c('f',sets[[target]])) & r$set==target)
       if(length(prow)!=1) fail(i,kind,b,'parent n',length(prow))
       expected<-ratio(r$value[j],r$value[prow])
      }
      if(!isTRUE(all.equal(r$p[j],expected))) {print(r);fail(i,kind,b,'parent',j,expected,r$p[j])}
     }
    }
   }
  }
 }
 cat('DONE',i,'\n')
}
DBI::dbDisconnect(sc);DBI::dbDisconnect(dc,shutdown=TRUE)
