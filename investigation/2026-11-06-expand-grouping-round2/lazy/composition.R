Sys.setenv(TESTTHAT_PARALLEL='false')
output <- dirname(normalizePath(sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())])))
pkgload::load_all('.',quiet=TRUE)
library(dplyr,warn.conflicts=FALSE)
library(arrow,warn.conflicts=FALSE)
warnings_seen<-character(); cap<-function(x)withCallingHandlers(tryCatch(force(x),error=function(e)e),warning=function(w){warnings_seen<<-c(warnings_seen,conditionMessage(w));invokeRestart('muffleWarning')})
canon<-function(x){x<-collect(x); x<-x[,sort(names(x)),drop=FALSE]; if(nrow(x))x<-x[do.call(order,c(lapply(x,as.character),list(na.last=TRUE))),,drop=FALSE];rownames(x)<-NULL;x}
base_data<-tibble(k=c('b','a',NA),j=c(2L,1L,3L),rid=1:3,z=c(4,1,NA_real_))
t<-Table$create(base_data)
ordinary<-function(x){union_all(mutate(x,sid=1L),mutate(x,k=NA_character_,sid=2L))}
q<-expand_with_margins(t,.grouping=rollup(k),.id='sid',.margin_label=NULL)
e<-ordinary(t)
forms<-list(identity=identity,head=function(x)head(x,3),tail=function(x)tail(x,3),filter=function(x)filter(x,sid==2L),select=function(x)select(x,rid,k,sid),rename=function(x)rename(x,renamed=k),distinct=function(x)distinct(x,k,sid),group=function(x)summarise(group_by(x,sid),z=sum(z,na.rm=TRUE)),window=function(x)mutate(group_by(x,sid),w=dense_rank(z)),join_self=function(x)left_join(x,x,by=c('rid','sid'),relationship='many-to-many'),join_left=function(x)left_join(x,t,by='rid',relationship='many-to-many'),join_right=function(x)left_join(t,x,by='rid',relationship='many-to-many'),union_self=function(x)union_all(x,x),intersect_self=function(x)intersect(x,x),except=function(x)setdiff(x,filter(x,sid==2L)))
res<-list();n<-0L
forms<-forms[setdiff(names(forms),c('head','tail'))]
for(nm in names(forms))for(route in c('collect','compute','reader'))for(sort in c('none','first','last')) {
 qq<-expand_with_margins(t,.grouping=rollup(k),.id='sid',.margin_label=NULL,.sort=sort)
 act<-cap(forms[[nm]](qq)); native<-if(sort=='none') e else if(sort=='first') arrange(e,desc(if_else(sid==2L,1L,0L)),if_else(is.na(k),1L,0L),k,sid) else arrange(e,if_else(sid==2L,1L,0L),if_else(is.na(k),1L,0L),k,sid); expected<-cap(forms[[nm]](native))
 consume<-function(x){if(inherits(x,'error'))return(x);cap(switch(route,collect=canon(x),compute=canon(compute(x)),reader=canon(as_arrow_table(as_record_batch_reader(x)))))}
 a<-consume(act);b<-consume(expected)
 status<-if(inherits(b,'error'))'ordinary-error' else if(inherits(a,'error'))'package-error' else if(isTRUE(all.equal(a,b,check.attributes=TRUE)))'match' else 'mismatch'
 n<-n+1L;res[[n]]<-data.frame(form=nm,route,sort,status,error=if(inherits(a,'error'))conditionMessage(a)else'',native_error=if(inherits(b,'error'))conditionMessage(b)else'')
}
r<-bind_rows(res);write.csv(r,file.path(output,'composition.csv'),row.names=FALSE);print(count(r,status));print(filter(r,status %in% c('mismatch','package-error')),row.names=FALSE)
stopifnot(!any(r$status %in% c('mismatch','package-error')))
writeLines(warnings_seen,file.path(output,'composition-warnings.txt'))
