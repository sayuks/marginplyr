args<-commandArgs(trailingOnly=TRUE)
pkgload::load_all(if(length(args))args[[1L]]else getwd(),quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
selectors<-list(numeric=quote(where(is.numeric)),integer=quote(where(is.integer)),double=quote(where(is.double)),character=quote(where(is.character)),logical=quote(where(is.logical)),classed=quote(where(is.object)),matrix=quote(where(is.matrix)),list=quote(where(is.list)))
values<-list(integer=1:3,double=c(1,2,NA),logical=c(TRUE,FALSE,NA),complex=as.complex(1:3),matrix=matrix(1:6,3),array=array(1:6,c(3,1,2)),factor=factor(c('a','b',NA)),date=as.Date('2026-01-01')+1:3)
changes<-list(identity=function(x)x,copy=function(x)mutate(x,new=a),arithmetic=function(x)mutate(x,new=a+1L),negative=function(x)mutate(x,new=-a),divide=function(x)mutate(x,new=a/2L),overcopy=function(x)mutate(x,b=a),overwrite=function(x)mutate(x,a=a+1L),select=function(x)select(x,b,a),relocate=function(x)relocate(x,b),rename=function(x)rename(x,renamed=a),filter=function(x)filter(x,row>1),drop=function(x)select(x,-b),leftjoin=function(x)left_join(x,dtplyr::lazy_dt(data.frame(row=2:4,v=1:3)),by='row'),union=function(x)union_all(x,x))
checked<-0L;refused<-0L
for(vn in names(values)){
 input<-data.table::data.table(row=1:3,a=values[[vn]],b=c('x','y','z'))
 source<-dtplyr::lazy_dt(input)
 for(cn in names(changes)){
  step<-tryCatch(changes[[cn]](source),error=identity)
  if(inherits(step,'error'))next
  actual<-tryCatch(collect(step),error=identity)
  if(inherits(actual,'error'))next
  for(sn in names(selectors)){
   spec<-eval(rlang::call2('grouping_set',selectors[[sn]]))
   observed<-tryCatch(inspect_grouping(step,.grouping=spec,.format='list'),error=identity)
   expected<-tryCatch(inspect_grouping(actual,.grouping=spec,.format='list'),error=identity)
   if(inherits(observed,'marginplyr_error')&&grepl('safely determine column metadata',conditionMessage(observed))) {refused<-refused+1L;next}
   checked<-checked+1L
   if(!identical(observed,expected)){cat('FAIL',vn,cn,sn,'\n');print(observed);print(expected);stop('Typed metadata mismatch')}
  }
 }
}
cat('PASS: dtplyr typed metadata',checked,'comparisons;',refused,'documented unsafe-step refusals\n')
