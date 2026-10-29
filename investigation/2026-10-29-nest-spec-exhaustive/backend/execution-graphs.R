# Investigated: 2026-10-29; baseline ee4321789ca60b6259c53df2852f1574aaede584.
# Run from the repository root with Rscript <script>.
pkgload::load_all('.',quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
normal <- function(x) {
 x <- ungroup(collect(x)); x$data <- lapply(x$data,function(z){ z<-as_tibble(z);if('row'%in%names(z))z<-arrange(z,row);z });x<-arrange(x,sid,g);as_tibble(x)
}
root_source <- function() data.table::data.table(g=c('a','a','b','c'),v=c(3L,1L,2L,4L),row=1:4)
other_source <- function() data.table::data.table(g=c('a','b','d'),w=c(8L,9L,10L))
ops <- list(id=identity,filter=function(x)filter(x,TRUE),reorder=function(x)relocate(x,v),rename=function(x)rename(x,value=v),rename_back=function(x)rename(rename(x,value=v),v=value),cache=function(x)compute(x),cache2=function(x)compute(compute(x)),arrange=function(x)arrange(x,desc(v)),head=function(x)head(x,3L),slice=function(x)slice(x,2:3),mutate=function(x)mutate(x,new=v+1L),select=function(x)select(x,g,v,row))
posts <- list(id=identity,cache=function(x)compute(x),filter=function(x)filter(x,TRUE),rename_back=function(x)rename(rename(x,zz=g),g=zz),reorder=function(x)relocate(x,row),arrange=function(x)arrange(x,g))
make <- list(single=function(x)x,left=function(x)left_join(x,other,by='g'),right=function(x)right_join(x,other,by='g'),full=function(x)full_join(x,other,by='g'),inner=function(x)inner_join(x,other,by='g'),semi=function(x)semi_join(x,other,by='g'),anti=function(x)anti_join(x,other,by='g'),union=function(x)union_all(x,x),set=function(x)union(x,x))
rows<-list(); fails<-list(); n<-0L
for (op in names(ops)) for(post in names(posts)) for(graph in names(make)) {
 source <- root_source(); reference <- data.table::copy(source)
 right <- other_source(); right_reference <- data.table::copy(right)
 root <- dtplyr::lazy_dt(source); other <- dtplyr::lazy_dt(right)
 label<-paste(op,post,graph,sep='/')
 step<-tryCatch(make[[graph]](posts[[post]](ops[[op]](root))),error=identity)
 if(inherits(step,'error')) next
 src<-tryCatch(collect(step),error=identity)
 if(inherits(src,'error'))next
 for(keep in c(FALSE,TRUE))for(verb in c('nest','nest_by')){
  n<-n+1L
  fun<-if(verb=='nest')nest_with_margins else nest_by_with_margins
  expected<-tryCatch(normal(fun(src,.grouping=rollup(g),.keep=keep,.id='sid',.sort='last')),error=identity)
  actual<-tryCatch(normal(fun(step,.grouping=rollup(g),.keep=keep,.id='sid',.sort='last')),error=identity)
  status<-if(inherits(actual,'marginplyr_error')&&grepl("Can't safely determine column metadata",conditionMessage(actual),fixed=TRUE))'metadata-refusal' else if(inherits(expected,'error'))'control-error' else if(inherits(actual,'error'))'error' else if(identical(actual,expected))'match' else 'mismatch'
  stopifnot(identical(source,reference),identical(right,right_reference))
  rows[[n]]<-data.frame(label,keep,verb,status,message=if(inherits(actual,'error'))conditionMessage(actual)else'')
  if(status%in%c('error','mismatch')){fails[[paste(label,keep,verb)]]<-list(actual=actual,expected=expected);cat('FAIL',label,keep,verb,status,'\n')}
 }
}
res<-bind_rows(rows);print(count(res,status));print(filter(res,status%in%c('error','mismatch')),n=30)
outdir <- dirname(sub('^--file=', '', grep('^--file=', commandArgs(), value=TRUE)[[1L]]))
write.csv(res,file.path(outdir,'execution-graphs-results.csv'),row.names=FALSE)
cat('Source snapshots remained identical in all cases.\n')
stopifnot(!length(fails))
