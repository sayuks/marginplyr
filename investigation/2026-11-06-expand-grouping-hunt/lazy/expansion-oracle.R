pkgload::load_all('.',quiet=TRUE)
library(dplyr,warn.conflicts=FALSE)
Sys.setenv(TESTTHAT_PARALLEL='false')
cat('Baseline:',system2('git',c('rev-parse','HEAD'),stdout=TRUE),'\n')
backends <- list(arrow=function(x)arrow::Table$create(x),dtplyr=function(x)dtplyr::lazy_dt(x))
shapes <- list(identity=function(x)x,filter=function(x)filter(x,rid!=2L),arrange=function(x)arrange(x,desc(rid)),project=function(x)select(x,payload,rid,y,x,f))
families <- list(
 rollup=list(spec=rollup(x,y),sets=list(c('x','y'),'x',character())),
 cube=list(spec=cube(x,y),sets=list(c('x','y'),'x','y',character())),
 product=list(spec=grouping_spec(rollup(x),cube(y)),sets=list(c('x','y'),'x','y',character())),
 repeated=list(spec=grouping_sets(grouping_set(),grouping_set(x),grouping_set(),grouping_set(y)),sets=list(character(),'x',character(),'y')),
 union=list(spec=grouping_sets(grouping_set(y),grouping_set(x,y)),sets=list('y',c('x','y'))))
checks <- 0L; failures<-list()
canonical<-function(x){x<-as_tibble(x);x<-x[order(x$sid,x$rid),c('f','x','y','sid','rid','payload')];rownames(x)<-NULL;x}
for(be in names(backends))for(n in c(0L,1L,4L))for(shape in names(shapes))for(family in names(families))for(label in list(NULL,'Total'))for(sort in c('none','first','last')) {
 data<-tibble(f=c('b','a',NA,'a')[seq_len(n)],x=c('x','z',NA,'x')[seq_len(n)],y=c('c',NA,'b','a')[seq_len(n)],rid=seq_len(n),payload=c(2L,4L,6L,8L)[seq_len(n)])
 source<-shapes[[shape]](backends[[be]](data)); eager<-shapes[[shape]](data)
 expected<-bind_rows(lapply(seq_along(families[[family]]$sets),function(sid){
  x<-eager; for(dim in setdiff(c('x','y'),families[[family]]$sets[[sid]]))x[[dim]]<-rep(if(is.null(label))NA_character_ else label,nrow(x));x$sid<-rep(as.integer(sid),nrow(x));x
 }))
 actual<-tryCatch(collect(expand_with_margins(source,.by=f,.grouping=families[[family]]$spec,.margin_label=label,.id='sid',.sort=sort,.duplicates='keep')),error=identity)
 ok<-!inherits(actual,'error')&&identical(canonical(actual),canonical(expected))
 if(!ok)failures[[length(failures)+1L]]<-data.frame(backend=be,n=n,shape=shape,family=family,label=if(is.null(label))'NULL' else label,sort=sort,error=if(inherits(actual,'error'))conditionMessage(actual)else 'row mismatch')
 checks<-checks+1L
}
cat('Independent row-copy comparisons:',checks,'; failures:',length(failures),'\n')
if(length(failures)) {write.csv(bind_rows(failures),'investigation/2026-11-06-expand-grouping-hunt/lazy/expansion-oracle-failures.csv',row.names=FALSE);print(head(bind_rows(failures),10))}
stopifnot(length(failures)==0L)
