args<-commandArgs(TRUE); pkgload::load_all(args[[1]],quiet=TRUE);suppressPackageStartupMessages(library(dplyr))
run <- function(label,expr){cat('\nCASE',label,'\n');print(tryCatch(force(expr),error=function(e)paste('ERROR',conditionMessage(e))))}
con<-DBI::dbConnect(RSQLite::SQLite(),':memory:');src<-copy_to(con,tibble(g=c('a','b'),v=c(2,5)),'source',temporary=FALSE);q<-summarize_with_margins(src,total=sum(v),.grouping=rollup(g),.id='id')
DBI::dbExecute(con,"ATTACH DATABASE ':memory:' AS other")
run('qualified index/analyze/overwrite success',{
 DBI::dbWriteTable(con,DBI::Id(schema='other',table='report'),data.frame(old=1))
 m<-compute(q,name=dbplyr::in_schema('other','report'),temporary=FALSE,overwrite=TRUE,indexes=list('g'),analyze=TRUE)
 print(collect(m)); print(DBI::dbGetQuery(con,"PRAGMA other.index_list('report')"));print(DBI::dbGetQuery(con,'SELECT * FROM other.sqlite_stat1'));TRUE
})
run('ordinary dbplyr qualified index control',{
 m<-compute(select(src,g,v),name=dbplyr::in_schema('other','ordinary'),temporary=FALSE,indexes=list('g'),analyze=TRUE)
 collect(m)
})
run('overwrite input atomic refusal',{
 before<-DBI::dbReadTable(con,'source');err<-tryCatch(compute(q,name='source',temporary=FALSE,overwrite=TRUE,analyze=FALSE),error=identity)
 stopifnot(inherits(err,'error'),identical(DBI::dbReadTable(con,'source'),before));conditionMessage(err)
})
run('ordinary overwrite input control',{
 before<-DBI::dbReadTable(con,'source');err<-tryCatch(compute(select(src,g,v),name='source',temporary=FALSE,overwrite=TRUE,analyze=FALSE),error=identity)
 print(err);print(DBI::dbExistsTable(con,'source')); if(DBI::dbExistsTable(con,'source'))DBI::dbReadTable(con,'source')
})
DBI::dbDisconnect(con)
for(sorted in c(FALSE,TRUE)) run(paste('bare persistent destination competing TEMP',sorted),{
 con<-DBI::dbConnect(RSQLite::SQLite(),':memory:');src<-copy_to(con,tibble(g=c('a','b'),v=c(2,5)),'source',temporary=FALSE)
 q<-summarize_with_margins(src,total=sum(v),.grouping=rollup(g),.id='id',.sort=if(sorted)'last' else 'none')
 DBI::dbWriteTable(con,'report',data.frame(g='sentinel',id=99L,total=-1),temporary=TRUE)
 m<-compute(q,name='report',temporary=FALSE,analyze=TRUE)
 print(DBI::dbGetQuery(con,'SELECT * FROM main.report'));print(DBI::dbGetQuery(con,'SELECT * FROM temp.report'));print(collect(m));DBI::dbDisconnect(con)
})
