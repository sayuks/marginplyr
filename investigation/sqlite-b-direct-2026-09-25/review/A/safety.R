args<-commandArgs(TRUE);pkgload::load_all(args[[1]],quiet=TRUE);suppressPackageStartupMessages(library(dplyr))
for(sort in c('none','last'))for(dest in c('qualified','bare'))for(tx in c(FALSE,TRUE)){
 cat('CASE',sort,dest,'transaction=',tx,'\n')
 con<-DBI::dbConnect(RSQLite::SQLite(),':memory:');src<-copy_to(con,tibble(g=c('a','b'),v=c(2,5)),'source',temporary=FALSE)
 q<-summarize_with_margins(src,total=sum(v,na.rm=TRUE),.grouping=rollup(g),.id='id',.sort=sort)
 DBI::dbExecute(con,"ATTACH DATABASE ':memory:' AS other")
 sentinel<-data.frame(g='sentinel',id=99L,total=-1)
 DBI::dbWriteTable(con,'report',sentinel,temporary=FALSE)
 DBI::dbExecute(con,'CREATE TEMP TABLE report AS SELECT * FROM main.report')
 DBI::dbExecute(con,'CREATE TABLE marker (x INT)')
 if(tx){DBI::dbBegin(con);DBI::dbExecute(con,'INSERT INTO marker VALUES (1)')}
 before<-DBI::dbListTables(con)
 name<-if(dest=='qualified')dbplyr::in_schema('other','report') else 'report'
 err<-tryCatch(compute(q,name=name,temporary=FALSE,overwrite=TRUE),error=identity)
 stopifnot(inherits(err,'marginplyr_error'));cat(conditionMessage(err),'\n')
 stopifnot(identical(before,DBI::dbListTables(con)),identical(DBI::dbGetQuery(con,'SELECT * FROM main.report'),sentinel),identical(DBI::dbGetQuery(con,'SELECT * FROM temp.report'),sentinel),nrow(collect(q))==3L)
 if(tx){stopifnot(nrow(DBI::dbReadTable(con,'marker'))==1L);DBI::dbRollback(con);stopifnot(nrow(DBI::dbReadTable(con,'marker'))==0L)}
 control<-compute(src,name='control',analyze=FALSE);stopifnot(nrow(collect(control))==2L)
 DBI::dbDisconnect(con);cat('PASS no staging/write, source preserved, collect and ordinary compute usable\n')
}
