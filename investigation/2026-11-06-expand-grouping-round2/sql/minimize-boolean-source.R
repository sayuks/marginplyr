Sys.setenv(TESTTHAT_PARALLEL='false')
pkgload::load_all('.',quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
con<-DBI::dbConnect(RSQLite::SQLite(),':memory:');on.exit<-NULL
input<-data.frame(g='a',v=7L,check.names=FALSE);names(input)[[2L]]<-"TRUE";DBI::dbWriteTable(con,'input',input)
src<-tbl(con,'input')
obs<-function(name,expr){out<-tryCatch(force(expr),error=identity);cat(name,if(inherits(out,'error'))conditionMessage(out)else if(inherits(out,'tbl_sql')) 'built' else paste(capture.output(print(out)),collapse=' '),'\n');invisible(out)}
for(gn in c('root','mutate','window','collapse','union')){
 x<-switch(gn,root=src,mutate=mutate(src,z=1L),window=mutate(src,z=row_number(g)),collapse=collapse(filter(src,TRUE)),union=union_all(src,src))
 obs(paste(gn,'source'),collect(x))
 for(sets in c('single','rollup'))for(id in c(FALSE,TRUE))for(sort in c('none','last')){
  sp<-if(sets=='single')grouping_set(g)else rollup(g)
  m<-obs(paste(gn,sets,id,sort,'build'),expand_with_margins(x,.grouping=sp,.margin_label=NULL,.id=if(id)'sid'else NULL,.sort=sort))
  if(!inherits(m,'error')){obs(paste(gn,sets,id,sort,'collect'),collect(m));if(gn=='mutate'&&sets=='single'&&id&&sort=='none')cat(as.character(dbplyr::sql_render(m)),'\n')}
 }
}
DBI::dbDisconnect(con)
