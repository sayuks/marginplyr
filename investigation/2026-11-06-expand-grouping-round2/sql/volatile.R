# Outputs use MARGINPLYR_HUNT_OUTPUT, with a dated scratch default.
output_dir <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", unset = "/private/tmp/marginplyr-sql-round2")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(TESTTHAT_PARALLEL='false')
pkgload::load_all('.',quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
records<-list();i<-0L
for(engine in c('sqlite','duckdb')){
 con<-if(engine=='sqlite')DBI::dbConnect(RSQLite::SQLite(),':memory:')else DBI::dbConnect(duckdb::duckdb(shared_home=FALSE),':memory:')
 DBI::dbWriteTable(con,'input',data.frame(g=c('a','b','c'),rid=1:3),overwrite=TRUE)
 for(name in c('v','TRUE','.env','..0'))for(graph in c('root','window','reexpand'))for(sort in c('none','last'))for(cte in c(FALSE,TRUE)){
  expr<-if(engine=='sqlite') dbplyr::sql('RANDOM()') else dbplyr::sql('RANDOM()')
  src<-mutate(tbl(con,'input'),!!name:=!!expr)
  if(graph=='window') src<-mutate(src,rn=row_number(rid))
  if(graph=='reexpand') src<-expand_with_margins(src,.grouping=rollup(g),.margin_label=NULL,.id='previous')
  m<-expand_with_margins(src,.grouping=rollup(g),.margin_label=NULL,.id='sid',.sort=sort)
  result<-tryCatch(suppressWarnings(collect(m,sql_options=dbplyr::sql_options(cte=cte))),error=identity)
  ok<-if(inherits(result,'error'))FALSE else all(vapply(split(result[[name]],result$rid),function(z)length(unique(z))==1L,logical(1)))&&nrow(result)==if(graph=='reexpand')12L else 6L
  i<-i+1L;records[[i]]<-data.frame(engine,name,graph,sort,cte,ok,message=if(inherits(result,'error'))conditionMessage(result)else'')
 }
 DBI::dbDisconnect(con,shutdown=engine=='duckdb')
}
out<-do.call(rbind,records);write.csv(out,file.path(output_dir, "volatile.csv"),row.names=FALSE);print(table(out$engine,out$ok));print(out[!out$ok,])
