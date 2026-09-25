args <- commandArgs(TRUE)
pkgload::load_all(args[[1]],quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
passed <- 0L
for(sort in c("none","last")) for(spelling in c("string","ident","Id","AsIs","literal_dot")) {
 con <- DBI::dbConnect(RSQLite::SQLite(),":memory:")
 base <- if(spelling=="literal_dot") "re.port" else "report"
 name <- switch(spelling,string=base,ident=dbplyr::ident(base),Id=DBI::Id(table=base),AsIs=I(base),literal_dot=base)
 source <- copy_to(con,tibble(g=c("a","b"),v=c(2,5)),"source")
 sentinel <- data.frame(g="keep",sid=99L,total=-1)
 DBI::dbWriteTable(con,DBI::Id(schema="temp",table=base),sentinel)
 q <- summarize_with_margins(source,total=sum(v,na.rm=TRUE),.grouping=rollup(g),.id="sid",.sort=sort,.margin_label=NULL)
 err <- tryCatch({compute(q,name=name,temporary=FALSE,overwrite=TRUE,indexes=list("g"),analyze=TRUE);NULL},error=identity)
 stopifnot(inherits(err,"marginplyr_error"),grepl("shadows",conditionMessage(err)),
  identical(DBI::dbReadTable(con,DBI::Id(schema="temp",table=base)),sentinel),
  !DBI::dbExistsTable(con,DBI::Id(schema="main",table=base)),
  nrow(DBI::dbGetQuery(con,"SELECT name FROM sqlite_temp_master WHERE type='index'"))==0L)
 cat("PASS ambiguous bare refused",sort,spelling,"\n")
 passed <- passed+1L; DBI::dbDisconnect(con)
}
for(sort in c("none","last")) for(temporary in c(FALSE,TRUE)) {
 con <- DBI::dbConnect(RSQLite::SQLite(),":memory:")
 source <- copy_to(con,tibble(g=c("a","b"),v=c(2,5)),"source")
 q <- summarize_with_margins(source,total=sum(v,na.rm=TRUE),.grouping=rollup(g),.id="sid",.sort=sort,.margin_label=NULL)
 expected <- as.data.frame(collect(q))
 for(overwrite in c(FALSE,TRUE)) {
   m<-compute(q,name="report",temporary=temporary,overwrite=overwrite,indexes=list("g"),analyze=TRUE)
   stopifnot(identical(as.data.frame(collect(m)),expected))
   schema<-if(temporary) "temp" else "main"
   stopifnot(nrow(DBI::dbGetQuery(con,paste0("PRAGMA ",schema,".index_list('report')")))==1L,
     nrow(DBI::dbGetQuery(con,paste0("SELECT * FROM ",schema,".sqlite_stat1 WHERE tbl='report'")))>0L)
   passed<-passed+1L;cat("PASS bare indexes/analyze",sort,temporary,overwrite,"\n")
 }
 DBI::dbDisconnect(con)
}
for(sort in c("none","last")) for(indexes in c(FALSE,TRUE)) {
 con <- DBI::dbConnect(RSQLite::SQLite(),":memory:")
 source <- copy_to(con,tibble(g=c("a","b"),v=c(2,5)),"source")
 sentinel <- data.frame(g="keep",sid=99L,total=-1)
 DBI::dbWriteTable(con,DBI::Id(schema="temp",table="report"),sentinel)
 DBI::dbWriteTable(con,DBI::Id(schema="main",table="report"),sentinel)
 q <- summarize_with_margins(source,total=sum(v,na.rm=TRUE),.grouping=rollup(g),.id="sid",.sort=sort,.margin_label=NULL)
 result <- tryCatch(compute(q,name=DBI::Id(schema="main",table="report"),temporary=FALSE,overwrite=TRUE,indexes=if(indexes)list("g")else list(),analyze=TRUE),error=identity)
 stopifnot(identical(DBI::dbReadTable(con,DBI::Id(schema="temp",table="report")),sentinel))
 if(indexes) {
  stopifnot(inherits(result,"error"),grepl("syntax error",conditionMessage(result)),
  identical(DBI::dbReadTable(con,DBI::Id(schema="main",table="report")),sentinel))
 } else stopifnot(identical(as.data.frame(collect(result)),as.data.frame(collect(q))))
 passed<-passed+1L;cat("PASS explicit main with temp shadow",sort,"indexes",indexes,if(indexes) "ordinary syntax error+rollback" else "correct destination", "\n")
 DBI::dbDisconnect(con)
}
cat("TOTAL",passed,"PASS\n")
for(spelling in c("string","ident","Id","AsIs","literal_dot")) {
 con <- DBI::dbConnect(RSQLite::SQLite(),":memory:")
 base <- if(spelling=="literal_dot") "re.port" else "report"
 name <- switch(spelling,string=base,ident=dbplyr::ident(base),Id=DBI::Id(table=base),AsIs=I(base),literal_dot=base)
 source <- copy_to(con,tibble(g=c("a","b"),v=c(2,5)),"source")
 sentinel <- data.frame(g="keep",sid=99L,total=-1)
 DBI::dbWriteTable(con,DBI::Id(schema="main",table=base),sentinel)
 q <- summarize_with_margins(source,total=sum(v,na.rm=TRUE),.grouping=rollup(g),.id="sid",.sort="last",.margin_label=NULL)
 err <- tryCatch({compute(q,name=name,temporary=TRUE,overwrite=TRUE,indexes=list("g"),analyze=TRUE);NULL},error=identity)
 stopifnot(inherits(err,"marginplyr_error"),grepl("drop a main table",conditionMessage(err)),
  identical(DBI::dbReadTable(con,DBI::Id(schema="main",table=base)),sentinel),
  !DBI::dbExistsTable(con,DBI::Id(schema="temp",table=base)),
  nrow(DBI::dbGetQuery(con,"SELECT name FROM sqlite_master WHERE type='index'"))==0L)
 cat("PASS temp overwrite protects main",spelling,"\n")
 passed<-passed+1L;DBI::dbDisconnect(con)
}
cat("FINAL TOTAL",passed,"PASS\n")
