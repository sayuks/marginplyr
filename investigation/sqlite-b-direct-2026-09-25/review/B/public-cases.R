# THROWAWAY: run public APIs on an isolated package copy and disposable SQLite DBs.
args <- commandArgs(TRUE)
package_path <- if (length(args)) args[[1L]] else "/private/tmp/marginplyr-sqlite-design-20260925/variants/B"
pkgload::load_all(package_path, quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
results <- list()
same <- function(x,y) identical(as.data.frame(x), as.data.frame(y))
check <- function(name, code) {
  cat("CASE", name, "\n")
  result <- tryCatch({ force(code); "PASS" }, error = function(e) paste("FAIL",conditionMessage(e)))
  cat(result,"\n")
  results[[length(results)+1L]] <<- data.frame(case=name, result=result)
}
fresh <- function(code) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  eval(substitute(code), envir = environment())
}
no_stage <- function(con) {
  tabs <- DBI::dbGetQuery(con,"SELECT name FROM sqlite_temp_master WHERE type='table'")$name
  stopifnot(!any(grepl("^marginplyr_(typed|order)_",tabs)))
}
query <- function(source, sort="none", label="Total") {
  summarize_with_margins(source,total=sum(v,na.rm=TRUE),.grouping=rollup(g),
                         .id="sid",.sort=sort,.margin_label=label)
}
cat("ENVIRONMENT\n"); print(sessionInfo())
for (sort in c("none","first","last")) for (ident in c("schema","DBI")) {
 for (analyze in c(FALSE,TRUE)) for (overwrite in c(FALSE,TRUE)) {
  check(paste("661",sort,ident,"analyze",analyze,"overwrite",overwrite),fresh({
    DBI::dbExecute(con,"ATTACH DATABASE ':memory:' AS other")
    source <- copy_to(con,tibble(g=c("a","b"),v=c(2,5)),"source")
    sentinel <- data.frame(g="keep",sid=99L,total=-1)
    DBI::dbWriteTable(con,DBI::Id(schema="main",table="report"),sentinel)
    if(overwrite) DBI::dbWriteTable(con,DBI::Id(schema="other",table="report"),sentinel)
    q <- query(source, sort, if(sort=="none") "Total" else NULL)
    expected <- collect(q)
    target <- if(ident=="schema") dbplyr::in_schema("other","report") else DBI::Id(schema="other",table="report")
    m <- compute(q,name=target,temporary=FALSE,overwrite=overwrite,analyze=analyze)
    stopifnot(same(collect(m),expected), identical(DBI::dbReadTable(con,DBI::Id(schema="main",table="report")),sentinel))
    observed <- DBI::dbReadTable(con,DBI::Id(schema="other",table="report"))
    stopifnot(identical(names(observed),names(expected)),nrow(observed)==3L, sum(observed$total)==14)
    if(analyze) stopifnot(nrow(DBI::dbGetQuery(con,"SELECT * FROM other.sqlite_stat1 WHERE tbl='report'"))>0L)
    stopifnot(identical(collect(source),tibble(g=c("a","b"),v=c(2,5))))
    no_stage(con)
  }))
 }
}
for(sort in c("none","last")) for(outer in c(FALSE,TRUE)) for(inner in c(FALSE,TRUE)) {
 for(fail in c(FALSE,TRUE)) {
  check(paste("662",sort,"caller",outer,"in_transaction",inner,"failure",fail),fresh({
    source <- copy_to(con,tibble(g=c("a","b"),v=c(2,5)),"source")
    DBI::dbWriteTable(con,"marker",data.frame(value=1L))
    original <- data.frame(g="keep",sid=99L,total=-1)
    DBI::dbWriteTable(con,"report",original,temporary=TRUE)
    if(outer) {DBI::dbBegin(con); DBI::dbExecute(con,"INSERT INTO marker VALUES (2)")}
    q <- query(source,sort,if(sort=="none") "Total" else NULL)
    expected <- collect(q)
    if(fail) {
      err <- tryCatch({compute(q,name="report",overwrite=TRUE,analyze=FALSE,in_transaction=inner,
                      unique_indexes=list("sid"));NULL},error=identity)
      stopifnot(inherits(err,"error"), identical(DBI::dbReadTable(con,"report"),original))
    } else {
      m <- compute(q,name="report",overwrite=TRUE,analyze=FALSE,in_transaction=inner)
      stopifnot(same(collect(m),expected))
    }
    if(outer) {
      stopifnot(identical(DBI::dbReadTable(con,"marker")$value,1:2))
      DBI::dbRollback(con)
      stopifnot(identical(DBI::dbReadTable(con,"report"),original), identical(DBI::dbReadTable(con,"marker")$value,1L))
    }
    no_stage(con)
    stopifnot(identical(collect(source),tibble(g=c("a","b"),v=c(2,5))))
    m <- compute(q,name="after",analyze=FALSE)
    stopifnot(same(collect(m),expected));no_stage(con)
  }))
 }
}
for(sort in c("first","last")) {
 check(paste("663 downstream",sort),fresh({
   source <- copy_to(con,tibble(g=c("a","b"),v=1:2),"source")
   q <- query(source,sort,NULL); expected <- collect(q); m <- compute(q,name="materialized")
   stopifnot(same(collect(m),expected), identical(collect(select(m,g))$g,expected$g),
      identical(names(collect(rename(m,value=total))),c("g","sid","value")),
      nrow(collect(filter(m,total>1)))==2L, identical(collect(mutate(m,value=total*2))$value,expected$total*2),
      identical(sort(collect(arrange(m,total))$total),c(1L,2L,3L)),
      identical(DBI::dbListFields(con,"materialized"),names(expected)))
 }))
 check(paste("overwrite source",sort),fresh({
   source <- copy_to(con,tibble(g=c("a","b"),v=1:2),"source",temporary=FALSE)
   q <- query(source,sort,NULL); expected <- collect(q)
   m <- tryCatch(compute(q,name="source",temporary=FALSE,overwrite=TRUE,analyze=FALSE),error=identity)
   if(inherits(m,"error")) {
     cat("self-overwrite refused:",conditionMessage(m),"\n")
     stopifnot(identical(DBI::dbReadTable(con,"source"),data.frame(g=c("a","b"),v=1:2)))
   } else {
     cat("self-overwrite succeeded\n")
     stopifnot(same(collect(m),expected))
   }
   no_stage(con)
 }))
}
for(sort in c("none","first","last")) for(label in list("Total",NULL,NA_character_)) for(explicit in c(FALSE,TRUE)) {
 check(paste("665",sort,if(is.null(label)) "NULL" else label,"explicit",explicit),fresh({
   source <- copy_to(con,tibble(x=1L),"source")
   empty <- filter(source,x>1L)
   q <- if(explicit) expand_with_margins(empty,.grouping=rollup(x),.id="set",.sort=sort,.margin_label=label) else expand_with_margins(empty,.id="set",.sort=sort,.margin_label=label)
   for(out in list(collect(q),collect(compute(q)))) {
     stopifnot(nrow(out)==0L,identical(out$set,integer()),identical(names(out),if(explicit) c("x","set") else c("set","x")))
   }
   stopifnot(identical(collect(source),tibble(x=1L))); no_stage(con)
 }))
}
for(sort in c("none","first","last")) for(label in list("Total",NULL)) for(fun in c("sum","mean")) {
 check(paste("666",sort,if(is.null(label)) "NULL" else label,fun),fresh({
   source <- copy_to(con,tibble(fixed=c("A","A","B"),g=c("x","y","z"),v=c(2,-2,3)),"source")
   q <- summarize_with_margins(source,total=!!rlang::call2(fun,quote(v),na.rm=TRUE),
      parent=share_of_parent(total), share=share_of_total(total),
      across(total,share_of_total,.names="{.col}_across"),
      .by=fixed,.grouping=rollup(g),.sort=sort,.margin_label=label,.check_share_source=FALSE)
   full <- collect(q); m <- compute(q)
   stopifnot(same(collect(m),full),nrow(full)==5L)
   for(n in c(0L,1L,2L,3L,5L,Inf)) {
     out <- collect(q,n=n); materialized <- collect(m,n=n)
     for(col in c("parent","share","total_across")) stopifnot(typeof(out[[col]])=="double",typeof(materialized[[col]])=="double")
     if(n>0) stopifnot(same(out,head(full,n=if(is.finite(n)) n else nrow(full))),same(out,materialized))
   }
   if(sort=="last") stopifnot(is.na(collect(q,n=1)$share),is.na(collect(q,n=1)$parent))
   no_stage(con)
 }))
}
check("666 original missing numerator",fresh({
 source <- copy_to(con,tibble(g=c("a","b"),v=c(NA_real_,1)),"source")
 q <- summarize_with_margins(source,total=sum(v,na.rm=TRUE),share=share_of_total(total),.grouping=rollup(g),.sort="last",.check_share_source=FALSE)
 stopifnot(identical(collect(q)$share,c(NA_real_,1,1)),identical(collect(q,n=1)$share,NA_real_),identical(collect(compute(q),n=1)$share,NA_real_))
}))
check("all rowid aliases remain refused before materialization",fresh({
 source <- copy_to(con,tibble(rowid=1L,oid=2L,`_rowid_`=3L,v=4),"source")
 q <- summarize_with_margins(source,total=sum(v,na.rm=TRUE),.grouping=rollup(rowid,oid,`_rowid_`),.sort="last",.margin_label=NULL)
 err <- tryCatch({compute(q,name="report"); NULL},error=identity)
 stopifnot(inherits(err,"marginplyr_error"),!DBI::dbExistsTable(con,"report"));no_stage(con)
}))
check("audit result remains exact direct SQL; compute adds no audit rows",fresh({
 source <- copy_to(con,tibble(g=c("a","b"),v=c(2,5)),"source")
 old <- options(marginplyr.audit_sql=TRUE); on.exit(options(old),add=TRUE)
 q <- query(source,"last",NULL)
 before <- last_sent_queries()
 stopifnot(identical(before$purpose,"result"),identical(before$sql,as.character(dbplyr::sql_render(q))))
 m <- compute(q)
 stopifnot(identical(last_sent_queries(),before))
}))
rows <- do.call(rbind,results)
print(rows, row.names=FALSE)
cat("TOTAL",nrow(rows),"PASS",sum(rows$result=="PASS"),"FAIL",sum(rows$result!="PASS"),"\n")
write.csv(rows,paste0("/private/tmp/marginplyr-sqlite-design-20260925/review/B/public-cases-",basename(package_path),"-results.csv"),row.names=FALSE)
if(any(rows$result!="PASS")) quit(status=1L)
