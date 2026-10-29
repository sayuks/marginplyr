# Investigated 2026-10-29 against 59d55a7867ffc0255f75be32a233d9d475bcb7fb.
# Run with Rscript <script> [repository-root].
args <- commandArgs(trailingOnly = TRUE)
positional <- args[!startsWith(args, "--")]
repo <- if (length(positional)) positional[[1L]] else "."
pkgload::load_all(repo, quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
# Exercise two reader-containing nodes and repeated inspection on composed graphs.
arrow_checks <- 0L
for (case in c('join-left-union-right','union-left-join-right','join-both','union-both')) {
  x <- tibble(g = c('a','b'), v = 1:2)
  reader1 <- arrow::RecordBatchReader$create(arrow::RecordBatch$create(x))
  reader2 <- arrow::RecordBatchReader$create(arrow::RecordBatch$create(x))
  q1 <- select(reader1, everything()); q2 <- select(reader2, everything())
  table <- arrow::Table$create(x)
  q <- switch(case,
    'join-left-union-right' = inner_join(q1, union_all(table, q2), by='g'),
    'union-left-join-right' = union_all(q1, select(inner_join(table, q2, by='g'),g,v=v.x)),
    'join-both' = inner_join(q1, q2, by='g'),
    'union-both' = union_all(q1,q2))
  for (typed in c(FALSE,TRUE)) for (repeat_inspection in seq_len(2L)) {
    got <- if(typed) inspect_grouping(q,.grouping=rollup(where(is.character))) else inspect_grouping(q,.grouping=rollup(g))
    stopifnot(identical(got$included,c('(g)','()'))); arrow_checks <- arrow_checks+1L
  }
  for (verb in list(nest_with_margins,nest_by_with_margins)) {
    e <- tryCatch(verb(q,.grouping=rollup(g)),error=identity)
    stopifnot(inherits(e,'marginplyr_error'));arrow_checks<-arrow_checks+1L
  }
  stopifnot(identical(as_tibble(as.data.frame(reader1)),x),identical(as_tibble(as.data.frame(reader2)),x))
}
cat('Arrow graph boundary calls',arrow_checks,'readers preserved 8\n')
# Count live SQL queries while refusing nesting or inspecting derived relations.
sql_checks <- 0L
for(engine in c('sqlite','duckdb')) {
 con <- if(engine=='sqlite') DBI::dbConnect(RSQLite::SQLite(),':memory:') else DBI::dbConnect(duckdb::duckdb(shared_home=FALSE))
 src <- copy_to(con,tibble(g=c('b','a'),v=1:2),'boundary_input',temporary=TRUE)
 shapes <- list(root=src,derived=head(arrange(filter(src,v>0L),v),1L),
                union=union_all(src,src),join=left_join(src,rename(src,w=v),by='g'))
 statements <- character()
 trace('dbSendQuery',where=asNamespace('DBI'),print=FALSE,
   tracer=quote({statements<<-c(statements,as.character(statement))}))
 for(shape in names(shapes)) {
  q <- shapes[[shape]]
  for(verb in list(nest_with_margins,nest_by_with_margins)){
   e<-tryCatch(verb(q,.grouping=rollup(g)),error=identity)
   stopifnot(inherits(e,'marginplyr_error'),length(statements)==0L);sql_checks<-sql_checks+1L
  }
 }
 for(shape in names(shapes)) {
  q <- shapes[[shape]]
  n <- length(statements)
  plan <- inspect_grouping(q,.grouping=rollup(g))
  stopifnot(identical(plan$included,c('(g)','()')),length(statements)==n+as.integer(engine=='duckdb'))
  n <- length(statements)
  typed <- tryCatch(inspect_grouping(q,.grouping=rollup(where(is.character))),error=identity)
  if(engine=='sqlite')stopifnot(inherits(typed,'marginplyr_error'),length(statements)==n)
  else stopifnot(identical(typed$included,c('(g)','()')),length(statements)==n+1L)
  sql_checks<-sql_checks+2L
 }
 if(length(statements)) stopifnot(all(grepl('LIMIT 0',statements,fixed=TRUE)))
 untrace('dbSendQuery',where=asNamespace('DBI'))
 cat('SQL',engine,'statements',length(statements),'all LIMIT 0 when present\n')
 DBI::dbDisconnect(con,shutdown=engine=='duckdb')
}
cat('SQL boundary calls',sql_checks,'\n')
# Unknown metadata transformations are refused before rows or callbacks run.
seen <- integer()
callback <- function(x){seen<<-c(seen,length(x));x}
root<-dtplyr::lazy_dt(data.frame(g=c('a','b'),v=1:2))
unsafe<-mutate(root,new=callback(v))
for(q in list(unsafe,arrange(compute(unsafe),g),union_all(unsafe,unsafe),left_join(unsafe,root,by='g'))){
 for(verb in list(function(x)inspect_grouping(x,.grouping=rollup(where(is.character))),
                  function(x)nest_with_margins(x,.grouping=rollup(g)),
                  function(x)nest_by_with_margins(x,.grouping=rollup(g)))){
  e<-tryCatch(verb(q),error=identity)
  stopifnot(inherits(e,'marginplyr_error'),length(seen)==0L)
 }
}
cat('Unsafe metadata refusals 12; callback calls 0\n')
