# Dated investigation evidence, investigated 2026-10-26 at 6b91254016dbcce60cc2cc69071f01f69eb8084e.
# Run from the repository root, or pass its path as the first argument.
args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args)) args[[1L]] else "."
pkgload::load_all(repo, quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
x <- tibble(g=c('a','b'),v=1:2)
# Count actual public DBI query entry for live SQL calls, excluding input setup.
for (engine in c('sqlite','duckdb')) {
  con <- if(engine=='sqlite') DBI::dbConnect(RSQLite::SQLite(),':memory:') else DBI::dbConnect(duckdb::duckdb(shared_home=FALSE),dbdir=':memory:')
  remote <- copy_to(con,x,'backend_contracts',temporary=TRUE)
  query_counter <- 0L
  query_statements <- character()
  trace('dbSendQuery',where=asNamespace('DBI'),tracer=quote({query_counter <<- query_counter+1L;query_statements <<- c(query_statements,as.character(statement))}),print=FALSE)
  for(verb in list(nest_with_margins,nest_by_with_margins)) {
    err <- tryCatch(verb(remote,.grouping=rollup(g)),error=identity)
    stopifnot(inherits(err,'marginplyr_error'),query_counter==0L)
  }
  got <- inspect_grouping(remote,.grouping=rollup(g))
  stopifnot(identical(got$included,c('(g)','()')),query_counter==if(engine=='sqlite')0L else 1L)
  typed <- tryCatch(inspect_grouping(remote,.grouping=rollup(where(is.character))),error=identity)
  if(engine=='sqlite') stopifnot(inherits(typed,'marginplyr_error'),query_counter==0L)
  else stopifnot(identical(typed$included,c('(g)','()')),query_counter==2L)
  if(engine=='duckdb')stopifnot(length(query_statements)==2L,all(grepl('LIMIT 0',query_statements,fixed=TRUE)))
  untrace('dbSendQuery',where=asNamespace('DBI'))
  cat('PASS SQL',engine,'nest refusals 2 with 0 queries; name-only plus typed inspection total queries',query_counter,'\n')
  DBI::dbDisconnect(con,shutdown=engine=='duckdb')
}
# Inspect Arrow reader-containing graphs repeatedly, then consume the reader.
# A full original batch after inspection/refusals proves no hidden consumption.
arrow_cases <- c('direct','wrapped','join-right','union-right')
for (case in arrow_cases) {
 reader <- arrow::RecordBatchReader$create(arrow::RecordBatch$create(x))
 wrapped <- select(reader,everything())
 input <- switch(case,direct=reader,wrapped=wrapped,
   'join-right'=inner_join(arrow::Table$create(x),wrapped,by='g'),
   'union-right'=union_all(arrow::Table$create(x),wrapped))
 for(typed in c(FALSE,TRUE)) {
   got <- if(typed) inspect_grouping(input,.grouping=rollup(where(is.character))) else inspect_grouping(input,.grouping=rollup(g))
   stopifnot(identical(got$included,c('(g)','()')))
 }
 for(verb in list(nest_with_margins,nest_by_with_margins)) {
   err<-tryCatch(verb(input,.grouping=rollup(g)),error=identity)
   stopifnot(inherits(err,'marginplyr_error'))
 }
 stopifnot(identical(as_tibble(as.data.frame(reader)),x))
 cat('PASS Arrow reader graph',case,'inspection 2; nesting refusals 2; complete reader retained\n')
}
# Metadata-safe subsets may carry arbitrary row predicates; none is executed.
predicate_calls<-0L
predicate<-function(v){predicate_calls<<-predicate_calls+1L;rep(TRUE,length(v))}
s<-dtplyr::lazy_dt(x)|>filter(predicate(v))
inspect_grouping(s,.grouping=rollup(where(is.character)))
r<-nest_with_margins(s,.grouping=rollup(g));invisible(show_query(r))
stopifnot(predicate_calls==0L)
invisible(collect(r));stopifnot(predicate_calls==2L)
# Unknown value-derived schema is refused before executing its function.
value_calls<-0L
unsafe<-dtplyr::lazy_dt(x)|>mutate(z={value_calls<<-value_calls+1L;v})
for(f in list(function()inspect_grouping(unsafe,.grouping=rollup(where(is.character))),function()nest_with_margins(unsafe,.grouping=rollup(g)))) {
 err<-tryCatch(f(),error=identity);stopifnot(inherits(err,'marginplyr_error'),value_calls==0L)
}
cat('PASS dtplyr source execution: predicate calls 0 before collection, 2 after; unsafe transformation calls 0 across 2 refusals\n')
# Every mutable graph root stays unchanged through typed inspection and refusal.
for(case in c('root','filter','select','join','union')) {
 left<-data.table::data.table(g=c('a','b'),v=1:2);right<-data.table::data.table(g=c('a','b'),v=3:4)
 left_before<-data.table::copy(left);right_before<-data.table::copy(right)
 l<-dtplyr::lazy_dt(left,immutable=FALSE);r<-dtplyr::lazy_dt(right,immutable=FALSE)
 s<-switch(case,root=l,filter=filter(l,v>0),select=select(l,g),join=inner_join(l,r,by='g'),union=union_all(l,r))
 got<-inspect_grouping(s,.grouping=rollup(where(is.character)))
 stopifnot(identical(got$included,c('(g)','()')))
 for(verb in list(nest_with_margins,nest_by_with_margins)) {
  err<-tryCatch(verb(s,.grouping=rollup(g)),error=identity);stopifnot(inherits(err,'marginplyr_error'))
 }
 stopifnot(identical(left,left_before),identical(right,right_before))
 cat('PASS mutable dtplyr graph',case,'typed inspection 1; nesting refusals 2; roots unchanged\n')
}
