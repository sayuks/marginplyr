pkgload::load_all('.',quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
x <- tibble(g=c('a','b','a'),v=1:3)
con <- DBI::dbConnect(RSQLite::SQLite(), ':memory:')
on.exit <- NULL
remote <- copy_to(con,x,'nest_scope_probe',temporary=TRUE)
arrow_table <- arrow::Table$create(x)
arrow_batch <- arrow::RecordBatch$create(x)
reader <- arrow::RecordBatchReader$create(arrow_batch)
shapes <- list(sqlite=remote,arrow_table=arrow_table,arrow_batch=arrow_batch,arrow_query=select(arrow_table,everything()),arrow_reader=reader)
counter <- 0L
trace('dbGetQuery',where=asNamespace('DBI'),tracer=quote(counter<<-counter+1L),print=FALSE)
for (shape in names(shapes)) for (verb in list(nest_with_margins,nest_by_with_margins)) {
 counter<-0L
 err <- tryCatch(verb(shapes[[shape]],.grouping=rollup(g),.check_margin_label=TRUE),error=identity)
 stopifnot(inherits(err,'marginplyr_error'), counter==0L)
 cat('PASS',shape,if(identical(verb,nest_with_margins))'nest' else 'nest_by','refused before dbGetQuery\n')
}
untrace('dbGetQuery',where=asNamespace('DBI'))
stopifnot(identical(collect(reader),x))
cat('PASS reader remained fully consumable\n')
DBI::dbDisconnect(con)
