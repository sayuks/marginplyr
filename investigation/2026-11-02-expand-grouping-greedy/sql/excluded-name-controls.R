# Ordinary DuckDB computation shares the .data name refusal.
Sys.setenv(TESTTHAT_PARALLEL = 'false')
pkgload::load_all(quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
con <- DBI::dbConnect(duckdb::duckdb(shared_home=FALSE),dbdir=':memory:')
dat <- stats::setNames(data.frame(g='a',payload=7L),c('g','.data'))
DBI::dbWriteTable(con,'src',dat)
src <- tbl(con,'src')
for(nm in c('source','expanded')) {
 x <- if(nm=='source') src else expand_with_margins(src,.grouping=rollup(g))
 result <- tryCatch(compute(x,name='out',temporary=TRUE,overwrite=TRUE),error=identity)
 stopifnot(inherits(result,'error'),grepl("Can't select columns with `.data`",conditionMessage(result),fixed=TRUE))
 cat(nm,': ',conditionMessage(result),'\n',sep='')
}
DBI::dbDisconnect(con,shutdown=TRUE)
