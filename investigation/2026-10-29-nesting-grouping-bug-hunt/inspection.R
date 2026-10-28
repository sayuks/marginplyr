pkgload::load_all(commandArgs(TRUE)[[1]], quiet=TRUE)
library(dplyr)
d <- tibble(a=1:3,b=c('x','y','x'),c=factor(c('a','b','a')), z=as.Date('2024-01-01')+1:3)
con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ':memory:')
con_duck <- DBI::dbConnect(duckdb::duckdb())
inputs <- list(local=d, dt=dtplyr::lazy_dt(d), sqlite=copy_to(con_sqlite,d,'d'), duck=copy_to(con_duck,d,'d'), arrow=arrow::Table$create(d), batch=arrow::RecordBatch$create(d))
selectors <- list(quote(a), quote(c(a,b)), quote(where(is.numeric)), quote(where(is.character)), quote(where(is.factor)), quote(where(function(x) inherits(x, 'Date'))), quote(everything()), quote(-b), quote(starts_with('a')), quote(c(1,3)), quote(a:z), quote(!!c('a','b')))
for(i in seq_along(selectors)) {
  ex <- selectors[[i]]
  if(i==length(selectors)) ex <- rlang::expr(!!c('a','b'))
  spec <- eval(rlang::call2('rollup', ex))
  expected <- inspect_grouping(d, .grouping=spec, .format='list')
  for(n in names(inputs)) {
    got <- tryCatch(inspect_grouping(inputs[[n]], .grouping=spec, .format='list'),error=identity)
    if(n=='sqlite' && i %in% 3:6) stopifnot(inherits(got,'marginplyr_error'),grepl("doesn't report column types",conditionMessage(got),fixed=TRUE)) else stopifnot(identical(got,expected))
    cat(i,n,if(inherits(got,'error')) conditionMessage(got) else paste(vapply(got$included,paste,collapse=':',FUN.VALUE=''),collapse='|'),'\n')
  }
}
DBI::dbDisconnect(con_sqlite); DBI::dbDisconnect(con_duck,shutdown=TRUE)
cat('PASS: 72 inspection cases matched local plans or the documented SQLite predicate refusal\n')
