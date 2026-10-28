pkgload::load_all('.',quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
x <- tibble(g=c('a','b'),i=1:2,d=c(1.2,2.3),l=c(TRUE,FALSE),f=factor(c('x','y')),day=as.Date('2026-10-01')+0:1,ts=as.POSIXct('2026-10-01',tz='UTC')+0:1)
con <- DBI::dbConnect(duckdb::duckdb(shared_home=FALSE),dbdir=':memory:')
sources <- list(local=x,dtplyr=dtplyr::lazy_dt(x),arrow_table=arrow::Table$create(x),arrow_batch=arrow::record_batch(x),duckdb=copy_to(con,x,'metadata_types'))
preds <- list(is.integer,is.numeric,is.character,is.factor,is.logical,is.list,function(v)inherits(v,'Date'),function(v)inherits(v,'POSIXct'))
constructors <- c('grouping_set','grouping_sets','rollup','cube','grouping_spec')
comparisons<-0L; failures<-0L
for(s in names(sources)) {
  input<-sources[[s]]; reference<-if(s=='local') x else collect(input)
  for(p in preds) for(ctor in constructors) {
    spec <- rlang::inject((get(ctor))(where(p)))
    actual <- tryCatch(inspect_grouping(input,.grouping=spec,.format='list'),error=identity)
    expected <- tryCatch(inspect_grouping(reference,.grouping=spec,.format='list'),error=identity)
    ok <- if(inherits(expected,'error')) inherits(actual,'marginplyr_error') else identical(actual,expected)
    comparisons<-comparisons+1L
    if(!ok) {failures<-failures+1L;cat('FAIL',s,ctor,'\n');print(actual);print(expected)}
  }
  for(p in preds) {
    actual<-inspect_grouping(input,.by=where(p),.format='list')
    expected<-inspect_grouping(reference,.by=where(p),.format='list')
    comparisons<-comparisons+1L
    if(!identical(actual,expected)) {failures<-failures+1L;cat('FAIL fixed',s,'\n')}
  }
}
DBI::dbDisconnect(con,shutdown=TRUE)
cat('comparisons:',comparisons,'failures:',failures,'\n')
stopifnot(failures==0L)
