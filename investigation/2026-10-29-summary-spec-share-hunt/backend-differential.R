pkgload::load_all('.', quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
cons <- list(sqlite=DBI::dbConnect(RSQLite::SQLite(), ':memory:'), duckdb=DBI::dbConnect(duckdb::duckdb(shared_home=FALSE), dbdir=':memory:'))
on.exit <- function() invisible(NULL)
set.seed(193061)
canonical <- function(x) {
  x <- as.data.frame(x)
  x[] <- lapply(x, function(z) if(is.factor(z)) as.character(z) else if(inherits(z, 'integer64')) as.double(z) else z)
  x <- x[do.call(order,c(x, list(na.last=TRUE))), , drop=FALSE]
  rownames(x) <- NULL
  x
}
errors <- list(); checks <- 0L
for (case in seq_len(30)) {
 n <- sample(c(0:7,12),1)
 d <- data.frame(f=sample(c('P','Q',NA_character_),n,TRUE),a=sample(c('a','b',NA_character_),n,TRUE),b=sample(c('u','v',NA_character_),n,TRUE),v=sample(1:10,n,TRUE))
 specs <- list(rollup=rollup(a,b),cube=cube(a,b),product=grouping_spec(rollup(a),rollup(b)),duplicate=grouping_sets(rollup(a,b),grouping_set()),sparse=grouping_sets(grouping_set(a),grouping_set(b),grouping_set()))
 inputs <- list(local=d,dt=dtplyr::lazy_dt(d),arrow=arrow::Table$create(d),sqlite=copy_to(cons$sqlite,d,paste0('t',case),temporary=TRUE),duckdb=copy_to(cons$duckdb,d,paste0('t',case),temporary=TRUE))
 for (sn in names(specs)) for(fixed in c(FALSE,TRUE)) for(so in c('none','first','last')) {
   spec <- specs[[sn]]
   run <- function(input) summarize_with_margins(input,z=sum(v),n=dplyr::n(),gid=grouping_id(a,b),.by=if(fixed) all_of('f') else NULL,.grouping=spec,.duplicates='keep',.id='id',.margin_label=NULL,.sort=so) |> collect()
   expected <- canonical(run(d))
   for(bn in names(inputs)[-1]) {
    checks <- checks+1L
    got <- tryCatch(canonical(run(inputs[[bn]])),error=identity)
    if(inherits(got,'error') || !isTRUE(all.equal(expected,got,check.attributes=FALSE))) {
      key<-paste(case,sn,fixed,so,bn,sep='/')
      errors[[key]]<-list(data=d,expected=expected,got=got)
      cat('FAIL',key,if(inherits(got,'error')) conditionMessage(got) else paste(all.equal(expected,got,check.attributes=FALSE),collapse='; '),'\n')
    }
   }
 }
 cat('case',case,'checks',checks,'failures',length(errors),'\n')
}
saveRDS(errors,'investigation/2026-10-29-summary-spec-share-hunt/backend-differential-results.rds')
for(co in cons) DBI::dbDisconnect(co)
cat('TOTAL',checks,'FAILURES',length(errors),'\n')
