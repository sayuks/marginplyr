args <- commandArgs(TRUE)
pkgload::load_all(if(length(args)) args[[1]] else getwd(), quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
set.seed(28471)
cons <- list(sqlite=DBI::dbConnect(RSQLite::SQLite(), ':memory:'), duckdb=DBI::dbConnect(duckdb::duckdb(shared_home=FALSE)))
expected_refusals <- 0L
failures <- list(); checks <- 0L
normalize <- function(x) {
  x <- as.data.frame(x)
  for (n in names(x)) { if (is.factor(x[[n]])) x[[n]] <- as.character(x[[n]]); if (is.integer(x[[n]])) x[[n]] <- as.double(x[[n]]) }
  x <- x[vctrs::vec_order(x),,drop=FALSE]; rownames(x) <- NULL; x
}
for (i in seq_len(40L)) {
  n <- sample(1:15,1)
  g <- switch(as.character(i %% 5), '0'=sample(c('a','b',NA_character_),n,TRUE), '1'=sample(c(1,2,NA_real_),n,TRUE), '2'=sample(c(FALSE,TRUE,NA),n,TRUE), '3'=factor(sample(c('a','b',NA_character_),n,TRUE),levels=c('b','a','unused')), '4'=sample(c(1.000000000001,1.000000000002,NA_real_),n,TRUE))
  d <- data.frame(f=sample(c('x','y',NA_character_),n,TRUE),g=g,h=sample(c('j','k',NA_character_),n,TRUE),v=sample(1:20,n,TRUE))
  spec <- switch(as.character(i %% 3),'0'=rollup(g,h),'1'=cube(g,h),'2'=grouping_sets(grouping_set(),grouping_set(h),grouping_set(g,h)))
  sources <- list(dtplyr=dtplyr::lazy_dt(d),arrow=arrow::arrow_table(d))
  for (b in names(cons)) { DBI::dbWriteTable(cons[[b]],'input',d,overwrite=TRUE); sources[[b]] <- tbl(cons[[b]], 'input') }
  for (b in names(sources)) {
    src <- sources[[b]]
    if (i %% 4 == 0) src <- filter(src,v > 5)
    if (i %% 4 == 1) src <- head(src, ceiling(n/2))
    if (i %% 4 == 2) src <- mutate(src,v=v+1)
    raw <- collect(src)
    for (s in c('none','last','first')) {
      checks <- checks+1L
      callit <- function(x) summarize_with_margins(x,z=sum(v),n=dplyr::n(),gb=grouping_bit(g),gh=grouping_bit(h),.grouping=spec,.by=f,.margin_label=NULL,.sort=s,.id='set')
      expected <- tryCatch(normalize(callit(raw)),error=function(e)e)
      actual <- tryCatch(normalize(collect(callit(src))),error=function(e)e)
      ok <- !inherits(actual,'error') && !inherits(expected,'error') && isTRUE(all.equal(actual,expected,check.attributes=FALSE))
      if (!ok && b == 'dtplyr' && i %% 4 == 1 && inherits(actual,'error') && grepl('safely determine column metadata',conditionMessage(actual),fixed=TRUE)) { expected_refusals <- expected_refusals+1L; next }
      if (!ok) {
        k<-paste(i,b,s,sep=':'); failures[[k]]<-list(i=i,backend=b,sort=s,data=d,actual=actual,expected=expected)
        cat('FAIL',k, if(inherits(actual,'error')) conditionMessage(actual) else paste(all.equal(actual,expected,check.attributes=FALSE),collapse=';'), '\n')
      }
    }
  }
  cat('iteration',i,'checks',checks,'failures',length(failures),'\n')
}

for (c in cons) DBI::dbDisconnect(c)
cat('TOTAL',checks,'MATCHED',checks-expected_refusals-length(failures),'EXPECTED_REFUSALS',expected_refusals,'FAILURES',length(failures),'\n'); stopifnot(length(failures)==0L)
