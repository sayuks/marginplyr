args <- commandArgs(TRUE)
root <- if (length(args)) args[[1]] else getwd()
pkgload::load_all(root, quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
cons <- list(sqlite = DBI::dbConnect(RSQLite::SQLite(), ':memory:'), duckdb = DBI::dbConnect(duckdb::duckdb(shared_home=FALSE)))
ns <- c('.SDall','.xSD','.iSD','.ROW','.NATURAL','.Last.updated','.SDcols','key','check.names','.EACHI','T','F','NA_integer_','NA_real_','NA_character_','NA_complex_','..0','..00','..01','..-1','...0','a\rb','a\tb','a\\b','a\'b','?','*','.','(x)','x::y','x$y','x[[1]]','é','e\u0301','ß','ẞ','K','Ｋ','\u200b','\u2028',' a ','..marginplyr_parent_original_1','..MARGINPLYR_SET_ID_1')
normalize <- function(z) { z <- as.data.frame(z); for (k in names(z)) { if (is.integer(z[[k]])) { z[[k]] <- as.double(z[[k]]) } }; z <- z[vctrs::vec_order(z), , drop=FALSE]; rownames(z) <- NULL; z }
failures <- list(); checks <- 0L; upstream <- 0L
for (i in seq_along(ns)) for (role in c('fixed','dimension','output','identifier')) {
  n <- ns[[i]]; dat <- data.frame(p=c('p','p','q'),g=c('a','b','a'),x=c(1,3,7))
  p <- if (role=='fixed') n else 'p'; g <- if (role=='dimension') n else 'g'; out <- if (role=='output') n else 'value'; id <- if (role=='identifier') n else 'set'
  names(dat)[1:2] <- c(p,g)
  sources <- list(dtplyr=tryCatch(dtplyr::lazy_dt(dat),error=identity),arrow=tryCatch(arrow::arrow_table(dat),error=identity))
  for (b in names(cons)) { DBI::dbWriteTable(cons[[b]],'inp',dat,overwrite=TRUE); sources[[b]] <- tryCatch(tbl(cons[[b]],'inp'),error=identity) }
  for (b in names(sources)) {
    checks <- checks+1L
    src <- sources[[b]]
    if (inherits(src,'error')) { upstream <- upstream+1L; next }
    f <- function(input) summarize_with_margins(input, !!out := sum(x), .by=all_of(p), .grouping=rollup(all_of(g)), .margin_label=NULL, .sort='last', .id=id)
    ex <- tryCatch(normalize(f(collect(src))),error=identity)
    ac <- tryCatch(normalize(collect(f(src))),error=identity)
    if (inherits(ex,'error')) { upstream <- upstream+1L; next }
    if (inherits(ac,'error') || !isTRUE(all.equal(ex,ac,check.attributes=FALSE))) {
      k <- paste(i,role,b,sep=':'); failures[[k]] <- list(name=n,actual=ac,expected=ex)
      cat('FAIL',k,'name=',encodeString(n),if(inherits(ac,'error'))conditionMessage(ac) else paste(all.equal(ex,ac,check.attributes=FALSE),collapse=';'),'\n')
    }
  }
}
for(con in cons) DBI::dbDisconnect(con)
cat('CHECKS',checks,'UPSTREAM_REFUSALS',upstream,'FAILURES',length(failures),'\n')
stopifnot(length(failures)==0L)
