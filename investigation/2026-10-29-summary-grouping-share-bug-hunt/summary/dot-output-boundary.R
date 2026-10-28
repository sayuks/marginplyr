suppressPackageStartupMessages(library(dplyr))
args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args)) normalizePath(args[[1L]]) else getwd()
devtools::load_all(repo, quiet = TRUE)
con_sq<-DBI::dbConnect(RSQLite::SQLite(),':memory:');con_du<-DBI::dbConnect(duckdb::duckdb(shared_home=FALSE))
d<-tibble(v=c(1,3));dots<-setNames(rlang::quos(sum(v)),'.')
cap<-function(e)tryCatch(collect(e),error=function(c)paste('ERROR',conditionMessage(c)))
for(src in list(dtplyr::lazy_dt(d),copy_to(con_sq,d,'data'))){
 cat('CLASS',class(src),'\nDYNAMIC\n');print(cap(summarise(.data=src,!!!dots,.by=NULL)))
 cat('EARLY INJECTION\n');print(cap(rlang::inject(summarise(.data=src,!!!dots,.by=NULL))))
}
src<-copy_to(con_du,d,'data');cat('DUCKDB PORTABLE\n');print(cap(summarize_with_margins(src,!!!dots,.id='set',.duplicates='keep')))
DBI::dbDisconnect(con_sq);DBI::dbDisconnect(con_du,shutdown=TRUE)
