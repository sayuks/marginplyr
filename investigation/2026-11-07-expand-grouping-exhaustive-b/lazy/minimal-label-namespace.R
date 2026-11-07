Sys.setenv(TESTTHAT_PARALLEL='false')
args <- commandArgs(trailingOnly=TRUE)
source_dir <- normalizePath(args[[1L]],mustWork=TRUE)
pkgload::load_all(source_dir,quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
root <- new.env(parent=globalenv())
root$source <- data.table::data.table(g='north',value=13L)
input <- evalq(dtplyr::lazy_dt(source,immutable=TRUE),root)
query <- expand_with_margins(input,.grouping=grouping_set(g))
expected <- collect(query)
callbacks <- 0L
root$`:::` <- function(pkg,name) {
 package <- as.character(substitute(pkg)); member <- as.character(substitute(name))
 if(identical(c(package,member),c('marginplyr','margin_display_character'))) {
  callbacks <<- callbacks+1L
  return(function(x) rep('MASK',length(x)))
 }
 getFromNamespace(member,package)
}
ordinary <- collect(input); got <- collect(query)
cat('Ordinary source:\n');print(ordinary)
cat('Expected expansion:\n');print(expected)
cat('Actual expansion:\n');print(got);cat('callbacks:',callbacks,'\n')
rm(list=':::',envir=root)
restored <- collect(query)
stopifnot(identical(ordinary$g,'north'),identical(got$g,'MASK'),callbacks==1L,identical(restored,expected))
if('--assert' %in% commandArgs(trailingOnly=TRUE)) stopifnot(identical(got,expected))
