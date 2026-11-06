Sys.setenv(TESTTHAT_PARALLEL='false')
args <- commandArgs(trailingOnly=TRUE)
pkgload::load_all(if(length(args)) args[[1L]] else '.',quiet=TRUE)
helpers <- c('list','as_tibble','seq_len','min','vec_slice','n','summarize','rowwise','is.na','if_else')
count <- 0L
for(helper in helpers) for(empty in c(FALSE,TRUE)) for(keep in c(FALSE,TRUE)) for(verb in c('nest_with_margins','nest_by_with_margins')) {
 env <- new.env(parent=globalenv()); env$input <- tibble::tibble(g=c('a','b'),v=1:2)
 if(empty) env$input <- env$input[0L,]
 env$calls <- 0L
 env[[helper]] <- function(...) {env$calls <- env$calls+1L; stop('Caller binding ran')}
 env$verb <- get(verb); env$keep <- keep
 out <- evalq(verb(input,.grouping=rollup(g),.keep=keep,.sort='last',.id='set'),env)
 stopifnot(env$calls==0L,nrow(out)==if(empty) 0L else 3L)
 if(!empty) stopifnot(identical(lapply(out$data,function(x)x$v),list(1L,2L,1:2)))
 count <- count+1L
}
cat('Local caller-binding comparisons:',count,'all matched; no masked helper invoked.\n')
