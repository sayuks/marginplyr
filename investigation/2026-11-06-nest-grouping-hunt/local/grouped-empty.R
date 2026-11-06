Sys.setenv(TESTTHAT_PARALLEL='false')
args <- commandArgs(trailingOnly = TRUE)
pkgload::load_all(if(length(args)) args[[1L]] else '.',quiet=TRUE)
count <- 0L
verbs <- list(nest=nest_with_margins,nest_by=nest_by_with_margins)
for(size in c(0L,1L,6L)) for(drop in c(FALSE,TRUE)) for(keep in c(FALSE,TRUE)) for(sort in c('none','first','last')) for(verb in names(verbs)) {
 x <- tibble::tibble(fixed=factor(c('n','s',NA,'n','s',NA)[seq_len(size)],levels=c('unused','s','n')),g=c('a','b',NA,'a',NA,'b')[seq_len(size)],row=seq_len(size))
 grouped <- dplyr::group_by(x,fixed,.drop=drop)
 before <- serialize(grouped,NULL)
 actual <- tryCatch(verbs[[verb]](grouped,.grouping=rollup(g),.keep=keep,.sort=sort,.margin_label=NULL,.id='set'),error=identity)
 if(!drop) { stopifnot(inherits(actual,'marginplyr_error'),grepl('.drop = FALSE',conditionMessage(actual),fixed=TRUE),identical(serialize(grouped,NULL),before)); count<-count+1L; next }
 expected <- verbs[[verb]](x,.by=fixed,.grouping=rollup(g),.keep=keep,.sort=sort,.margin_label=NULL,.id='set')
 stopifnot(identical(actual,expected),identical(serialize(grouped,NULL),before))
 count <- count+1L
}
cat('grouped equivalence and source serialization:',count,'PASS\n')
# Input rowwise is a documented refusal rather than implicit fixed grouping.
x <- dplyr::rowwise(tibble::tibble(g='a',row=1L),g)
for(verb in verbs) {
 y <- tryCatch(verb(x,.grouping=rollup(g)),error=identity)
 stopifnot(inherits(y,'marginplyr_error'))
}
cat('rowwise input refusal: 2 PASS\n')
# Both APIs promise row-sized zero-column cells and different keyless empty shapes.
count <- 0L
for(size in c(0L,1L,7L)) for(verb in names(verbs)) for(keep in c(FALSE,TRUE)) {
 x <- tibble::new_tibble(list(),nrow=size)
 actual <- verbs[[verb]](x,.keep=keep,.id='set')
 outer <- as.integer(size>0L || verb=='nest_by')
 stopifnot(nrow(actual)==outer,identical(actual$set,rep(1L,outer)))
 if(outer) stopifnot(identical(dim(actual$data[[1L]]),c(size,0L)))
 count <- count+1L
}
cat('zero-column inputs:',count,'PASS\n')
