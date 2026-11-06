Sys.setenv(TESTTHAT_PARALLEL='false')
args <- commandArgs(trailingOnly=TRUE)
pkgload::load_all(if(length(args)) args[[1L]] else '.',quiet=TRUE)
nms <- c('data','.data','.env','.rows','...','..1','..0','..01','.SD','list','nrow','{x}','a\nb','日本語','..marginplyr_nest_1','..marginplyr_column_1')
attempts <- 0L; refusals <- 0L
for(nm in nms) for(role in c('key','payload')) for(empty in c(FALSE,TRUE)) for(verb in list(nest_with_margins,nest_by_with_margins)) for(keep in c(FALSE,TRUE)) {
 input <- tibble::tibble(g=c('a','a','b'),row_id=1:3,v=3:1)
 if(role=='payload') names(input)[3L] <- nm
 if(empty) input <- vctrs::vec_slice(input,integer())
 before <- serialize(input,NULL)
 key <- if(role=='key') nm else 'cell'
 out <- tryCatch(verb(input,.grouping=rollup(g),.id='set',.key=key,.keep=keep,.sort='last'),error=identity)
 if(inherits(out,'error')) {
  stopifnot(role=='payload',nm %in% c('...','..1','..01'))
  native <- tryCatch(if(identical(verb,nest_with_margins)) tidyr::nest(input,.by=g) else dplyr::nest_by(input,g,.keep=keep),error=identity)
  stopifnot(inherits(native,'error'),grepl('Names can',conditionMessage(native),fixed=TRUE),identical(serialize(input,NULL),before))
  refusals <- refusals+1L; attempts <- attempts+1L; next
 }
 stopifnot(identical(names(out),c('g','set',key)),identical(serialize(input,NULL),before))
 if(empty) stopifnot(nrow(out)==0L) else {
  stopifnot(identical(out$set,c(1L,1L,2L)),identical(out$g,c('a','b','Total')))
  stopifnot(identical(lapply(out[[key]],function(x)x$row_id),list(1:2,3L,1:3)))
  for(cell in out[[key]]) {
   cols <- if(keep) names(input) else setdiff(names(input),'g')
   expected <- lapply(as.list(input)[cols],vctrs::vec_slice,i=cell$row_id)
   stopifnot(identical(as.list(cell),expected))
  }
 }
 attempts <- attempts+1L
}
for(size in 0:3) for(verb in list(nest_with_margins,nest_by_with_margins)) for(keep in c(FALSE,TRUE)) {
 input <- data.frame(row.names=seq_len(size))
 out <- verb(input,.keep=keep,.key='...',.id='set')
 expected_outer <- if(size>0L || identical(verb,nest_by_with_margins)) 1L else 0L
 stopifnot(nrow(out)==expected_outer)
 if(expected_outer) stopifnot(nrow(out[['...']][[1L]])==size,ncol(out[['...']][[1L]])==0L)
 attempts <- attempts+1L
}
cat('Name and empty-shape comparisons:',attempts,'with',attempts-refusals,'semantic matches and',refusals,'paired native name refusals.\n')
stopifnot(attempts==272L)
