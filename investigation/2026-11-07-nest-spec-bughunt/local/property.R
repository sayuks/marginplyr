Sys.setenv(TESTTHAT_PARALLEL = 'false')
args <- commandArgs(trailingOnly = TRUE)
source <- normalizePath(if (length(args)) args[[1L]] else '.', mustWork = TRUE)
output <- if (length(args) > 1L) args[[2L]] else tempdir()
dir.create(output, recursive = TRUE, showWarnings = FALSE)
pkgload::load_all(source, quiet = TRUE)
library(dplyr)
scalar_types <- list(
 char=c('b','a',NA,'a','b'), integer=c(2L,1L,NA,1L,2L), double=c(2,1,NA,NaN,2),
 logical=c(TRUE,FALSE,NA,FALSE,TRUE), complex=c(1+1i,2+1i,NA,2+1i,1+1i),
 factor=factor(c('b','a',NA,'a','b'), levels=c('b','a','c')), ordered=ordered(c('b','a',NA,'a','b'), levels=c('b','a','c')),
 factor_na=factor(c('b','a',NA,'a','b'), levels=c('b','a',NA),exclude=NULL),
 date=as.Date(c('2000-01-02','2000-01-01',NA,'2000-01-01','2000-01-02')),
 time=as.POSIXct(c('2000-01-02','2000-01-01',NA,'2000-01-01','2000-01-02'),tz='Asia/Tokyo'),
 diff=as.difftime(c(1,2,NA,2,1),units='hours'),
 list=list('b','a',NULL,'a','b'), list_na=list(1L,NA_integer_,NULL,NA_integer_,1L),
 matrix=matrix(c(1,2,NA,2,1,3,4,NA,4,3), ncol=2),
 packed=tibble(x=c(1,2,NA,2,1),y=c('a','b',NA,'b','a')),
 raw=as.raw(c(1,2,0,2,1)))
failures <- list(); attempts <- 0L; refusals <- 0L; matched <- 0L
for (type in names(scalar_types)) for (empty in c(FALSE,TRUE)) {
 input <- tibble(f=c('q','q','q','r','r'), g=scalar_types[[type]], h=c('x','y','x','y','x'), rid=seq_len(5L), p=scalar_types[[type]])
 if(empty) input <- input[0,]
 specs <- list(rollup=rollup(g,h), cube=cube(g,h), set=grouping_set(g), union=grouping_sets(grouping_set(g),grouping_set(h),grouping_set()))
 sets <- list(rollup=list(c('g','h'),'g',character()), cube=list(c('g','h'),'g','h',character()),set=list('g'),union=list('g','h',character()))
 for(s in names(specs)) for(verbname in c('nest_with_margins','nest_by_with_margins')) for(keep in c(FALSE,TRUE)) for(sort in c('none','first','last')) {
  attempts <- attempts + 1L
  spec <- specs[[s]]; verb <- get(verbname)
  out <- tryCatch(verb(input,.by=f,.grouping=spec,.margin_label=NULL,.id='sid',.keep=keep,.sort=sort),error=identity)
  if(inherits(out,'error')) {
   if(type == 'raw' && inherits(out,'marginplyr_error') && grepl('typed-missing',conditionMessage(out),fixed=TRUE)) {refusals <- refusals+1L; next}
   failures[[length(failures)+1]] <- list(type=type,empty=empty,spec=s,verb=verbname,keep=keep,sort=sort,error=conditionMessage(out)); next
  }
  matched <- matched+1L
  out <- ungroup(out)
  for(si in seq_along(sets[[s]])) {
   cells <- out$data[out$sid==si]
   observed <- sort(as.integer(unlist(lapply(cells, function(x) x$rid))))
   if(!identical(observed,input$rid)) failures[[length(failures)+1]] <- list(type=type,empty=empty,spec=s,verb=verbname,keep=keep,sort=sort,error='source rid partition mismatch')
   groupcols <- c('f',sets[[s]][[si]])
   expected_groups <- vctrs::vec_group_id(input[groupcols])
   for(cell in cells) {
    if(length(unique(expected_groups[cell$rid]))>1L) failures[[length(failures)+1]] <- list(type=type,spec=s,error='cell combined distinct source groups')
    expected <- input[cell$rid, if(keep) names(input) else setdiff(names(input),c('f',unique(unlist(sets[[s]]))))]
    if(!identical(names(cell),names(expected)) || !identical(lapply(cell, unname),lapply(expected,unname))) failures[[length(failures)+1]] <- list(type=type,empty=empty,spec=s,verb=verbname,keep=keep,sort=sort,error='payload mismatch',cell=cell,expected=expected)
   }
   if(length(cells)!=length(unique(expected_groups))) failures[[length(failures)+1]] <- list(type=type,empty=empty,spec=s,verb=verbname,keep=keep,sort=sort,error='source group count mismatch')
  }
 }
}
cat('attempts=',attempts,' semantic_matches=',matched,' documented_raw_refusals=',refusals,' unexpected_mismatches=',length(failures),'\n',sep='')
dput(failures,file=file.path(output,'property-failures.txt'))
write.csv(data.frame(attempts=attempts,semantic_matches=matched,documented_raw_refusals=refusals,unexpected_mismatches=length(failures)),file.path(output,'property-summary.csv'),row.names=FALSE)
check_partition <- function(cells) identical(sort(as.integer(unlist(cells))),1:2)
stopifnot(check_partition(list(1L,2L)),!check_partition(list(1L,1L)),!check_partition(list(1L)),!check_partition(list(1L,2L,2L)))
cat('Calibration rejected duplicate, omitted and extra source membership.\n')
stopifnot(attempts==1536L,matched==1464L,refusals==72L,length(failures)==0L)
print(sessionInfo())
