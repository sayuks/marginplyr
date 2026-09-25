args <- commandArgs(TRUE)
pkgload::load_all(args[[1L]], quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
rows <- list()
record <- function(name, pass, detail='') {
 rows[[length(rows)+1L]] <<- data.frame(case=name,pass=isTRUE(pass),detail=detail)
 cat(if(isTRUE(pass)) 'PASS' else 'FAIL',name,detail,'\n')
}
for(sort in c('first','last','none')) {
 src <- data.table::data.table(g='a')
 before <- serialize(src,NULL)
 got <- tryCatch(expand_with_margins(src,.grouping=rollup(g),.sort=sort),error=identity)
 if(sort=='none') record(paste('data.table',sort),!inherits(got,'error') && identical(got$g,c('a','Total'))) else record(paste('data.table',sort,'known-rejection'),inherits(got,'error') && grepl('columns to join by must be specified',conditionMessage(got),fixed=TRUE),if(inherits(got,'error')) conditionMessage(got) else '')
 record(paste('data.table',sort,'source-preserved'),identical(before,serialize(src,NULL)))
 expected <- if(sort=='first')c('Total','a') else c('a','Total')
 for(kind in c('data.frame','dtplyr')) {
  input <- if(kind=='data.frame')as.data.frame(src) else dtplyr::lazy_dt(src,immutable=TRUE)
  got <- expand_with_margins(input,.grouping=rollup(g),.sort=sort)
  if(kind=='dtplyr')got<-collect(got)
  record(paste(kind,sort,'control'),identical(got$g,expected))
  record(paste(kind,sort,'source-preserved'),identical(before,serialize(src,NULL)))
 }
}
for(sort in c('first','last')) {
 src<-data.table::data.table(year=c(2025L,2025L,2026L),g=c('a',NA_character_,'b'),v=1:3)
 before<-serialize(src,NULL)
 results<-lapply(c('data.frame','dtplyr'),function(kind) {
  x<-if(kind=='data.frame')as.data.frame(src) else dtplyr::lazy_dt(src,immutable=TRUE)
  z<-expand_with_margins(x,.by=year,.grouping=rollup(g),.id='set',.sort=sort)
  if(kind=='dtplyr')z<-collect(z)
  as.data.frame(z)
 })
 for(i in seq_along(results)) {
  z<-results[[i]];label<-c('data.frame','dtplyr')[[i]]
  # Equal grouping keys may tie; no order of payload v is promised within ties.
  expected<-data.frame(year=c(2025L,2025L,2026L,2025L,2025L,2026L),g=c('a',NA,'b','Total','Total','Total'),set=c(1L,1L,1L,2L,2L,2L),v=c(1:3,1:3))
  keyed<-function(d)d[order(d$year,d$set,d$v),,drop=FALSE]
  a<-keyed(z);b<-keyed(expected);rownames(a)<-NULL;rownames(b)<-NULL
  record(paste(label,sort,'fixed-id-missing-values'),identical(a,b))
  expected_id<-if(sort=='last')c(1L,1L,2L,2L,1L,2L) else c(2L,2L,1L,1L,2L,1L)
  record(paste(label,sort,'fixed-id-margin-order'),identical(z$set,expected_id)&&identical(z$year,c(rep(2025L,4),rep(2026L,2))))
 }
 record(paste('rich',sort,'source-preserved'),identical(before,serialize(src,NULL)))
}
write.csv(do.call(rbind,rows),args[[2L]],row.names=FALSE)
stopifnot(all(vapply(rows,function(x)x$pass,logical(1))))
