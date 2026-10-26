# Dated investigation evidence, investigated 2026-10-26 at 6b91254016dbcce60cc2cc69071f01f69eb8084e.
# Run from the repository root, or pass its path as the first argument.
args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args)) args[[1L]] else "."
pkgload::load_all(repo, quiet = TRUE)
library(dplyr)
set.seed(6351)
construct <- list(
 chr=function(n)sample(c('x','y',NA),n,TRUE),
 int=function(n)sample(c(1L,2L,NA_integer_),n,TRUE),
 dbl=function(n)sample(c(1,2,NA_real_,NaN),n,TRUE),
 logical=function(n)sample(c(TRUE,FALSE,NA),n,TRUE),
 fct=function(n)factor(sample(c('x','y',NA),n,TRUE),levels=c('y','z','x')),
 ordered=function(n)ordered(sample(c('x','y',NA),n,TRUE),levels=c('y','z','x')),
 nafactor=function(n)addNA(factor(sample(c('x','y',NA),n,TRUE),levels=c('y','z','x'))),
 Date=function(n)as.Date(sample(c('2020-01-01','2020-01-02',NA),n,TRUE)),
 POSIXct=function(n)as.POSIXct(as.Date(sample(c('2020-01-01','2020-01-02',NA),n,TRUE)),tz='Japan'))
normalize <- function(r) {
 r<-ungroup(collect(r)); outer<-r[setdiff(names(r),'data')]
 # Missing values share the Margin-order slot; NaN and NA need explicit tie keys for backend-neutral matching.
 for(nm in names(outer)) {
   if(is.double(outer[[nm]])&&!is.object(outer[[nm]])) {
     outer[[paste0(nm,'_nan')]]<-is.nan(outer[[nm]])
   }
 }
 ord<-vctrs::vec_order(outer); r<-r[ord,,drop=FALSE]
 r$data<-lapply(r$data,function(cell){cell<-as_tibble(cell); cell[order(cell$row_id),,drop=FALSE]})
 as_tibble(r)
}
fail<-0L;cases<-0L
for(a in names(construct))for(b in names(construct))for(keep in c(TRUE,FALSE))for(n in c(0L,5L)) {
 cases<-cases+1L
 x<-tibble(fixed=sample(c('A','B',NA),n,TRUE),a=construct[[a]](n),b=construct[[b]](n),row_id=seq_len(n),p=construct$nafactor(n))
 run<-function(x)nest_with_margins(x,.by=fixed,.grouping=cube(a,b),.margin_label=NULL,.keep=keep,.id='set',.sort='last')
 local<-tryCatch(normalize(run(x)),error=identity); lazy<-tryCatch(normalize(run(dtplyr::lazy_dt(x))),error=identity)
 equal<-if(inherits(local,'error')||inherits(lazy,'error')) FALSE else identical(local,lazy)
 if(!equal) { fail<-fail+1L; cat('FAIL',a,b,keep,n,'\n');if(inherits(local,'error'))cat('local',conditionMessage(local),'\n');if(inherits(lazy,'error'))cat('lazy',conditionMessage(lazy),'\n'); if(!inherits(local,'error')&&!inherits(lazy,'error'))print(all.equal(local,lazy));saveRDS(list(x=x,local=local,lazy=lazy),file.path(tempdir(),paste0('backend-fuzz-',fail,'.rds'))) }
}
cat('cases',cases,'failures',fail,'\n')
stopifnot(cases == 324L, fail == 0L)
