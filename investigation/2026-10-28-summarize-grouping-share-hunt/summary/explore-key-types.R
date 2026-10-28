args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args)) normalizePath(args[[1L]]) else getwd()
suppressPackageStartupMessages(library(dplyr))
devtools::load_all(repo, quiet = TRUE)
keys <- list(character=c('a',NA,'b'),numeric=c(1,NA,2),inf=c(-Inf,NaN,Inf),complex=c(1+1i,NA_complex_,2i),list=list(1,NA,2),list_frame=list(tibble(a=1),tibble(a=NA_real_),tibble(a=2)),factor=factor(c('a',NA,'b')),factor_NA=factor(c('a',NA,'b'),exclude=NULL),ordered=ordered(c('a',NA,'b')),date=as.Date(c('2020-01-01',NA,'2020-01-02')),posix=as.POSIXct(c('2020-01-01',NA,'2020-01-02'),tz='Asia/Tokyo'),diff=as.difftime(c(1,NA,2),units='hours'),matrix=matrix(c(1,NA,2,2,NA,1),ncol=2),packed=tibble(a=c(1,NA,2),b=c(2,NA,1)),array=array(1:12,c(3,2,2)))
cap <- function(e) tryCatch(e,error=identity)
for(nm in names(keys)) {
 g <- keys[[nm]];d <- tibble(g=g,x=1:3)
 expected <- cap({detail<-summarise(d,s=sum(x),.by=g);total<-tibble(g=vctrs::vec_init(g,1),s=sum(d$x));bind_rows(detail,total)})
 actual <- cap(summarize_with_margins(d,s=sum(x),.grouping=rollup(g),.margin_label=NULL))
 if(inherits(expected,'error')&&inherits(actual,'error')) next
 if(!isTRUE(all.equal(actual,expected))) {cat('\nTYPE',nm,'\n');cat('EXPECTED\n');print(expected);cat('ACTUAL\n');print(actual)}
}
cat('KEY CASES',length(keys),'\n')
