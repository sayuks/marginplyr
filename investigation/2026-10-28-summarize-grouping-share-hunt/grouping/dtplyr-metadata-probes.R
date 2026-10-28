args <- commandArgs(trailingOnly = TRUE)
if (length(args)) setwd(args[[1L]])
pkgload::load_all(quiet=TRUE)
library(dplyr,warn.conflicts=FALSE)
d<-data.table::data.table(a=c('x','y'),b=1:2,f=factor(c('v','w')),m=as.Date('2026-01-01')+0:1)
l<-dtplyr::lazy_dt(d)
cases<-list(copy=mutate(l,b=a),copyfactor=mutate(l,b=f),copydate=mutate(l,b=m),copynum=mutate(l,a=b),arith=mutate(l,b=b/2),newvec=mutate(l,z=c(1L,2L)))
for(n in names(cases)) {
 q<-cases[[n]]
 actual<-collect(q)
 proxy<-tryCatch(getFromNamespace('dtplyr_selection_proxy','marginplyr')(q),error=function(e)e)
 cat(n,'actual:',paste(vapply(actual,typeof,''),collapse=','),'proxy:')
 if(inherits(proxy,'error'))cat(conditionMessage(proxy),'\n')else cat(paste(vapply(proxy,typeof,''),collapse=','),'levels:',paste(lapply(proxy,levels),collapse=','),'\n')
}
