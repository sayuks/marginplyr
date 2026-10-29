# GForce's built-in aggregate optimization admits input names a wrapper rejects.
pkgload::load_all(quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
args<-commandArgs(trailingOnly=TRUE)
out<-if(length(args))args[[1L]]else 'investigation/2026-10-29-datatable-reserved-bug-hunt/summary'
attempt<-function(code)tryCatch(paste(force(code),collapse=','),error=function(e)conditionMessage(e))
original<-getOption('datatable.optimize')
rows<-list();i<-0L
for(opt in c(0,1,2,Inf))for(name in c('.SD','.GRP','.NGRP','.SDall','.xSD','.iSD','.BY')){
 options(datatable.optimize=opt)
 input<-tibble::tibble(g=c('a','b'),f=c('P','P'),measure=c(1,3));names(input)[3]<-name
 expr<-rlang::call2('sum',rlang::sym(name))
 ordinary<-attempt(collect(summarise(dtplyr::lazy_dt(input),value=!!expr,.by=c(f,g)))$value)
 no_share<-attempt(collect(summarize_with_margins(dtplyr::lazy_dt(input),value=!!expr,.grouping=rollup(g),.by=f))$value)
 share<-attempt(collect(summarize_with_margins(dtplyr::lazy_dt(input),value=!!expr,fraction=share_of_total(value),.grouping=rollup(g),.by=f))$value)
 i<-i+1L;rows[[i]]<-data.frame(opt,name,ordinary,no_share,share,stringsAsFactors=FALSE)
}
options(datatable.optimize=original)
results<-do.call(rbind,rows);write.csv(results,file.path(out,'optimization-controls.csv'),row.names=FALSE)
print(results,row.names=FALSE)

stopifnot(all((results$ordinary == '1,3') == (results$opt >= 2)), all((results$no_share == '1,3,4') == (results$opt >= 2)), all((results$share == '1,3,4') == (results$name == '.BY')))
