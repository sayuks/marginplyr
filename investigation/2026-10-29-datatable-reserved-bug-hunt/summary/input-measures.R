# Evidence only: compare the same aggregate before and after a contextual share.
pkgload::load_all(quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
args<-commandArgs(trailingOnly=TRUE)
mode<-intersect(args,c('--expect-bug','--assert-correct'))
args<-setdiff(args,mode)
out<-if(length(args))args[[1L]]else 'investigation/2026-10-29-datatable-reserved-bug-hunt/summary'
attempt<-function(code)tryCatch(list(ok=TRUE,value=force(code)),error=function(e)list(ok=FALSE,message=conditionMessage(e)))
fmt<-function(x)if(x$ok)paste(x$value,collapse=',')else x$message
run<-function(data,measure,helper,style,share=TRUE){
 dots<-if(style=='direct')rlang::set_names(list(rlang::call2('sum',rlang::sym(measure))),'value')else list(rlang::expr(dplyr::across(dplyr::all_of(!!measure),sum,.names='value')))
 if(share)dots<-c(dots,rlang::exprs(fraction=!!rlang::call2(helper,quote(value))))
 summarize_with_margins(data,!!!dots,.grouping=rollup(g),.by=f)
}
rows<-list();i<-0L
for(name in c('.SD','.GRP','.NGRP','.SDall','.xSD','.iSD','.BY'))for(helper in c('share_of_parent','share_of_total'))for(style in c('direct','across')){
 input<-data.table::data.table(g=c('a','b'),f=c('P','P'),measure=c(1,3));data.table::setnames(input,'measure',name)
 before<-data.table::copy(input);expr<-rlang::call2('sum',rlang::sym(name))
 ordinary<-attempt(collect(summarise(dtplyr::lazy_dt(input),value=!!expr,.by=c(f,g)))$value)
 no_share<-attempt(collect(run(dtplyr::lazy_dt(input),name,helper,style,FALSE))$value)
 local<-attempt(run(tibble::as_tibble(input),name,helper,style)$fraction)
 safe<-data.table::copy(input);data.table::setnames(safe,name,'measure')
 renamed<-attempt(collect(run(dtplyr::lazy_dt(safe),'measure',helper,style))$fraction)
 query<-attempt(run(dtplyr::lazy_dt(input),name,helper,style))
 actual<-if(query$ok)attempt(collect(query$value)$fraction)else query
 if(query$ok&&name=='.SD'&&helper=='share_of_total'&&style=='direct')writeLines(capture.output(show_query(query$value)),file.path(out,'query-input-SD.txt'))
 stopifnot(identical(input,before))
 i<-i+1L;rows[[i]]<-data.frame(name,helper,style,ordinary=fmt(ordinary),no_share=fmt(no_share),local=fmt(local),renamed=fmt(renamed),actual=fmt(actual),actual_ok=actual$ok,matches_local=actual$ok && local$ok && isTRUE(all.equal(actual$value,local$value)),unchanged=identical(input,before),stringsAsFactors=FALSE)
}
results<-do.call(rbind,rows);write.csv(results,file.path(out,'input-measures.csv'),row.names=FALSE)
print(results[c('name','helper','style','actual_ok','unchanged')],row.names=FALSE)
cat('\nFailing candidates:',sum(!results$actual_ok),'of',nrow(results),'cases; .BY control passed.\n')

stopifnot(all(results$ordinary == '1,3'), all(results$no_share == '1,3,4'), all(results$local == '0.25,0.75,1'), all(results$renamed == '0.25,0.75,1'))
if ("--assert-correct" %in% mode) stopifnot(all(results$actual_ok), all(results$matches_local)) else stopifnot(sum(!results$actual_ok) == 24L, all(results$unchanged))
