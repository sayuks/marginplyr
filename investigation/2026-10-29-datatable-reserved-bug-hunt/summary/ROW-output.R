# Evidence only: .ROW is read-safe but cannot be a := output target.
pkgload::load_all(quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
args<-commandArgs(trailingOnly=TRUE)
mode<-intersect(args,c('--expect-bug','--assert-correct'))
args<-setdiff(args,mode)
out<-if(length(args))args[[1L]]else 'investigation/2026-10-29-datatable-reserved-bug-hunt/summary'
attempt<-function(code)tryCatch(list(ok=TRUE,value=force(code)),error=function(e)list(ok=FALSE,message=conditionMessage(e)))
fmt<-function(x)if(x$ok)paste(capture.output(dput(x$value)),collapse=' ')else x$message
input<-data.table::data.table(g=c('a','b'),x=c(1,3))
run<-function(data,name,role,helper,style){
 dots<-rlang::exprs(value=sum(x))
 fraction<-if(role=='share_output')name else 'fraction'
 dots<-c(dots,if(style=='direct')rlang::set_names(list(rlang::call2(helper,quote(value))),fraction)else list(rlang::expr(dplyr::across(value,!!rlang::sym(helper),.names=!!fraction))))
 summarize_with_margins(data,!!!dots,.grouping=rollup(g),.id=if(role=='id')name else 'set')
}
rows<-list();i<-0L
for(role in c('share_output','id'))for(helper in c('share_of_parent','share_of_total'))for(style in c('direct','across')){
 before<-data.table::copy(input)
 local<-attempt(run(tibble::as_tibble(input),'.ROW',role,helper,style))
 safe<-attempt(collect(run(dtplyr::lazy_dt(input),'safe_name',role,helper,style)))
 query<-attempt(run(dtplyr::lazy_dt(input),'.ROW',role,helper,style))
 actual<-if(query$ok)attempt(collect(query$value))else query
 if(query$ok&&helper=='share_of_total'&&style=='direct')writeLines(capture.output(show_query(query$value)),file.path(out,paste0('query-ROW-',role,'.txt')))
 stopifnot(identical(input,before))
 i<-i+1L;rows[[i]]<-data.frame(role,helper,style,local=fmt(local),renamed=fmt(safe),local_ok=local$ok,renamed_ok=safe$ok,actual=fmt(actual),actual_ok=actual$ok,matches_local=actual$ok && local$ok && isTRUE(all.equal(actual$value,local$value)),unchanged=identical(input,before),stringsAsFactors=FALSE)
}
results<-do.call(rbind,rows);write.csv(results,file.path(out,'ROW-output.csv'),row.names=FALSE)
print(results[c('role','helper','style','actual_ok','unchanged')],row.names=FALSE)
# Ordinary summary output creation is supported upstream; subsequent := is not.
cat('\nOrdinary .ROW source output:\n');print(collect(summarise(dtplyr::lazy_dt(input),.ROW=sum(x),.by=g)))
cat('\nNo-share .ROW identifier:\n');print(collect(summarize_with_margins(dtplyr::lazy_dt(input),value=sum(x),.grouping=rollup(g),.id='.ROW')))
cat('\nOrdinary mutation upstream limitation:\n');print(attempt(collect(mutate(dtplyr::lazy_dt(input),.ROW=x)))$message)

stopifnot(all(results$local_ok), all(results$renamed_ok))
if ("--assert-correct" %in% mode) stopifnot(all(results$actual_ok), all(results$matches_local)) else stopifnot(sum(!results$actual_ok) == 8L, all(results$unchanged))
