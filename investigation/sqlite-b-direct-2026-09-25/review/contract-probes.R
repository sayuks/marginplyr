pkgload::load_all('.', quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
con <- DBI::dbConnect(RSQLite::SQLite(), ':memory:')
on.exit(DBI::dbDisconnect(con))
src <- copy_to(con, data.frame(g = c('a','b'), v = c(2,5)), 'contract_source')
cat(R.version.string, '\n')
for(p in c('dplyr','dbplyr','DBI','RSQLite')) cat(p, as.character(packageVersion(p)), '\n')
for(verb in c('summary','expand')) for(label in c('text','typed')) for(sort in c('none','last')) for(id in c(FALSE,TRUE)) {
 args <- list(.data=src, .grouping=rollup(g), .margin_label=if(label=='typed')NULL else 'Total', .sort=sort, .id=if(id)'set' else NULL)
 q <- if(verb=='summary')do.call(summarize_with_margins,c(args,list(total=rlang::expr(sum(v,na.rm=TRUE))))) else do.call(expand_with_margins,args)
 cat(verb,label,sort,id,':',inherits(q,'marginplyr_sqlite_typed_result'),'\n')
}
old <- options(marginplyr.audit_sql=TRUE)
q <- summarize_with_margins(src, total=sum(v,na.rm=TRUE), .grouping=rollup(g), .margin_label=NULL, .sort='last')
sent_before <- last_sent_queries()
invisible(collect(q,n=1))
cat('audit_unchanged_after_finite_collect',identical(sent_before,last_sent_queries()),'\n')
invisible(compute(q,name='contract_result',analyze=TRUE))
cat('audit_unchanged_after_compute',identical(sent_before,last_sent_queries()),'\n')
cat('audit_result_matches_full_render',identical(sent_before$sql[sent_before$purpose=='result'],as.character(dbplyr::sql_render(q))),'\n')
options(old)
for(n in c('compute.tbl_sql','db_compute.DBIConnection','remote_name','remote_table','rename_order','collect.tbl_sql','with_transaction')) {
 cat('\n### dbplyr ',n,'\n',sep=''); print(get0(n,envir=asNamespace('dbplyr')))
}
