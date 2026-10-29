# Investigated: 2026-10-29 against ee4321789ca60b6259c53df2852f1574aaede584.
pkgload::load_all('.',quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
arrow_n<-0L
for(shape in c('nested-left-join','nested-right-join','mixed-union','double-union','semi-join','anti-join')){
 x<-tibble(g=c('a','b'),v=1:2);r1<-arrow::RecordBatchReader$create(arrow::RecordBatch$create(x));r2<-arrow::RecordBatchReader$create(arrow::RecordBatch$create(x));t<-arrow::Table$create(x)
 a<-arrange(filter(select(r1,everything()),v>0L),g);b<-head(select(r2,everything()),2L)
 q<-switch(shape,'nested-left-join'=left_join(union_all(t,a),b,by='g'),'nested-right-join'=left_join(t,union_all(a,b),by='g'),'mixed-union'=union_all(select(inner_join(t,b,by='g'),g,v=v.x),a),'double-union'=union_all(union_all(a,t),union_all(t,b)),'semi-join'=semi_join(a,b,by='g'),'anti-join'=anti_join(a,b,by='g'))
 for(i in 1:3){for(typed in c(FALSE,TRUE)){
  p<-if(typed)inspect_grouping(q,.grouping=rollup(where(is.character))) else inspect_grouping(q,.grouping=rollup(g));stopifnot(identical(p$included,c('(g)','()')));arrow_n<-arrow_n+1L
 }}
 for(fun in list(nest_with_margins,nest_by_with_margins)){evaluated<-FALSE;spec<-function(){evaluated<<-TRUE;rollup(g)};e<-tryCatch(fun(q,.grouping=spec()),error=identity);stopifnot(inherits(e,'marginplyr_error'),!evaluated);arrow_n<-arrow_n+1L}
 stopifnot(identical(as_tibble(as.data.frame(r1)),x),identical(as_tibble(as.data.frame(r2)),x))
}
cat('Arrow reader graph calls',arrow_n,'readers preserved',12L,'\n')
for(engine in c('sqlite','duckdb')){
 con<-if(engine=='sqlite')DBI::dbConnect(RSQLite::SQLite(),':memory:')else DBI::dbConnect(duckdb::duckdb(shared_home=FALSE))
 x<-tibble(g=c('b','a'),v=1:2,`.env`=c('north','south'),`TRUE`=c('yes','no'))
 root<-copy_to(con,x,'boundary_data',temporary=TRUE)
 shapes<-list(root=root,head=head(arrange(root,g),1L),filter=filter(root,v>0L),rename=rename(root,amount=v),union=union_all(root,root),distinct=distinct(root),semi=semi_join(root,root,by='g'),joined=left_join(root,select(root,g,w=v),by='g'))
 source_errors <- lapply(shapes,function(q)tryCatch({collect(q);NULL},error=function(e)conditionMessage(e)))
 valid <- vapply(source_errors,is.null,logical(1));cat(engine,'ordinary unsupported source graphs:',paste(names(shapes)[!valid],collapse=','),'\n');if(!all(valid))cat('ordinary source diagnostic:',source_errors[[which(!valid)[[1L]]]],'\n')
 shapes <- shapes[valid]
 statements<-character();trace('dbSendQuery',where=asNamespace('DBI'),print=FALSE,tracer=quote({statements<<-c(statements,as.character(statement))}))
 checks<-0L;refused<-0L
 for(q in shapes){for(fun in list(nest_with_margins,nest_by_with_margins)){n<-length(statements);evaluated<-FALSE;spec<-function(){evaluated<<-TRUE;rollup(g)};e<-tryCatch(fun(q,.grouping=spec()),error=identity);stopifnot(inherits(e,'marginplyr_error'),!evaluated,length(statements)==n);checks<-checks+1L}
  for(typed in c(FALSE,TRUE)){n<-length(statements);p<-tryCatch(if(typed)inspect_grouping(q,.grouping=grouping_set(where(is.character)),.format='list')else inspect_grouping(q,.grouping=rollup(g),.format='list'),error=identity)
   if(engine=='sqlite'&&typed){stopifnot(inherits(p,'marginplyr_error'),length(statements)==n);refused<-refused+1L}else {if(inherits(p,'error')){cat('REMOTE ERROR engine',engine,'typed',typed,'shape',names(shapes)[[match(TRUE,vapply(shapes,identical,logical(1),y=q))]],'\n');cat(conditionMessage(p),'\n')};stopifnot(!inherits(p,'error'),length(statements)==n+as.integer(engine=='duckdb'))};checks<-checks+1L
  }
 }
 untrace('dbSendQuery',where=asNamespace('DBI'));stopifnot(!length(statements)||all(grepl('LIMIT 0',statements,fixed=TRUE)))
 cat(engine,'boundary calls',checks,'typed refusals',refused,'queries',length(statements),'all queries LIMIT 0 when present\n')
 DBI::dbDisconnect(con,shutdown=engine=='duckdb')
}
