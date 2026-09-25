# THROWAWAY executable evidence, synthetic in-memory databases only.
args <- commandArgs(TRUE)
pkg <- normalizePath(args[[1]])
outdir <- normalizePath(args[[2]])
pkgload::load_all(pkg, quiet = TRUE)
library(dplyr)
results <- list()
check <- function(name, code) {
  warnings <- character()
  value <- tryCatch(withCallingHandlers(force(code), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w)); invokeRestart('muffleWarning')
  }), error = function(e) e)
  ok <- !inherits(value, 'error') && isTRUE(value)
  detail <- if (inherits(value, 'error')) conditionMessage(value) else paste(capture.output(str(value)),collapse=' ')
  results[[length(results)+1L]] <<- data.frame(case=name, pass=ok, detail=detail, warnings=paste(warnings,collapse=' | '))
  cat(if (ok) 'PASS' else 'FAIL', name, detail, '\n')
  invisible(value)
}
with_source <- function(data, body) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ':memory:')
  on.exit(DBI::dbDisconnect(con))
  src <- copy_to(con, data, 'source', temporary=FALSE)
  before <- DBI::dbReadTable(con, 'source')
  res <- body(con, src)
  DBI::dbExecute(con, 'PRAGMA reverse_unordered_selects=OFF')
  stopifnot(identical(before, DBI::dbReadTable(con, 'source')))
  res
}
fixture <- tibble(g=c('a','b'), v=c(2,5))
for (sort in c('none','first','last')) for (kind in c('in_schema','DBI_Id')) {
  check(paste('661_destination',sort,kind), with_source(fixture,function(con,src) {
    DBI::dbExecute(con, "ATTACH ':memory:' AS other")
    sentinel <- data.frame(g='keep',id=99L,total=-1)
    DBI::dbWriteTable(con, DBI::Id(schema='main',table='report'),sentinel)
    q <- summarize_with_margins(src,total=sum(v,na.rm=TRUE),.grouping=rollup(g),.id='id',.sort=sort,.margin_label=NULL)
    destination <- if(kind=='in_schema') dbplyr::in_schema('other','report') else DBI::Id(schema='other',table='report')
    m <- compute(q,name=destination,temporary=FALSE,analyze=TRUE)
    got <- collect(m)
    stopifnot(nrow(got)==3L, identical(DBI::dbReadTable(con,DBI::Id(schema='main',table='report')),sentinel))
    stopifnot(setequal(got$total,c(2,5,7)), identical(names(got),c('g','id','total')))
    TRUE
  }))
}
for (sort in c('none','last')) for (outer in c(FALSE,TRUE)) for (it in c(FALSE,TRUE)) {
  check(paste('662_transaction',sort,outer,it), with_source(fixture,function(con,src) {
    q <- summarize_with_margins(src,total=sum(v,na.rm=TRUE),.grouping=rollup(g),.id='id',.sort=sort,.margin_label=NULL)
    DBI::dbExecute(con,'CREATE TABLE marker (v INTEGER)')
    if(outer) DBI::dbBegin(con)
    DBI::dbExecute(con,'INSERT INTO marker VALUES (42)')
    m <- compute(q,name='report',analyze=FALSE,in_transaction=it)
    stopifnot(nrow(collect(m))==3L,DBI::dbGetQuery(con,'SELECT * FROM marker')$v==42L)
    if(outer) { DBI::dbRollback(con); stopifnot(nrow(DBI::dbReadTable(con,'marker'))==0, !DBI::dbExistsTable(con,'report')) }
    TRUE
  }))
}
for (sort in c('first','last')) for (verb in c('select','rename','filter','mutate','arrange')) {
  check(paste('663_downstream',sort,verb),with_source(fixture,function(con,src){
    q<-summarize_with_margins(src,total=sum(v,na.rm=TRUE),.grouping=rollup(g),.margin_label=NULL,.sort=sort)
    m<-compute(q)
    z<-switch(verb,select=select(m,g),rename=rename(m,renamed=total),filter=filter(m,total>=2),mutate=mutate(m,twice=2*total),arrange=arrange(m,total))
    r<-collect(z);stopifnot(nrow(r)==3L)
    if(verb=='select')stopifnot(identical(names(r),'g'),setequal(r$g,c('a','b',NA_character_)))
    if(verb=='rename')stopifnot(identical(names(r),c('g','renamed')),setequal(r$renamed,c(2,5,7)))
    TRUE
  }))
}
for (sort in c('none','first','last')) for (lab in c('text','typed')) for (route in c('direct','compute')) {
  check(paste('665_empty_expansion',sort,lab,route),with_source(tibble(x=1L),function(con,src){
    q<-expand_with_margins(filter(src,x>1L),.id='set',.grouping=rollup(x),.sort=sort,.margin_label=if(lab=='text')'Total' else NULL)
    z<-if(route=='direct') q else compute(q)
    r<-collect(z);stopifnot(nrow(r)==0,identical(r$set,integer()),identical(names(r),c('x','set')));TRUE
  }))
}
for(share in c('total','parent')) for(agg in c('sum','mean')) for(sort in c('first','last')) for(n in c(0,1,2,Inf)) {
  check(paste('666_share',share,agg,sort,n),with_source(tibble(fixed=c('A','A','B'),g=c('x','y','z'),v=c(2,-2,3)),function(con,src){
    value<-if(agg=='sum')rlang::expr(sum(v,na.rm=TRUE)) else rlang::expr(mean(v,na.rm=TRUE))
    sh<-if(share=='total')rlang::expr(share_of_total(z)) else rlang::expr(share_of_parent(z))
    q<-summarize_with_margins(src,z=!!value,s=!!sh,.by=fixed,.grouping=rollup(g),.sort=sort,.check_share_source=FALSE)
    all<-collect(q);r<-collect(q,n=n); m<-collect(compute(q),n=n)
    stopifnot(typeof(r$s)=='double',typeof(m$s)=='double',nrow(r)==min(n,nrow(all)),(n == 0 || isTRUE(all.equal(as.data.frame(r),as.data.frame(head(all,n))))))
    TRUE
  }))
}
for(type in c('character','integer','double')) for(sort in c('first','last')) for(route in c('direct','finite','compute')) {
  check(paste('normal_all_null_source',type,sort,route),with_source(tibble(g=switch(type,character=NA_character_,integer=NA_integer_,double=NA_real_),v=1),function(con,src){
    q<-summarize_with_margins(src,z=sum(v,na.rm=TRUE),.grouping=rollup(g),.margin_label=NULL,.sort=sort,.id='set')
    r<-switch(route,direct=collect(q),finite=collect(q,n=1),compute=collect(compute(q)))
    stopifnot(typeof(r$g)==type,nrow(r)==if(route=='finite')1L else 2L,all(is.na(r$g)))
    ids<-if(sort=='first')c(2L,1L)else c(1L,2L)
    stopifnot(identical(r$set,head(ids,nrow(r))));TRUE
  }))
}
for(sort in c('first','last')) {
  check(paste('materialized_explicit_order',sort),with_source(fixture,function(con,src){
    q<-summarize_with_margins(src,total=sum(v,na.rm=TRUE),.grouping=rollup(g),.id='id',.sort=sort,.margin_label=NULL)
    expected<-collect(q);m<-compute(q)
    DBI::dbExecute(con,'PRAGMA reverse_unordered_selects=ON')
    r<-collect(m);stopifnot(isTRUE(all.equal(as.data.frame(r),as.data.frame(expected))));TRUE
  }))
}
for(outer in c(FALSE,TRUE)) {
 check(paste('failure_atomic_overwrite_unique',outer),with_source(tibble(g=c('a','a'),v=c(1,2)),function(con,src){
   q<-expand_with_margins(src,.grouping=rollup(g),.sort='last',.margin_label=NULL,.id='id')
   old<-data.frame(old=77L);DBI::dbWriteTable(con,'report',old)
   DBI::dbExecute(con,'CREATE TABLE marker (v INTEGER)')
   if(outer)DBI::dbBegin(con)
   DBI::dbExecute(con,'INSERT INTO marker VALUES (42)')
   err<-tryCatch(compute(q,name='report',temporary=FALSE,overwrite=TRUE,unique_indexes=list('g')),error=identity)
   stopifnot(inherits(err,'error'),identical(DBI::dbReadTable(con,'report'),old),DBI::dbReadTable(con,'marker')$v==42L,!any(grepl('^marginplyr_order_',DBI::dbListTables(con))))
   if(outer){DBI::dbRollback(con);stopifnot(nrow(DBI::dbReadTable(con,'marker'))==0)}
   stopifnot(nrow(collect(compute(q,name='retry')))==4L);TRUE
 }))
}
check('audit_result_sql_and_no_execution_record',with_source(fixture,function(con,src){
 options(marginplyr.audit_sql=TRUE);on.exit(options(marginplyr.audit_sql=NULL))
 q<-summarize_with_margins(src,z=sum(v,na.rm=TRUE),.grouping=rollup(g),.margin_label=NULL,.sort='last')
 log<-last_sent_queries();stopifnot(identical(log$sql,as.character(dbplyr::sql_render(q))))
 invisible(collect(q,n=1));invisible(compute(q));stopifnot(identical(log,last_sent_queries()));TRUE
}))
for(n in list(-1L,'1',c(1L,2L),1.9,0L,Inf)) {
 check(paste('finite_validation',paste(n,collapse=',')),with_source(fixture,function(con,src){
 q<-summarize_with_margins(src,z=sum(v,na.rm=TRUE),.grouping=rollup(g),.margin_label=NULL,.sort='last')
 a<-tryCatch(collect(src,n=n),error=identity);b<-tryCatch(collect(q,n=n),error=identity)
 if(!identical(n,1.9))stopifnot(identical(inherits(a,'error'),inherits(b,'error')))
 if(identical(n,1.9))stopifnot(!inherits(b,'error'))
 if(!inherits(b,'error'))stopifnot(nrow(b)==min(trunc(n),3L))
 TRUE
 }))
}
check('ordinary_dbplyr_control',with_source(fixture,function(con,src){
 DBI::dbExecute(con,"ATTACH ':memory:' AS other")
 DBI::dbBegin(con)
 q<-src|>group_by(g)|>summarize(total=sum(v,na.rm=TRUE))
 m<-compute(q,name=dbplyr::in_schema('other','control'),temporary=FALSE,analyze=FALSE)
 stopifnot(nrow(collect(select(m,g)))==2L)
 DBI::dbRollback(con);TRUE
}))
res<-do.call(rbind,results)
write.csv(res,file.path(outdir,'cross-check.csv'),row.names=FALSE)
writeLines(capture.output(sessionInfo()),file.path(outdir,'session-info.txt'))
cat('TOTAL',nrow(res),'PASS',sum(res$pass),'FAIL',sum(!res$pass),'\n')
