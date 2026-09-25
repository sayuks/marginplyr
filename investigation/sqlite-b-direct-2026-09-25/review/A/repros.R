args <- commandArgs(TRUE)
pkgload::load_all(args[[1L]], quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
checks <- list()
check <- function(label, fun) {
  cat('\nCASE',label,'\n')
  warnings <- character()
  out <- tryCatch(withCallingHandlers(fun(),warning=function(w){warnings <<- c(warnings,conditionMessage(w)); invokeRestart('muffleWarning')}), error=identity)
  status <- if(inherits(out,'error')) 'FAIL' else 'PASS'
  detail <- if(inherits(out,'error')) conditionMessage(out) else paste(out,collapse='; ')
  cat(status, detail,'\n'); if(length(warnings))cat('WARNINGS:',paste(warnings,collapse=' | '),'\n')
  checks[[length(checks)+1L]] <<- data.frame(case=label,status=status,detail=detail,warnings=paste(warnings,collapse=' | '))
}
eq <- function(a,b) {if(is.data.frame(a) && is.data.frame(b)){a<-as.data.frame(a);b<-as.data.frame(b)};if(!isTRUE(all.equal(a,b,check.attributes=TRUE)))stop(paste(capture.output(all.equal(a,b)),collapse='; '))}
setup <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(),':memory:')
  src <- copy_to(con,tibble(g=c('a','b'),v=c(2,5)),'source',temporary=FALSE)
  list(con=con,src=src)
}
query <- function(src,kind) {
 if(kind=='sorted') summarize_with_margins(src,total=sum(v,na.rm=TRUE),.grouping=rollup(g),.margin_label=NULL,.sort='last',.id='id')
 else summarize_with_margins(src,total=sum(v,na.rm=TRUE),.grouping=rollup(g),.id='id')
}
clean <- function(con) stopifnot(!any(grepl('^marginplyr_order_',DBI::dbListTables(con))))
for(kind in c('sorted','unsorted')) for(form in c('in_schema','Id')) for(analyze in c(FALSE,TRUE)) check(paste('#661',kind,form,analyze), function(){
 z<-setup();con<-z$con;on.exit(DBI::dbDisconnect(con));DBI::dbExecute(con,"ATTACH DATABASE ':memory:' AS other")
 sentinel<-data.frame(g='sentinel',id=99L,total=-1)
 DBI::dbWriteTable(con,'report',sentinel)
 name<-if(form=='Id') DBI::Id(schema='other',table='report') else dbplyr::in_schema('other','report')
 q<-query(z$src,kind);expected<-collect(q)
 m<-compute(q,name=name,temporary=FALSE,analyze=analyze)
 eq(DBI::dbReadTable(con,'report'),sentinel)
 eq(DBI::dbGetQuery(con,'SELECT * FROM other.report'),as.data.frame(expected))
 eq(collect(m),expected);clean(con)
 paste('remote_table=',dbplyr::remote_table(m))
})
for(kind in c('sorted','unsorted')) for(active in c(FALSE,TRUE)) for(in_transaction in c(FALSE,TRUE)) for(fail in c(FALSE,TRUE)) check(paste('#662',kind,'active=',active,'in_transaction=',in_transaction,'failure=',fail), function(){
 z<-setup();con<-z$con;on.exit(DBI::dbDisconnect(con));q<-query(z$src,kind)
 DBI::dbExecute(con,'CREATE TABLE marker (x INT)');DBI::dbExecute(con,'INSERT INTO marker VALUES (1)')
 old<-data.frame(old=21L);DBI::dbWriteTable(con,'target',old)
 if(active){DBI::dbBegin(con);DBI::dbExecute(con,'INSERT INTO marker VALUES (2)')}
 result<-tryCatch(compute(q,name='target',temporary=FALSE,overwrite=TRUE,analyze=FALSE,in_transaction=in_transaction,unique_indexes=if(fail)list('id') else list()),error=identity)
 if(fail){stopifnot(inherits(result,'error'));stopifnot(grepl('UNIQUE constraint failed',conditionMessage(result),fixed=TRUE));eq(DBI::dbReadTable(con,'target'),old)} else {stopifnot(!inherits(result,'error'));eq(collect(result),collect(q))}
 eq(DBI::dbReadTable(con,'source'),data.frame(g=c('a','b'),v=c(2,5)))
 eq(DBI::dbReadTable(con,'marker')$x,if(active)c(1L,2L) else 1L)
 if(active){DBI::dbRollback(con);eq(DBI::dbReadTable(con,'marker')$x,1L);eq(DBI::dbReadTable(con,'target'),old)}
 clean(con);m<-compute(q,name='later',analyze=FALSE);eq(collect(m),collect(q));clean(con)
 'atomic target / marker ownership / subsequent compute / cleanup preserved'
})
for(sort in c('first','last')) check(paste('#663',sort),function(){
 z<-setup();con<-z$con;on.exit(DBI::dbDisconnect(con))
 q<-summarize_with_margins(z$src,total=sum(v),.grouping=rollup(g),.margin_label=NULL,.sort=sort)
 m<-compute(q,name='m');expected<-collect(q);eq(collect(m),expected)
 eq(names(DBI::dbReadTable(con,'m')),c('g','total'))
 eq(collect(select(m,g)),select(expected,g));eq(collect(rename(m,z=total)),rename(expected,z=total))
 eq(collect(filter(m,total>2)),filter(expected,total>2));eq(collect(mutate(m,z=total+1)),mutate(expected,z=total+1));eq(collect(arrange(m,desc(total))),arrange(expected,desc(total)))
 'direct order and select/rename/filter/mutate/arrange pass'
})
for(sort in c('none','first','last')) for(label in list('Total',NULL,NA_character_)) for(explicit in c(FALSE,TRUE)) check(paste('#665',sort,if(is.null(label))'NULL' else label,'explicit=',explicit),function(){
 con<-DBI::dbConnect(RSQLite::SQLite(),':memory:');on.exit(DBI::dbDisconnect(con));src<-copy_to(con,tibble(x=1L),'source');empty<-filter(src,x>1L)
 q<-if(explicit)expand_with_margins(empty,.grouping=rollup(x),.id='set',.sort=sort,.margin_label=label) else expand_with_margins(empty,.id='set',.sort=sort,.margin_label=label)
 for(out in list(collect(q),collect(q,n=0),collect(compute(q)))) {stopifnot(nrow(out)==0L);eq(out$set,integer());eq(names(out),if(explicit)c('x','set') else c('set','x'))}
 clean(con);eq(DBI::dbReadTable(con,'source')$x,1L)
 'empty .id integer in collect/n=0/compute'
})
for(sort in c('first','last')) for(label in list('Total',NULL,NA_character_)) for(kind in c('sum','mean')) for(fixture in c('missing','zero')) check(paste('#666',sort,if(is.null(label))'NULL' else label,kind,fixture),function(){
 con<-DBI::dbConnect(RSQLite::SQLite(),':memory:');on.exit(DBI::dbDisconnect(con))
 data<-if(fixture=='missing')tibble(f=c('A','A','B'),g=c('x','y','z'),v=c(NA_real_,NA_real_,3)) else tibble(f=c('A','A','B'),g=c('x','y','z'),v=c(2,-2,3))
 src<-copy_to(con,data,'source');expr<-if(kind=='sum')rlang::expr(sum(v,na.rm=TRUE)) else rlang::expr(mean(v,na.rm=TRUE))
 q<-summarize_with_margins(src,z=!!expr,p=share_of_parent(z),t=share_of_total(z),across(z,share_of_total,.names='{.col}_across'),.by=f,.grouping=rollup(g),.sort=sort,.margin_label=label,.check_share_source=FALSE)
 expected<-collect(q);m<-compute(q);cols<-c('p','t','z_across')
 for(n in c(0,1,2,3,4,Inf))for(out in list(collect(q,n=n),collect(m,n=n))) {for(col in cols)stopifnot(typeof(out[[col]])=='double');eq(out[cols],head(expected[cols],n));eq(names(out),names(expected));stopifnot(nrow(out)==nrow(head(expected,n)));for(col in setdiff(names(out),cols))eq(as.character(out[[col]]),as.character(head(expected[[col]],n)))}
 eq(DBI::dbReadTable(con,'source'),as.data.frame(data));clean(con)
 'Parent/Total/across declarations preserved over 0/1/2/3/4/Inf'
})
check('source overwrite atomic refusal (unsorted dedicated)',function(){
 z<-setup();con<-z$con;on.exit(DBI::dbDisconnect(con));q<-query(z$src,'unsorted');expected<-collect(q)
 before<-DBI::dbReadTable(con,'source')
 err<-tryCatch(compute(q,name='source',temporary=FALSE,overwrite=TRUE,analyze=FALSE),error=identity)
 stopifnot(inherits(err,'error'));eq(DBI::dbReadTable(con,'source'),before);clean(con)
 'self overwrite refuses atomically; source unchanged'
})
write.csv(do.call(rbind,checks),args[[2L]],row.names=FALSE)
cat('\nSUMMARY\n');print(table(do.call(rbind,checks)$status))
