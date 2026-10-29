pkgload::load_all('.', quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
cons <- list(sqlite=DBI::dbConnect(RSQLite::SQLite(),':memory:'),duckdb=DBI::dbConnect(duckdb::duckdb(shared_home=FALSE),dbdir=':memory:'))
keys <- list(integer=c(10L,-1L,NA_integer_),double=c(10.5,-1.5,NA_real_),logical=c(TRUE,FALSE,NA),date=as.Date(c('2026-01-02','2026-01-01',NA)),timestamp=as.POSIXct(c('2026-01-02','2026-01-01',NA),tz='Asia/Tokyo'),factor=factor(c('b','a',NA),levels=c('b','a','unused')),ordered=ordered(c('b','a',NA),levels=c('b','a','unused')),integer64=bit64::as.integer64(c('9007199254740993','9007199254740992',NA)),numeric_edge=c(-Inf,0,Inf),utf8=c('ä','中',NA_character_))
problems <- list();checks<-0L
for(kn in names(keys)) for(empty in c(FALSE,TRUE)) {
 d<-tibble(k=keys[[kn]],v=1:3)
 if(empty)d<-d[0,]
 inputs<-list(dt=dtplyr::lazy_dt(d),arrow=arrow::Table$create(d))
 for(bn in names(cons)) inputs[[bn]]<-tryCatch(copy_to(cons[[bn]],d,paste0(kn,empty)),error=identity)
 for(fixed in c(FALSE,TRUE)) for(so in c('first','last','none')) for(label in list(NULL,'Total')) {
  run<-function(x) summarize_with_margins(x,total=sum(v),bit=if(fixed) 0L else grouping_bit(k),.by=if(fixed) all_of('k') else NULL,.grouping=if(fixed) grouping_set() else rollup(k),.id='id',.sort=so,.margin_label=label) |> collect()
  expected<-run(d)
  for(bn in names(inputs)) {
   if(inherits(inputs[[bn]],'error'))next
   checks<-checks+1L
   got<-tryCatch(run(inputs[[bn]]),error=identity)
   key<-paste(kn,empty,fixed,so,if(is.null(label))'missing' else label,bn,sep='/')
   if(inherits(got,'error')) {problems[[key]]<-conditionMessage(got);cat('ERROR',key,conditionMessage(got),'\n');next}
   if(so=='none') {got<-got[order(got$id),];expected<-expected[order(expected$id),]}
   eq<-all.equal(as.data.frame(got),as.data.frame(expected),check.attributes=FALSE)
   if(!isTRUE(eq)) {problems[[key]]<-list(got=got,expected=expected,diff=eq);cat('DIFF',key,paste(eq,collapse='; '),'\n')}
  }
 }
}
for(con in cons)DBI::dbDisconnect(con)
saveRDS(problems,'investigation/2026-10-29-summary-spec-share-hunt/backend-types-results.rds');cat('CHECKS',checks,'PROBLEMS',length(problems),'\n')
