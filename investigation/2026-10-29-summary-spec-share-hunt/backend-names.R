pkgload::load_all('.', quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
cons<-list(sqlite=DBI::dbConnect(RSQLite::SQLite(),':memory:'),duckdb=DBI::dbConnect(duckdb::duckdb(shared_home=FALSE),dbdir=':memory:'))
ns<-c('.env','.data','..0','..00','...1','TRUE','FALSE','NA','NaN','Inf','.N','.SD','.I','.BY','.GRP','key','grouping','x y','a"b','a`b','a\\b','a\nb','a\rb','a$b','a:b','a::b','a[1]','é','中','a\tb')
errors<-list();checks<-0L;case<-0L
for(name in ns) for(role in c('fixed','dimension','summary','id')) {
 case<-case+1L
 nms<-c(fixed='f',dimension='g',summary='total',id='sid');nms[[role]]<-name
 d<-tibble(f=c('p','p','q'),g=c('b','a','a'),v=c(2L,3L,5L));names(d)[1:2]<-nms[1:2]
 spec<-rollup(all_of(nms[['dimension']]))
 dots<-setNames(rlang::quos(sum(v,na.rm=TRUE)),nms[['summary']])
 run<-function(x,so,dup) summarize_with_margins(x,!!!dots,.by=all_of(nms[['fixed']]),.grouping=spec,.id=nms[['id']],.sort=so,.duplicates=dup) |> collect()
 inputs<-list(dt=dtplyr::lazy_dt(d),arrow=arrow::Table$create(d))
 for(bn in names(cons))inputs[[bn]]<-tryCatch(copy_to(cons[[bn]],d,paste0('case',case)),error=identity)
 for(so in c('first','none')) for(dup in c('drop','keep')) {
  expected<-tryCatch(run(d,so,dup),error=identity)
  if(inherits(expected,'error'))next
  for(bn in names(inputs)) {
   if(inherits(inputs[[bn]],'error'))next
   checks<-checks+1L;key<-paste(role,encodeString(name),so,dup,bn,sep='/')
   got<-tryCatch(run(inputs[[bn]],so,dup),error=identity)
   eq<-if(inherits(got,'error')) conditionMessage(got) else {
    if(so=='none') {got<-got[vctrs::vec_order(got),];expected<-expected[vctrs::vec_order(expected),]}
    all.equal(as.data.frame(got),as.data.frame(expected),check.attributes=FALSE)
   }
   if(!isTRUE(eq)){errors[[key]]<-list(data=d,got=got,expected=expected,diff=eq);cat('FAIL',key,paste(eq,collapse='; '),'\n')}
  }
 }
 cat('CASE',case,checks,'FAIL',length(errors),'\n')
}
for(co in cons)DBI::dbDisconnect(co)
saveRDS(errors,'investigation/2026-10-29-summary-spec-share-hunt/backend-names-results.rds');cat('TOTAL',checks,'ERRORS',length(errors),'\n')
