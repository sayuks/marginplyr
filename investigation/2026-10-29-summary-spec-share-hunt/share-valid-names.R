pkgload::load_all('.',quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
con1<-DBI::dbConnect(duckdb::duckdb(shared_home=FALSE));con2<-DBI::dbConnect(RSQLite::SQLite(),':memory:')
nms<-c('..0','..00','..01','..-1','.','.data','.env','.x','.N','.I','.SD','.BY','.GRP','_',' a ','a\nb','\u00e9','e\u0301','if','sum','NA','a`b','a"b','a$b','..marginplyr_set_id_1','..marginplyr_share_denominator_1','..marginplyr_total_key_1')
passed<-0;failures<-list()
for(role in c('fixed','source','output')) for(nm in nms) {
 d<-tibble(fixed=c('f','f','q','q'),g=rep(c('a','b'),2),x=c(1,3,2,6))
 sn<-if(role=='source')nm else 'total'; out<-if(role=='output')nm else 'share';key<-if(role=='fixed')nm else 'fixed';names(d)[1]<-key
 dots<-setNames(list(rlang::expr(sum(x)),rlang::call2('share_of_total',rlang::sym(sn))),c(sn,out))
 report<-function(input)summarize_with_margins(input,!!!dots,.by=all_of(key),.grouping=rollup(g),.id='set',.check_share_source=FALSE)
 for(b in c('local','dtplyr','duckdb','sqlite')) {
  input<-switch(b,local=d,dtplyr=dtplyr::lazy_dt(d),duckdb=copy_to(con1,d,'facts',overwrite=TRUE),sqlite=copy_to(con2,d,'facts',overwrite=TRUE))
  ans<-tryCatch(if(b=='dtplyr')as.data.frame(report(input)) else collect(report(input)),error=identity)
  if(inherits(ans,'error')) {failures[[length(failures)+1]]<-list(role=role,nm=nm,backend=b,error=conditionMessage(ans));next}
  ans<-arrange(ans,!!!rlang::syms(c(key,'set','g')))
  if(!identical(names(ans),c(key,'g','set',sn,out))||!isTRUE(all.equal(ans[[out]],rep(c(0.25,0.75,1),2)))) failures[[length(failures)+1]]<-list(role=role,nm=nm,backend=b,result=ans) else passed<-passed+1L
 }
}
DBI::dbDisconnect(con1);DBI::dbDisconnect(con2)
saveRDS(failures,'investigation/2026-10-29-summary-spec-share-hunt/share-name-failures.rds');print(failures);cat('PASS',passed,'FAIL',length(failures),'\n')
