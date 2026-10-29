pkgload::load_all('.',quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
cons<-list(sqlite=DBI::dbConnect(RSQLite::SQLite(),':memory:'),duckdb=DBI::dbConnect(duckdb::duckdb(shared_home=FALSE)))
d<-tibble(g=c('a','b','a','b','c'),h=c('u','v','u',NA,'u'),v=1:5)
inputs<-list(local=d,dt=dtplyr::lazy_dt(d),arrow=arrow::Table$create(d));for(bn in names(cons))inputs[[bn]]<-copy_to(cons[[bn]],d,'facts')
trans<-list(head=function(x)head(x,3),filter=function(x)filter(x,v>2),empty=function(x)filter(x,v>99),mutate=function(x)mutate(x,v=v*2),select=function(x)select(x,v,h,g),rename=function(x)rename(x,h0=h)|>rename(h=h0),distinct=function(x)distinct(x,g,h,.keep_all=TRUE),union=function(x)union_all(x,filter(x,v<3)),join=function(x)left_join(x,select(x,g,z=v),by='g',relationship='many-to-many'),aggregate=function(x)summarise(x,v=sum(v),.by=c(g,h)))
errors<-list();checks<-0L
for(tn in names(trans)) {
 ys<-lapply(inputs,function(x)tryCatch(trans[[tn]](x),error=identity))
 for(so in c('none','last')) for(du in c('drop','keep')) {
  run<-function(x)summarize_with_margins(x,total=coalesce(sum(v,na.rm=TRUE),0L),n=n(),gb=grouping_id(g,h),.grouping=rollup(g,h),.id='sid',.sort=so,.duplicates=du,.margin_label=NULL)|>collect()
  expected<-run(ys$local)
  for(bn in names(ys)[-1]) {
   checks<-checks+1L;key<-paste(tn,so,du,bn,sep='/')
   got<-tryCatch(run(ys[[bn]]),error=identity)
   eq<-if(inherits(got,'error'))conditionMessage(got) else {
    if(so=='none'){got<-got[vctrs::vec_order(got),];expected<-expected[vctrs::vec_order(expected),]}
    if(nrow(expected)==0 &&nrow(got)==0)TRUE else all.equal(as.data.frame(got),as.data.frame(expected),check.attributes=FALSE)
   }
   if(!isTRUE(eq)){errors[[key]]<-list(got=got,expected=expected,diff=eq);cat('FAIL',key,paste(eq,collapse='; '),'\n')}
  }
 }
}
saveRDS(errors,'investigation/2026-10-29-summary-spec-share-hunt/backend-derived-results.rds');for(co in cons)DBI::dbDisconnect(co);cat('CHECKS',checks,'PROBLEMS',length(errors),'\n')
