args<-commandArgs(TRUE); pkgload::load_all(if(length(args)) args[[1]] else getwd(),quiet=TRUE); suppressPackageStartupMessages(library(dplyr))
cons<-list(sqlite=DBI::dbConnect(RSQLite::SQLite(),':memory:'),duckdb=DBI::dbConnect(duckdb::duckdb(shared_home=FALSE)))
ns<-c('.data','.env','NA','NULL','TRUE','FALSE','Inf','NaN','rowid','_rowid_','oid','group','select','value...1','value...2','a b','a`b','a"b','a\nb','α','é','漢字','..marginplyr_set_id_1','..marginplyr_grouping_1')
fails<-list(); checks<-0L
for(i in seq_along(ns)) {
  key<-ns[[i]]; fixed<-paste0('f',i); measure<-paste0('m',i); out<-ns[[length(ns)-i+1]]
  if(out %in% c(key,fixed,measure))out<-paste0(out,'_out')
  d<-setNames(data.frame(f=c('p','p','q'),g=c('a','b','a'),v=c(1,3,5)),c(fixed,key,measure))
  sources<-list(dtplyr=dtplyr::lazy_dt(d),arrow=arrow::arrow_table(d))
  for(b in names(cons)){DBI::dbWriteTable(cons[[b]],'inp',d,overwrite=TRUE);sources[[b]]<-tbl(cons[[b]],'inp')}
  for(b in names(sources))for(s in c('none','last')) {
    checks<-checks+1L
    f<-function(input) summarize_with_margins(input,!!out:=sum(!!rlang::sym(measure)),.by=all_of(fixed),.grouping=rollup(all_of(key)),.margin_label=NULL,.sort=s,.id='set')
    expected<-tryCatch(as.data.frame(f(collect(sources[[b]]))),error=function(e)e)
    actual<-tryCatch(as.data.frame(collect(f(sources[[b]]))),error=function(e)e)
    norm<-function(z){z<-z[vctrs::vec_order(z),,drop=FALSE];rownames(z)<-NULL;z}
    if(inherits(expected,'error')) { cat('SOURCE_OR_ORACLE_REFUSAL',i,b,s,conditionMessage(expected),'\n'); next }; if(inherits(actual,'error')||!isTRUE(all.equal(norm(expected),norm(actual),check.attributes=FALSE))) {
      id<-paste(i,b,s,sep=':');fails[[id]]<-list(key=key,out=out,actual=actual,expected=expected);cat('FAIL',id,'key=',key,'out=',out,if(inherits(actual,'error'))conditionMessage(actual)else paste(all.equal(norm(expected),norm(actual),check.attributes=FALSE),collapse=';'),'\n')
    }
  }
  cat('name',i,'checks',checks,'failures',length(fails),'\n')
}
for(c in cons)DBI::dbDisconnect(c);cat('TOTAL',checks,'FAILURES',length(fails),'\n'); stopifnot(length(fails)==0L)
