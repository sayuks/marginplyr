# Run from the repository root; exploratory evidence for 2026-10-28.
pkgload::load_all('.',quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
set.seed(20261028)
x<-tibble(a=sample(c('x','y',NA),16,TRUE),b=sample(c(1L,2L,NA_integer_),16,TRUE),c=sample(c(FALSE,TRUE,NA),16,TRUE),f=rep(1:2,8),v=seq_len(16))
specs<-list(NULL,grouping_set(),grouping_set(a),grouping_set(c(a,b)),rollup(a,b,c),cube(a,b,c),rollup(grouping_set(a,b),c),cube(grouping_set(a,b),c),grouping_sets(a,b,grouping_set()),grouping_spec(rollup(a),cube(b,c)),grouping_sets(cube(a,b),rollup(a,b),grouping_set(c)))
con1<-DBI::dbConnect(duckdb::duckdb(shared_home=FALSE));con2<-DBI::dbConnect(RSQLite::SQLite())
sources<-list(local=x,dtplyr=dtplyr::lazy_dt(x),arrow=arrow::Table$create(x),duckdb=copy_to(con1,x,'data',temporary=TRUE),sqlite=copy_to(con2,x,'data',temporary=TRUE))
fail<-0L;n<-0L
for(src in names(sources)) for(i in seq_along(specs)) for(fixed in c(FALSE,TRUE)) for(dups in c('drop','keep')) {
 data<-sources[[src]];s<-specs[[i]];by<-if(fixed) 'f' else character()
 plan<-inspect_grouping(data,.by=all_of(by),.grouping=s,.duplicates=dups,.format='list')
 result<-tryCatch(collect(summarize_with_margins(data,n=n(),gid=grouping_id(),.by=all_of(by),.grouping=s,.duplicates=dups,.id='sid',.margin_label=NULL)),error=identity)
 n<-n+1L
 if(inherits(result,'error')) {cat('ERROR',src,i,fixed,dups,conditionMessage(result),'\n');fail<-fail+1L;next}
 ids<-plan$grouping_id[match(result$sid,plan$set_id)]
 if(!isTRUE(all.equal(as.integer(result$gid),ids))) {cat('IDS',src,i,fixed,dups,'\n');fail<-fail+1L}
 expected_sets<-plan$set_id
 if(!setequal(unique(result$sid),expected_sets)) {cat('MISSING SET',src,i,fixed,dups,'\n');fail<-fail+1L}
 rows<-aggregate(result$n,list(result$sid),sum)$x
 if(any(rows!=nrow(x))) {cat('MEMBERSHIP',src,i,fixed,dups,rows,'\n');fail<-fail+1L}
}
DBI::dbDisconnect(con1,shutdown=TRUE);DBI::dbDisconnect(con2)
cat('CHECKED',n,'FAILED',fail,'\n')
stopifnot(fail==0L)
