args<-commandArgs(TRUE);root<-if(length(args))args[[1]] else getwd();pkgload::load_all(root,quiet=TRUE);suppressPackageStartupMessages(library(dplyr))
cons<-list(sqlite=DBI::dbConnect(RSQLite::SQLite(),':memory:'),duckdb=DBI::dbConnect(duckdb::duckdb(shared_home=FALSE)))
checks<-0L;fail<-0L
for(b in names(cons)) {
 src<-copy_to(cons[[b]],data.frame(g=c('a','b'),x=c(1,3)),'inp')
 for(n in c('..0','..00','..01','..-1')) for(role in c('output','identifier')) for(s in c('none','last','first')) for(grp in c(FALSE,TRUE)) {
 checks<-checks+1L
 baseline<-tryCatch(collect(if(role=='output')summarise(src,!!n:=sum(x))else mutate(summarise(src,value=sum(x)),!!n:=1L)),error=identity)
 stopifnot(!inherits(baseline,'error'))
 result<-tryCatch(collect(if(role=='output')summarize_with_margins(src,!!n:=sum(x),.grouping=if(grp)rollup(g)else NULL,.sort=s,.margin_label=NULL)else summarize_with_margins(src,value=sum(x),.id=n,.grouping=if(grp)rollup(g)else NULL,.sort=s,.margin_label=NULL)),error=identity)
 if(inherits(result,'error')) {fail<-fail+1L;cat('FAIL',b,n,role,s,grp,conditionMessage(result),'\n')} else {stopifnot(n %in% names(result));cat('PASS',b,n,role,s,grp,'\n')}
 }
}
for(c in cons)DBI::dbDisconnect(c)
cat('CHECKS',checks,'FAIL',fail,'\n')
