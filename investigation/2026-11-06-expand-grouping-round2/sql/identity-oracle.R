# Outputs use MARGINPLYR_HUNT_OUTPUT, with a dated scratch default.
output_dir <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", unset = "/private/tmp/marginplyr-sql-round2")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(TESTTHAT_PARALLEL='false')
pkgload::load_all('.',quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
obs<-function(expr)tryCatch(suppressWarnings(force(expr)),error=identity)
canon<-function(x){x<-as.data.frame(x);x<-x[do.call(order,lapply(x,as.character)),,drop=FALSE];rownames(x)<-NULL;x}
records<-list();i<-0L
con<-DBI::dbConnect(RSQLite::SQLite(),':memory:')
for(nm in c('v','TRUE','FALSE','TrUe','FaLsE'))for(n in c(0L,1L,3L)){
 data<-tibble(g=rep('a',n),v=seq_len(n),rid=seq_len(n));names(data)[2]<-nm
 DBI::dbWriteTable(con,'input',as.data.frame(data),overwrite=TRUE);src<-tbl(con,'input')
 graphs<-list(root=src,constant=mutate(src,z=1L),window=mutate(src,z=row_number(rid)),collapse=collapse(filter(src,TRUE)),union=union_all(src,src),join=left_join(src,transmute(src,rid,j=rid+1L),by='rid'))
 for(gn in names(graphs)){
  x<-graphs[[gn]];base<-obs(collect(x));identity_result<-obs(collect(mutate(x)));projection<-obs(collect(select(x,everything())))
  control_ok<-!inherits(base,'error')&&!inherits(identity_result,'error')&&!inherits(projection,'error')&&isTRUE(all.equal(canon(base),canon(identity_result)))&&isTRUE(all.equal(canon(base),canon(projection)))
  result<-obs(collect(expand_with_margins(x)))
  status<-if(!control_ok)'ordinary-control-failure'else if(inherits(result,'error'))'expansion-error'else if(isTRUE(all.equal(canon(base),canon(result))))'match'else'mismatch'
  i<-i+1L;records[[i]]<-data.frame(nm,n,graph=gn,status,message=if(inherits(result,'error'))conditionMessage(result)else'')
 }
}
DBI::dbDisconnect(con)
out<-do.call(rbind,records);write.csv(out,file.path(output_dir, "identity-oracle.csv"),row.names=FALSE);print(table(out$status));print(xtabs(~graph+status,out))
