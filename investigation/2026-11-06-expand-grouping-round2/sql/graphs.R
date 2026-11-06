# Outputs use MARGINPLYR_HUNT_OUTPUT, with a dated scratch default.
output_dir <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", unset = "/private/tmp/marginplyr-sql-round2")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(TESTTHAT_PARALLEL='false')
pkgload::load_all('.',quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
observe <- function(expr) tryCatch(suppressWarnings(force(expr)),error=identity)
canon <- function(x) {
  if(inherits(x,'error')) return(x)
  z<-as.data.frame(x); rownames(z)<-NULL
  z<-z[do.call(order,lapply(z,as.character)),,drop=FALSE]; rownames(z)<-NULL; z
}
record<-list(); failures<-list(); i<-0L
for(engine in c('sqlite','duckdb')) {
 con<-if(engine=='sqlite') DBI::dbConnect(RSQLite::SQLite(),':memory:') else DBI::dbConnect(duckdb::duckdb(shared_home=FALSE),':memory:')
 for(nm in c('v','TRUE','FALSE','.env','.data','..0','NULL','..marginplyr_expand_1')) {
  data<-tibble(g=c('b','a',NA,'a','c'), k=c(2L,1L,3L,1L,NA_integer_), v=c(7L,8L,9L,8L,0L),rid=1:5)
  names(data)[3]<-nm
  DBI::dbWriteTable(con,'input',as.data.frame(data),overwrite=TRUE)
  src<-tbl(con,'input')
  nmq<-rlang::sym(nm)
  graphs<-list(root=src,filter=filter(src,rid>1),distinct=distinct(src,g,k,.keep_all=TRUE),head=head(arrange(src,rid),3),window=mutate(src,rn=row_number(rid)),rankfilter=filter(mutate(src,rn=row_number(rid)),rn<4),windoworder=arrange(src,rid),collapse=collapse(filter(src,rid>1)),slice_min=slice_min(src,k,n=3,with_ties=FALSE),slice_max=slice_max(src,k,n=3,with_ties=TRUE),unionall=union_all(filter(src,rid<4),filter(src,rid>2)),union=union(filter(src,rid<4),filter(src,rid>2)),intersect=intersect(src,filter(src,rid>1)),setdiff=setdiff(src,filter(src,rid>3)),semijoin=semi_join(src,select(filter(src,rid<3),k),by='k'),antijoin=anti_join(src,select(filter(src,rid<3),k),by='k'),leftjoin=left_join(src,transmute(src,rid,j=rid+1L),by='rid'),count=add_count(src,k,name='n'))
  for(gn in names(graphs)) {
   x<-graphs[[gn]]; base<-observe(collect(x))
   for(sort in c('none','first','last')) for(label in c('null','text')) {
    ml<-if(label=='null') NULL else 'Total'
    expected<-if(inherits(base,'error')) base else observe(expand_with_margins(base,.grouping=rollup(g),.margin_label=ml,.id='sid',.sort=sort))
    result<-observe(expand_with_margins(x,.grouping=rollup(g),.margin_label=ml,.id='sid',.sort=sort))
    got<-if(inherits(result,'error')) result else observe(collect(result))
    state<-if(inherits(base,'error')) 'source-error' else if(inherits(expected,'error')) 'oracle-error' else if(inherits(got,'error')) 'package-error' else if(isTRUE(all.equal(canon(got),canon(expected),check.attributes=TRUE))) 'match' else 'mismatch'
    i<-i+1L; record[[i]]<-data.frame(engine,nm,graph=gn,sort,label,state,message=if(inherits(got,'error'))conditionMessage(got)else'')
    if(state %in% c('package-error','mismatch')) failures[[paste(engine,nm,gn,sort,label,sep='/')]]<-list(base=base,expected=expected,got=got,sql=if(inherits(result,'error')) '' else observe(as.character(dbplyr::sql_render(result))))
   }
  }
 }
 DBI::dbDisconnect(con,shutdown=engine=='duckdb')
}
out<-do.call(rbind,record);write.csv(out,file.path(output_dir, "graphs.csv"),row.names=FALSE); saveRDS(failures,file.path(output_dir, "graphs-failures.rds"));print(table(out$engine,out$state)); if(length(failures)) print(head(names(failures),50))
