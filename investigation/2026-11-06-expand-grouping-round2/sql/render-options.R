# Outputs use MARGINPLYR_HUNT_OUTPUT, with a dated scratch default.
output_dir <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", unset = "/private/tmp/marginplyr-sql-round2")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(TESTTHAT_PARALLEL='false')
pkgload::load_all('.',quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
observe<-function(expr)tryCatch(suppressWarnings(force(expr)),error=identity)
canon<-function(x){if(inherits(x,'error'))return(x);x<-as.data.frame(x);x<-x[do.call(order,lapply(x,as.character)),,drop=FALSE];rownames(x)<-NULL;x}
records<-list();failures<-list();i<-0L
for(engine in c('sqlite','duckdb')){
 con<-if(engine=='sqlite') DBI::dbConnect(RSQLite::SQLite(),':memory:') else DBI::dbConnect(duckdb::duckdb(shared_home=FALSE),':memory:')
 for(nm in c('v','TRUE','.env','..0')){
  input<-tibble(g=c('b','a',NA),v=c(7L,8L,9L),rid=1:3); names(input)[2]<-nm
  DBI::dbWriteTable(con,'input',as.data.frame(input),overwrite=TRUE); src<-tbl(con,'input')
  for(graph in c('root','window','binary','collapse')){
   x<-switch(graph,root=src,window=mutate(src,rn=row_number(rid)),binary=union_all(filter(src,rid<3),filter(src,rid>1)),collapse=collapse(filter(src,rid>0)))
   for(sort in c('none','last')) for(label in c('null','text')){
    m<-expand_with_margins(x,.grouping=rollup(g),.margin_label=if(label=='null')NULL else 'Total',.id='sid',.sort=sort)
    control<-observe(collect(m)); if(inherits(control,'error')){cat('DEFAULT EXCLUSION',engine,nm,graph,sort,label,conditionMessage(control),'
');next}
    for(cte in c(FALSE,TRUE)) for(star in c(FALSE,TRUE)) for(qualify in c(FALSE,TRUE)){
     opts<-dbplyr::sql_options(cte=cte,use_star=star,qualify_all_columns=qualify)
     got<-observe(collect(m,sql_options=opts));state<-if(inherits(got,'error')) 'error' else if(isTRUE(all.equal(canon(got),canon(control))))'match' else 'mismatch'
     i<-i+1L;records[[i]]<-data.frame(engine,nm,graph,sort,label,cte,star,qualify,state,message=if(inherits(got,'error'))conditionMessage(got)else'')
     if(state!='match') failures[[as.character(i)]]<-list(case=records[[i]],control=control,got=got,sql=observe(as.character(dbplyr::sql_render(m,sql_options=opts))))
    }
   }
  }
 }
 DBI::dbDisconnect(con,shutdown=engine=='duckdb')
}
out<-do.call(rbind,records);write.csv(out,file.path(output_dir, "render-options.csv"),row.names=FALSE);saveRDS(failures,file.path(output_dir, "render-options-failures.rds"));print(table(out$engine,out$state));if(length(failures))print(do.call(rbind,lapply(failures,function(x)x$case)))
