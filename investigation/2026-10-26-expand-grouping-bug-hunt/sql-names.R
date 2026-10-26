# Investigation evidence, 2026-10-26. Run from the repository root.
suppressPackageStartupMessages(pkgload::load_all('.', quiet=TRUE))
nms <- c('a','T','F','NA','TRUE','FALSE','NULL','NaN','Inf','.N','.SD','.I','.GRP','.BY','.EACHI','.data','.env','..marginplyr_set_id','region, store','a\nb','a\rb','a`b','a"b',"a'b",'a\\b','x y','é','日本語')
cs <- list(sqlite=DBI::dbConnect(RSQLite::SQLite(),':memory:'),duckdb=DBI::dbConnect(duckdb::duckdb(shared_home = FALSE)))
failures <- list();total<-0L
for (backend in names(cs)) for (nm in nms) for(id in c('sid','.data','.env','TRUE','a"b')) {
 if(id==nm) next
 x<-tibble::as_tibble(setNames(list(c('a','b'),c(1L,2L)),c(nm,'value')))
 sp<-rlang::inject(rollup(dplyr::all_of(!!nm)))
 src<-dplyr::copy_to(cs[[backend]],x,overwrite=TRUE,name='probe')
 for(srt in c('none','first','last')) {
  total<-total+1L
  got<-tryCatch(expand_with_margins(src,.grouping=sp,.id=id,.sort=srt) |> dplyr::collect(),error=identity)
  msg<-if(inherits(got,'error')) conditionMessage(got) else if(nrow(got)!=4L || !identical(sort(got[[id]]),c(1L,1L,2L,2L)) || !identical(sort(got$value),c(1L,1L,2L,2L)) || sum(got[[nm]]=='Total')!=2L) paste(capture.output(print(got)),collapse=' ') else NULL
  if(!is.null(msg))failures[[length(failures)+1L]]<-list(backend=backend,name=nm,id=id,sort=srt,message=msg)
 }
}
cat('Cases:',total,' Failures:',length(failures),'\n');print(head(failures,6L));saveRDS(failures,'/tmp/marginplyr-root-sql-names-failures.rds')
lapply(cs,DBI::dbDisconnect)
