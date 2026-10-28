suppressPackageStartupMessages(library(dplyr));pkgload::load_all(if (length(commandArgs(TRUE))) commandArgs(TRUE)[1L] else getwd(),quiet=TRUE)
sc<-DBI::dbConnect(RSQLite::SQLite(),':memory:');dc<-DBI::dbConnect(duckdb::duckdb(shared_home=FALSE),dbdir=':memory:')
nms<-c('.data','.env','.N','.SD','.I','.GRP','.BY','.EACHI','..1','..x','...','sum','n','x y','a`b','a\nb','a\tb',' ',' g','g ','TRUE','NULL','NA','<','==','日本語')
for(typ in c('source','output','fixed','dim'))for(nm in nms){
 dat<-data.frame(f=c('P','P'),g=c('a','b'),x=c(1,3));s<-'value';o<-'share';f<-'f';g<-'g'
 if(typ=='source')s<-nm;if(typ=='output')o<-nm;if(typ=='fixed'){names(dat)[1]<-nm;f<-nm};if(typ=='dim'){names(dat)[2]<-nm;g<-nm}
 for(b in c('local','dt','sqlite','duck')){
  d<-switch(b,local=dat,dt=dtplyr::lazy_dt(dat),sqlite=copy_to(sc,dat,'facts',overwrite=TRUE),duck=copy_to(dc,dat,'facts',overwrite=TRUE))
  z<-tryCatch({q<-summarize_with_margins(d,!!s:=sum(x),!!o:=share_of_total(!!rlang::sym(s)),.by=all_of(f),.grouping=rollup(all_of(g)),.id='set',.check_share_source=FALSE,.margin_label=NULL);r<-if(b=='local')q else collect(q);v<-r[[o]];if(!isTRUE(all.equal(sort(v),c(0.25,0.75,1)))) stop('bad ratio');'PASS'},error=function(e)paste(class(e)[1],gsub('\n',' ',conditionMessage(e))))
  if(z!='PASS')cat('FAIL',typ,encodeString(nm),b,z,'\n')
 }
}
DBI::dbDisconnect(sc);DBI::dbDisconnect(dc,shutdown=TRUE)
