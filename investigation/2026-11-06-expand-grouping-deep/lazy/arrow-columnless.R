Sys.setenv(TESTTHAT_PARALLEL='false')
pkgload::load_all('.',quiet=TRUE)
library(dplyr,warn.conflicts=FALSE)
library(arrow,warn.conflicts=FALSE)
output<-'investigation/2026-11-06-expand-grouping-deep/lazy'
tmp<-tempfile('arrow-columnless-');dir.create(tmp)
families<-list(single=grouping_set(),repeated=grouping_sets(grouping_set(),grouping_set()))
r<-list();k<-0L
for(n in c(0L,1L,3L)) {
 t<-Table$create(g=rep('a',n),rid=seq_len(n));path<-file.path(tmp,paste0(n,'.arrow'));write_ipc_file(t,path)
 ds<-open_dataset(path,format='ipc');sources<-list(table=t,batch=as_record_batch(t),dataset=ds,uniondataset=open_dataset(list(ds,ds)))
 for(shape in names(sources)) for(family in names(families)) for(sort in c('none','first','last')) {
  source<-select(sources[[shape]],!everything());nr<-collect(source,as_data_frame=FALSE)$num_rows
  expected_rows<-nr*if(family=='single')1L else 2L
  q<-expand_with_margins(source,.grouping=families[[family]],.duplicates='keep',.id='sid',.sort=sort)
  for(consumer in c('collect','compute','reader')) {
   actual<-switch(consumer,collect=collect(q,as_data_frame=FALSE),compute=compute(q),reader=as_arrow_table(as_record_batch_reader(q)))
   expected<-rep(seq_len(if(family=='single')1L else 2L),each=nr)
   matched<-identical(names(actual),'sid')&&actual$num_rows==expected_rows&&identical(sort(as.vector(actual$GetColumnByName('sid'))),sort(expected))
   k<-k+1L;r[[k]]<-data.frame(n,shape,family,sort,consumer,source_rows=nr,expected_rows,actual_rows=actual$num_rows,matched)
  }
 }
}
r<-bind_rows(r);write.csv(r,file.path(output,'arrow-columnless.csv'),row.names=FALSE);print(count(r,matched));unlink(tmp,recursive=TRUE);stopifnot(all(r$matched))
