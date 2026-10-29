pkgload::load_all(quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
scenarios <- list(
 ordinary=list(part='hidden',id='sid',block=FALSE),
 dimension=list(part='..marginplyr_arrow_dimension_1',id='sid',block=FALSE),
 suffix=list(part='..marginplyr_arrow_dimension_1_',id='sid',block=TRUE),
 id=list(part='..marginplyr_arrow_id_1',id='..1',block=FALSE),
 sort=list(part='..marginplyr_sort_1',id=NULL,block=FALSE)
)
canon <- function(x,id){x<-as.data.frame(x); pos<-if(is.null(id)) order(x$row_id,is.na(x$g),x$g)else order(x[[id]],x$row_id);x<-x[pos,,drop=FALSE];rownames(x)<-NULL;x}
rows<-list();i<-0L
for(sc in names(scenarios)) for(pt in c('integer','string')) {
 s<-scenarios[[sc]];part<-s$part;typ<-if(pt=='integer')arrow::int32()else arrow::utf8();pv<-if(pt=='integer')1:2 else c('a','b')
 path<-tempfile();dir.create(path)
 for(j in 1:2){p<-file.path(path,paste0(part,'=',pv[[j]]));dir.create(p);d<-data.frame(g=10L-j,row_id=j);if(s$block)d[['..marginplyr_arrow_dimension_1']]<-100L+j;arrow::write_parquet(d,file.path(p,'f.parquet'),compression='uncompressed')}
 data<-data.frame(g=9:8,row_id=1:2);if(s$block)data[['..marginplyr_arrow_dimension_1']]<-101:102
 types<-c(list(arrow::int32(),arrow::int32()),if(s$block)list(arrow::int32())else list(),list(typ));nms<-c(names(data),part)
 ds<-arrow::open_dataset(path,factory_options=list(selector_ignore_prefixes=character()),schema=arrow::schema(!!!setNames(types,nms)),partitioning=arrow::hive_partition(!!!setNames(list(typ),part)))
 raw<-collect(ds);stopifnot(identical(as.data.frame(raw[order(raw$row_id),names(data),drop=FALSE]),data));stopifnot(length(ds$files)==2L)
 src<-select(ds,all_of(names(data)))
 shapes<-list(projected=src,filtered=filter(src,row_id>0),collapsed=collapse(src),aliased=rename(rename(src,h=g),g=h),materialized=compute(src),table=arrow::Table$create(data),inmemory=arrow::InMemoryDataset$create(arrow::Table$create(data)))
 for(shape in names(shapes))for(empty in c(FALSE,TRUE))for(lab in c('null','text'))for(sort in c('none','first','last')){
  input<-shapes[[shape]];if(empty)input<-filter(input,row_id<0L)
  want<-data;tot<-data;tot$g<-if(lab=='null')NA_integer_ else 'Total';if(lab=='text')want$g<-as.character(want$g)
  if(!is.null(s$id)){want[[s$id]]<-1L;tot[[s$id]]<-2L};want<-rbind(want,tot);if(empty)want<-want[FALSE,,drop=FALSE]
  actual<-tryCatch(collect(expand_with_margins(input,.grouping=rollup(g),.margin_label=if(lab=='null')NULL else 'Total',.id=s$id,.sort=sort)),error=identity)
  status<-if(inherits(actual,'error'))'error'else if(!isTRUE(all.equal(canon(actual,s$id)[names(want)],canon(want,s$id))))'value_diff'else if(sort!='none'&&!empty&&!identical(actual$g,if(sort=='first')c(tot$g,sort(want$g[seq_len(2)]))else c(sort(want$g[seq_len(2)]),tot$g)))'order_diff'else'pass'
  i<-i+1L;rows[[i]]<-data.frame(scenario=sc,type=pt,shape=shape,empty=empty,label=lab,sort=sort,status=status,error=if(inherits(actual,'error'))conditionMessage(actual)else'')
 }
 unlink(path,recursive=TRUE)
}
res<-do.call(rbind,rows);write.csv(res,'/tmp/marginplyr-audit-partition-matrix.csv',row.names=FALSE);print(table(res$status));print(with(res,table(scenario,shape,status)))
