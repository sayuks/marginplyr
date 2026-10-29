# Investigated 2026-10-29 against ee43217 main; read-only package exploration.
suppressPackageStartupMessages(library(dplyr))
dir.create('/tmp/lazy-deep-hunt', showWarnings = FALSE)
pkgload::load_all('.', quiet=TRUE)
scenarios<-list(identifier=list(part='sid',id='sid',block=FALSE),dimension=list(part='..marginplyr_arrow_dimension_1',id='sid',block=FALSE),dimension_suffix=list(part='..marginplyr_arrow_dimension_1_',id='sid',block=TRUE),private_id=list(part='..marginplyr_arrow_id_1',id='..1',block=FALSE),sort=list(part='..marginplyr_sort_1',id=NULL,block=FALSE),unrelated=list(part='hidden_unrelated',id='sid',block=FALSE))
canon<-function(x){x<-as.data.frame(x);x<-x[do.call(order,c(lapply(x,as.character),list(na.last=TRUE))),,drop=FALSE];rownames(x)<-NULL;x}
rows<-list();i<-0L
for(scenario in names(scenarios)){
s<-scenarios[[scenario]];part<-s$part;path<-tempfile();dir.create(path)
data<-data.frame(g=9:8,rid=1:2);if(s$block)data[['..marginplyr_arrow_dimension_1']]<-101:102
for(j in 1:2){p<-file.path(path,paste0(part,'=',j));dir.create(p);arrow::write_parquet(data[j,,drop=FALSE],file.path(p,'f.parquet'))}
public_schema<-arrow::Table$create(data)$schema
full<-arrow::open_dataset(path, factory_options=list(selector_ignore_prefixes=character()),partitioning=arrow::hive_partition(!!!setNames(list(arrow::int32()),part)))
narrow<-arrow::open_dataset(path,schema=public_schema,factory_options=list(selector_ignore_prefixes=character()),partitioning=arrow::hive_partition(!!!setNames(list(arrow::int32()),part)))
shapes<-list(projection=select(full,all_of(names(data))),narrow_open=narrow,withschema=full$WithSchema(public_schema),union_narrow=arrow::open_dataset(list(full,full),schema=public_schema),narrow_filter=filter(narrow,rid>0L),narrow_collapse=collapse(narrow),materialized=compute(narrow),table=arrow::Table$create(data))
for(shape in names(shapes))for(empty in c(FALSE,TRUE))for(label in c('null','text'))for(sort in c('none','first','last')){
x<-shapes[[shape]];if(empty)x<-filter(x,rid<0L)
source<-collect(x);stopifnot(identical(names(source),names(data)),nrow(source)==if(empty)0L else if(shape=='union_narrow')4L else 2L)
lab<-if(label=='null')NULL else 'Total';expected<-expand_with_margins(source,.grouping=rollup(g),.margin_label=lab,.id=s$id,.sort=sort)
got<-tryCatch(collect(expand_with_margins(x,.grouping=rollup(g),.margin_label=lab,.id=s$id,.sort=sort)),error=identity)
status<-if(inherits(got,'error'))'error' else if(!identical(canon(got),canon(expected)))'value_difference' else if(sort!='none'&&!identical(got$g,expected$g))'order_difference' else 'pass'
i<-i+1L;rows[[i]]<-data.frame(scenario,shape,empty,label,sort,status,message=if(inherits(got,'error'))conditionMessage(got) else '')
}
unlink(path,recursive=TRUE)
}
results<-do.call(rbind,rows);write.csv(results,'/tmp/lazy-deep-hunt/arrow-narrow-results.csv',row.names=FALSE)
cat('CASES',nrow(results),'\n');print(table(results$status));print(with(results,table(scenario,shape,status)))
stopifnot(all(results$status[results$shape %in% c('projection','materialized','table')]=='pass'),all(results$status[results$scenario=='unrelated']=='pass'))
