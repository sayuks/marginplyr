suppressPackageStartupMessages({library(dplyr);library(arrow);pkgload::load_all(quiet=TRUE)})
out <- Sys.getenv('MARGINPLYR_HUNT_OUTPUT', unset = file.path(tempdir(), 'marginplyr-expand-hunt-lazy'))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
args <- commandArgs(TRUE)
attempt <- function(e) tryCatch(e,error=identity)
records <- list(); idx <- 0L
bag <- function(x) {x |> arrange(rid,sid) |> as.data.frame()}
for(scenario in c('id','dimension','sort','id_stage','dimension_suffix','unrelated')) for(populated in c(FALSE,TRUE)) {
 part <- switch(scenario,id='sid',dimension='..marginplyr_arrow_dimension_1',sort='..marginplyr_sort_1',id_stage='..marginplyr_arrow_id_1',dimension_suffix='..marginplyr_arrow_dimension_1_',unrelated='unrelated')
 id <- if(scenario=='sort') NULL else if(scenario=='id_stage') '..1' else 'sid'
 root<-tempfile('directory-matrix-'); dir.create(root)
 for(i in 1:2) {
  dir.create(file.path(root,as.character(i)))
  data<-tibble(g=c(9L,8L)[i],rid=i)
  if(scenario=='dimension_suffix') data[['..marginplyr_arrow_dimension_1']]<-i+90L
  if(!populated)data<-data[FALSE,]
  write_parquet(data,file.path(root,as.character(i),'data.parquet'))
 }
 types <- list(g=int32(),rid=int32());if(scenario=='dimension_suffix')types[['..marginplyr_arrow_dimension_1']]<-int32()
 public<-schema(!!!types);partitioning<-DirectoryPartitioning$create(schema(!!!setNames(list(int32()),part)))
 narrow<-open_dataset(root,partitioning=partitioning,schema=public)
 full<-open_dataset(root,partitioning=partitioning)
 shapes<-list(explicit=narrow,with_schema=full$WithSchema(public),project_full=select(full,all_of(names(public))),table=as_arrow_table(narrow),materialized=compute(select(narrow,everything())),filtered=filter(narrow,rid>0L),arranged=arrange(narrow,desc(rid)),union=union_all(narrow,filter(narrow,rid<0L)),joined=left_join(narrow,arrow_table(tibble(rid=1:2)),by='rid'))
 for(shape in names(shapes))for(label in list(NULL,'Total'))for(sort in c('none','last','first')) {
  input<-shapes[[shape]];local<-collect(input)
  expected<-expand_with_margins(local,.grouping=rollup(g),.margin_label=label,.id=id,.sort=sort)
  actual<-attempt(expand_with_margins(input,.grouping=rollup(g),.margin_label=label,.id=id,.sort=sort)|>collect())
  row <- list(scenario=scenario,populated=populated,shape=shape,label=if(is.null(label))'NULL' else label,sort=sort)
  if(inherits(actual,'error')) {row$status<-'ERROR';row$message<-conditionMessage(actual)} else {
   order_ok<-identical(actual$g,expected$g)
   rowbag<-function(x)sort(apply(as.matrix(x),1L,function(z)paste(z,collapse=' | ')))
   samebag<-identical(names(actual),names(expected)) && identical(rowbag(actual),rowbag(expected))
   row$status<-if(samebag && (sort=='none'||order_ok))'PASS' else if(samebag)'ORDER_DIFF' else 'VALUE_DIFF'
   if(!row$status %in% c('PASS','PASS_UNSPECIFIED_ORDER')) {row$actual<-actual;row$expected<-expected}
  }
  idx<-idx+1L;records[[idx]]<-row
 }
 unlink(root,recursive=TRUE)
}
saveRDS(records,file.path(out,'directory-partition-matrix.rds'))
flat <- bind_rows(lapply(records,function(x)as.data.frame(x[c('scenario','populated','shape','label','sort','status')])))
write.csv(flat,file.path(out,'directory-partition-matrix.csv'),row.names=FALSE)
print(count(flat,status));print(count(flat,shape,status)); print(count(flat,scenario,status))
