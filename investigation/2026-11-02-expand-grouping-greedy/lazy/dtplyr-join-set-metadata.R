# Dated metadata fidelity audit of join/set row-shape boundaries.
Sys.setenv(TESTTHAT_PARALLEL = "false")
suppressPackageStartupMessages(library(dplyr))
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
out <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", "investigation/2026-11-02-expand-grouping-greedy/lazy")
capture <- function(expr) tryCatch(expr,error=identity)
shapes <- list(both_empty=list(integer(),integer()),left_empty=list(integer(),c(1L,2L)),right_empty=list(c(1L,2L),integer()),matched=list(c(1L,2L),c(1L,2L)),unmatched=list(c(1L,2L),c(3L,4L)),partial_na=list(c(1L,NA_integer_,2L),c(2L,NA_integer_,3L)))
converters <- list(logical=as.logical,integer=as.integer,double=as.double,character=as.character)
operations <- list(inner=function(x,y)inner_join(x,y,by="k",relationship="many-to-many"),left=function(x,y)left_join(x,y,by="k",relationship="many-to-many"),right=function(x,y)right_join(x,y,by="k",relationship="many-to-many"),full=function(x,y)full_join(x,y,by="k",relationship="many-to-many"),semi=function(x,y)semi_join(x,y,by="k"),anti=function(x,y)anti_join(x,y,by="k"),cross=function(x,y)cross_join(x,y),union=union,union_all=union_all,intersect=intersect,setdiff=setdiff)
predicates <- list(logical=is.logical,integer=is.integer,double=is.double,character=is.character,numeric=is.numeric)
rows <- list(); k<-0L
for(type in names(converters))for(shape in names(shapes)) {
 keys<-shapes[[shape]]
 x<-data.table::data.table(k=converters[[type]](keys[[1L]]),v=seq_along(keys[[1L]]),p=rep(c(TRUE,FALSE),length.out=length(keys[[1L]])))
 y<-data.table::data.table(k=converters[[type]](keys[[2L]]),v=seq_along(keys[[2L]])+10L,p=rep(c(FALSE,TRUE),length.out=length(keys[[2L]])))
 beforex<-data.table::copy(x);beforey<-data.table::copy(y)
 for(op in names(operations))for(cached in c(FALSE,TRUE)) {
  dx<-dtplyr::lazy_dt(x);dy<-dtplyr::lazy_dt(y)
  if(cached) {dx<-compute(dx,name="cache_left");dy<-compute(dy,name="cache_right")}
  q<-capture(operations[[op]](dx,dy));dat<-if(inherits(q,"error"))q else capture(collect(q))
  for(pred in names(predicates)) {
   f<-predicates[[pred]]
   expected<-if(inherits(dat,"error"))dat else names(dat)[vapply(dat,f,logical(1))]
   actual<-if(inherits(dat,"error"))dat else capture(inspect_grouping(q,.by=where(f),.grouping=grouping_set(),.format="list")$fixed[[1L]])
   status<-if(inherits(dat,"error"))"source_error"else if(inherits(actual,"error")) {if(grepl("safely determine column metadata",conditionMessage(actual),fixed=TRUE))"metadata_refusal"else"package_error"}else if(!identical(actual,expected))"metadata_diff"else"pass"
   k<-k+1L;rows[[k]]<-data.frame(type=type,shape=shape,operation=op,cached=cached,predicate=pred,status=status,message=if(inherits(actual,"error"))conditionMessage(actual)else"")
   if(status%in%c("package_error","metadata_diff"))cat("LEAD",type,shape,op,cached,pred,status,rows[[k]]$message,"\n")
  }
  stopifnot(identical(x,beforex),identical(y,beforey))
 }
}
res<-bind_rows(rows);write.csv(res,file.path(out,"dtplyr-join-set-results.csv"),row.names=FALSE)
print(table(res$status));print(table(res$operation,res$status))
