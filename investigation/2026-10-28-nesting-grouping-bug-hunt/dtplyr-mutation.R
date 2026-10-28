# Run from the repository root; exploratory evidence for 2026-10-28.
pkgload::load_all('.',quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
fail<-list();n<-0L
for(mutable in c(FALSE,TRUE)) {
 a<-data.table::data.table(g=c('a','b','a'),h=c('x','y','x'),value=c(1L,2L,3L)); b<-data.table::data.table(g=c('a','b'),v=4:5)
 before_a<-data.table::copy(a);before_b<-data.table::copy(b)
 root<-dtplyr::lazy_dt(a,immutable=!mutable);other<-dtplyr::lazy_dt(b,immutable=TRUE)
 shapes<-list(root=root,filter=filter(root,value>1L),select=select(root,g,h,value),rename=rename(root,hh=h),arrange=arrange(root,value),group=group_by(root,h),mutate=mutate(root,new=value*2L),join_left=left_join(root,other,by='g'),join_right=left_join(other,root,by='g'),union=union_all(root,root),intersect=intersect(root,root),setdiff=setdiff(root,root))
 for(shape in names(shapes))for(verb in list(nest_with_margins,nest_by_with_margins))for(keep in c(FALSE,TRUE)) {
 n<-n+1L;label<-paste(mutable,shape,identical(verb,nest_with_margins),keep)
 ans<-tryCatch(verb(shapes[[shape]],.grouping=rollup(g),.keep=keep,.sort='last'),error=identity)
 if(mutable) {if(!inherits(ans,'marginplyr_error')||!grepl('immutable = FALSE',conditionMessage(ans),fixed=TRUE))fail[[label]]<-ans} else if(inherits(ans,'error')) {fail[[label]]<-ans} else if(inherits(ans,'dtplyr_step')) { ans<-tryCatch(collect(ans),error=identity);if(inherits(ans,'error'))fail[[label]]<-ans }
 if(!identical(a,before_a)||!identical(b,before_b)){cat('SOURCE MUTATION',label,'\n');fail[[label]]<-list(a=a,b=b)}
 }
}
cat('TESTED',n,'FAIL',length(fail),'\n');if(length(fail))print(lapply(fail,function(z)if(inherits(z,'error'))conditionMessage(z)else z));saveRDS(fail,'/tmp/marginplyr-4306-dt-mutation-diffs.rds')
