# Investigated: 2026-10-29 against ee4321789ca60b6259c53df2852f1574aaede584.
pkgload::load_all('.',quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
n<-0L;refused<-0L;failed<-list()
normal<-function(x){x<-ungroup(collect(x));x$data<-lapply(x$data,function(z)as_tibble(arrange(as_tibble(z),row)));as_tibble(arrange(x,across(all_of(intersect(c('sid','g','h'),names(x))))))}
ops<-list(root=identity,cache=function(x)compute(x),cachearrange=function(x)arrange(compute(x),h),cacherename=function(x)rename(rename(compute(x),z=h),h=z),cacheunion=function(x)union_all(compute(x),compute(x)),cachedgroup=function(x)group_by(compute(x),g),groupcache=function(x)compute(group_by(x,g)),cache2=function(x)compute(compute(x)))
for(grouped in c(FALSE,TRUE))for(op in names(ops))for(keep in c(FALSE,TRUE))for(sort in c('none','first','last'))for(verb in list(nest_with_margins,nest_by_with_margins)){
 src<-data.table::data.table(g=c('b','a','a'),h=c('x','y','x'),row=1:3);old<-data.table::copy(src);root<-dtplyr::lazy_dt(src);if(grouped)root<-group_by(root,g);q<-ops[[op]](root)
 real<-collect(q)
 got<-tryCatch(normal(verb(q,.grouping=rollup(h),.keep=keep,.id='sid',.sort=sort)),error=identity)
 expected<-tryCatch(normal(verb(real,.grouping=rollup(h),.keep=keep,.id='sid',.sort=sort)),error=identity)
 n<-n+1L;stopifnot(identical(src,old))
 if(inherits(got,'marginplyr_error')&&grepl("Can't safely determine column metadata",conditionMessage(got),fixed=TRUE)){refused<-refused+1L;next}
 if(!identical(got,expected)){failed[[paste(grouped,op,keep,sort,deparse(substitute(verb)))]]<-list(got=got,expected=expected);cat('FAIL',grouped,op,keep,sort,'\n')}
}
cat('Cases',n,'metadata refusals',refused,'mismatches',length(failed),'source snapshots preserved.\n');if(length(failed))print(head(failed,2L));stopifnot(!length(failed))
