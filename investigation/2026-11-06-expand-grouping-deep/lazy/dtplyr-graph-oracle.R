Sys.setenv(TESTTHAT_PARALLEL = 'false')
pkgload::load_all('.',quiet=TRUE)
library(dplyr,warn.conflicts=FALSE)
output <- 'investigation/2026-11-06-expand-grouping-deep/lazy'
canonical <- function(x) {
  x <- as_tibble(data.table::as.data.table(x))
  x <- select(x,sort(names(x)))
  arrange(x,across(everything()))
}
shapes <- c('left_join','full_join','inner_join','union','intersect','setdiff','cached_join','cached_union','self_join')
families <- list(
 totalfirst=list(spec=grouping_sets(grouping_set(),grouping_set(g)),sets=list(character(),'g')),
 repeats=list(spec=grouping_sets(grouping_set(g),grouping_set(),grouping_set(g)),sets=list('g',character(),'g')),
 product=list(spec=grouping_spec(rollup(g),rollup(h)),sets=list(c('g','h'),'g','h',character()))
)
records <- list(); k<-0L
for(n in c(0L,1L,3L)) for(shape in shapes) for(family in names(families)) for(label in list(NULL,'TOTAL')) for(sort in c('none','first','last')) {
 counts <- c(a=0L,b=0L)
 probea <- function(x) {counts[['a']]<<-counts[['a']]+1L; rep(TRUE,length(x))}
 probeb <- function(x) {counts[['b']]<<-counts[['b']]+1L; rep(TRUE,length(x))}
 a <- data.table::data.table(g=c('b','a',NA_character_)[seq_len(n)],h=c('x','y','x')[seq_len(n)],rid=seq_len(n),value=seq_len(n)*3L)
 b <- data.table::data.table(g=c('b','a',NA_character_)[seq_len(n)],h=c('x','z','x')[seq_len(n)],rid=seq_len(n),value=seq_len(n)*3L)
 beforea<-data.table::copy(a);beforeb<-data.table::copy(b)
 la<-filter(dtplyr::lazy_dt(a),probea(rid));lb<-filter(dtplyr::lazy_dt(b),probeb(rid))
 if(shape %in% c('cached_join','cached_union')) {
  la<-arrange(compute(la,name='copy'),desc(rid));lb<-arrange(compute(lb,name='other_cache'),rid)
 }
 source <- switch(shape,
 left_join=left_join(la,select(lb,rid,extra=value),by='rid'),
 full_join=full_join(la,select(lb,rid,extra=value),by='rid'),
 inner_join=inner_join(la,select(lb,rid,extra=value),by='rid'),
 cached_join=left_join(la,select(lb,rid,extra=value),by='rid'),
 self_join=left_join(la,select(la,rid,extra=value),by='rid'),
 union=union_all(la,lb),cached_union=union_all(la,lb),
 intersect=intersect(la,lb),setdiff=setdiff(la,lb))
 eager<-as_tibble(data.table::as.data.table(source));native_counts<-counts;counts[]<-0L
 f<-families[[family]]
 expected<-bind_rows(lapply(seq_along(f$sets),function(i) {
  z<-eager
  for(col in setdiff(if(family=='product')c('g','h') else 'g',f$sets[[i]])) z[[col]]<-rep(if(is.null(label))NA_character_ else label,nrow(z))
  z$sid<-rep(as.integer(i),nrow(z));z
 }))
 ordered_expected<-expected
 if(sort!='none' && nrow(expected)) {
  keys<-list()
  for(col in if(family=='product')c('g','h') else 'g') {
   bit<-vapply(expected$sid,function(id)as.integer(!col%in%f$sets[[id]]),integer(1))
   keys<-c(keys,list(if(sort=='first')-bit else bit,is.na(expected[[col]]),expected[[col]]))
  }
  ordered_expected<-expected[do.call(order,c(keys,list(na.last=TRUE,method='radix'))),,drop=FALSE]
 }
 actual<-tryCatch({
  q<-expand_with_margins(source,.grouping=f$spec,.id='sid',.duplicates='keep',.margin_label=label,.sort=sort)
  stopifnot(identical(unname(counts),c(0L,0L)))
  z<-data.table::as.data.table(q)
  list(z=z,counts=counts)
 },error=function(e)e)
 status<-if(inherits(actual,'error'))'package-error' else if(!identical(canonical(actual$z),canonical(expected)))'mismatch' else if(sort!='none'&&!identical(select(as_tibble(actual$z),sort(names(actual$z))),select(ordered_expected,sort(names(ordered_expected)))))'order-mismatch' else if(!identical(actual$counts,native_counts))'evaluation-count' else if(!identical(a,beforea)||!identical(b,beforeb))'mutated-source' else 'match'
 k<-k+1L;records[[k]]<-data.frame(n,shape,family,label=if(is.null(label))'NULL' else label,sort,status,error=if(inherits(actual,'error'))conditionMessage(actual) else '',native_a=native_counts[['a']],native_b=native_counts[['b']],actual_a=counts[['a']],actual_b=counts[['b']])
}
r<-bind_rows(records);write.csv(r,file.path(output,'dtplyr-graph-oracle.csv'),row.names=FALSE)
print(count(r,status));print(utils::head(filter(r,status!='match'),30));stopifnot(all(r$status=='match'))
