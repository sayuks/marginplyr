pkgload::load_all('.',quiet=TRUE)
library(dplyr,warn.conflicts=FALSE)
# Native Arrow volatility supplies a value not reproducible by rewriting the oracle.
x <- arrow::Table$create(tibble(g=c('a','b'),rid=1:2)) |> mutate(payload=arrow_random())
base <- expand_with_margins(x,.grouping=rollup(g),.id='id',.margin_label=NULL)
checks <- 0L
for (sort in c('none','last','first')) for (path in c('collect','compute','reader','nested','union')) for(i in 1:8) {
 y <- switch(path,nested=expand_with_margins(base,.grouping=rollup(g),.id='id2',.margin_label=NULL,.sort=sort),union=expand_with_margins(union_all(base,filter(base,rid==1L)),.grouping=rollup(g),.id='id2',.margin_label=NULL,.sort=sort),expand_with_margins(x,.grouping=rollup(g),.id='id',.margin_label=NULL,.sort=sort))
 out <- switch(path,compute=collect(compute(y)),reader=collect(arrow::as_arrow_table(arrow::as_record_batch_reader(y))),collect(y))
 if(path %in% c('nested','union')) {
  a <- out |> filter(id2==1L) |> arrange(rid,id,payload) |> select(-g,-id2)
  b <- out |> filter(id2==2L) |> arrange(rid,id,payload) |> select(-g,-id2)
 } else {
  a <- out |> filter(id==1L) |> arrange(rid) |> select(-g,-id)
  b <- out |> filter(id==2L) |> arrange(rid) |> select(-g,-id)
 }
 stopifnot(identical(a,b));checks<-checks+1L
}
cat('Arrow deferred source copy invariants:',checks,'passed\n')
# dtplyr predicates and cached sources must likewise run once per collection.
make_dt <- function() {
 e<-new.env();e$n<-0L
 src<-dtplyr::lazy_dt(data.table::data.table(g=c('a','b'),rid=1:2))
 pred<-function(x) {e$n<-e$n+1L; rep(TRUE,length(x))}
 list(query=filter(src,pred(rid)),e=e)
}
checks<-0L
for (cache in c(FALSE,TRUE)) for (sort in c('none','last','first')) {
 z<-make_dt(); src<-if(cache) compute(z$query,name='cached_greedy') else z$query
 y<-expand_with_margins(src,.grouping=rollup(g),.id='id',.margin_label=NULL,.sort=sort)
 stopifnot(z$e$n==0L)
 for(i in 1:8) {
  before<-z$e$n;out<-as.data.frame(data.table::as.data.table(y)); stopifnot(z$e$n==before+1L,nrow(out)==4L);checks<-checks+1L
 }
}
cat('dtplyr source evaluation invariants:',checks,'passed\n')
