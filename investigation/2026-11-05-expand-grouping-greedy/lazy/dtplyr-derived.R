pkgload::load_all('.',quiet=TRUE)
library(dplyr,warn.conflicts=FALSE)
shapes <- list(
 identity=function(x)x,
 filter=function(x)filter(x,rid!=2L),
 arrange=function(x)arrange(x,desc(rid)),
 select=function(x)select(x,payload,rid,g),
 rename=function(x)rename(x,rid2=rid),
 mutate=function(x)mutate(x,payload=payload+1L),
 cache=function(x)compute(x,name='greedy_cache'),
 cache_arrange=function(x)arrange(compute(x,name='greedy_cache'),desc(rid)),
 cache_select=function(x)select(compute(x,name='greedy_cache'),payload,rid,g)
)
canon <- function(x) {
 as_tibble(x) |> select(sort(names(x))) |> arrange(across(everything()))
}
checks<-0L
for(shape in names(shapes))for(n in c(0L,1L,3L))for(label in list(NULL,'Total'))for(sort in c('none','last','first')) {
 data<-tibble(g=c('b','a',NA)[seq_len(n)],rid=seq_len(n),payload=seq_len(n)*3L)
 q<-shapes[[shape]](dtplyr::lazy_dt(data))
 eager<-as_tibble(data.table::as.data.table(q))
 actual<-expand_with_margins(q,.grouping=rollup(g),.id='sid',.margin_label=label,.sort=sort) |>
  data.table::as.data.table() |> as_tibble()
 expected<-expand_with_margins(eager,.grouping=rollup(g),.id='sid',.margin_label=label,.sort=sort)
 stopifnot(identical(canon(actual),canon(expected)))
 if(sort!='none') stopifnot(identical(actual,expected))
 checks<-checks+1L
}
cat('dtplyr derived/cached eager comparisons:',checks,'passed\n')
