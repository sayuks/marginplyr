Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
root <- new.env(parent=globalenv())
root$source <- data.frame(g=c("a","b"),x=c(1,3))
bytes<-serialize(root$source,NULL)
root$visits<-0L
root$track<-function(x) {root$visits<-root$visits+length(x); rep(TRUE,length(x))}
input<-evalq(dplyr::filter(dtplyr::lazy_dt(source),track(g)),root)
root$calls<-0L
root$is.na<-function(x) {root$calls<-root$calls+1L; rep(TRUE,length(x))}
query<-summarize_with_margins(input,value=sum(x),fraction=share_of_total(value),.grouping=rollup(g))
stopifnot(root$calls==0L,root$visits==0L)
cat("Construction: zero callback invocations and zero source-row visits\n")
actual<-dplyr::collect(query)
stopifnot(root$calls>0L,root$visits>0L,identical(actual$fraction,c(NA_real_,NA_real_,1)),identical(bytes,serialize(root$source,NULL)))
cat("Collection: callback invocations",root$calls,"; source-row visits",root$visits,"; source unchanged\n")
root$calls<-0L
caller<-summarize_with_margins(input,value=sum(x),independent=is.na(0),.grouping=rollup(g))
result<-dplyr::collect(caller)
stopifnot(all(result$independent),root$calls>0L)
cat("Caller-written is.na retained ordinary root lookup\n")
