Sys.setenv(TESTTHAT_PARALLEL="false")
args <- commandArgs(trailingOnly=TRUE)
pkgload::load_all(args[[1]],quiet=TRUE)
library(dplyr)
set.seed(20261107)
keys <- list(character=c("a","a",NA,"b","b",NA), numeric=c(0,-0,NA,NaN,Inf,-Inf), date=as.Date(c(1,1,2,NA,2,NA),origin="1970-01-01"), factor=factor(c("a","a",NA,"b","b",NA)), list=list(1,1,2,NULL,2,NULL), matrix=matrix(c(1,1,2,NA,2,NA,2,2,3,4,3,4),ncol=2), packed=tibble(a=c(1,1,2,NA,2,NA),b=c("x","x","y","z","y","z")))
count <- 0L
for (kn in names(keys)) for (kind in c("local","dtplyr")) for (sort in c("none","first","last")) {
 d <- tibble(k=keys[[kn]],g=c("a","b","a","b",NA,NA),value=c(2,3,5,7,11,13))
 input <- if(kind=="dtplyr") dtplyr::lazy_dt(d) else d
 out <- tryCatch(summarize_with_margins(input,n=sum(value),p=share_of_parent(n),t=share_of_total(n),.by=k,.grouping=rollup(g),.id="sid",.margin_label=NULL,.sort=sort) |> as_tibble(),error=identity)
 if(inherits(out,"error")) {cat(kn,kind,sort,"ERROR",conditionMessage(out),"
");next}
 groups <- vctrs::vec_group_id(out$k)
 for(i in seq_len(nrow(out))) { root <- which(groups==groups[[i]] & out$sid==2L); stopifnot(length(root)==1L); expected <- if(out$sid[[i]]==2L) 1 else out$n[[i]]/out$n[[root]]; stopifnot(identical(out$p[[i]],expected),identical(out$t[[i]],expected)) }
 count <- count+1L; cat(kn,kind,sort,"PASS
")
}
cat("successful comparisons:",count,"
")
