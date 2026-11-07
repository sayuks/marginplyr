pkgload::load_all(".",quiet=TRUE)
Sys.setenv(TESTTHAT_PARALLEL="false")
types <- list(complex=c(1+2i,2+3i),list=list(1L,2L),raw=as.raw(1:2),matrix=I(matrix(1:4,2)))
for(n in names(types)) {
  cat("\n",n,"\n")
  dat <- tibble::tibble(g=types[[n]],v=1:2)
  input <- tryCatch(dtplyr::lazy_dt(dat),error=identity)
  if(inherits(input,"error")) {
    cat("native lazy_dt construction:",conditionMessage(input),"\n")
    next
  }
  probes <- list(
    native_nest=function() dplyr::collect(tidyr::nest(input,data=-g)),
    native_union=function() dplyr::collect(dplyr::union_all(input,input)),
    included_nest=function() dplyr::collect(nest_with_margins(input,.grouping=grouping_set(g),.margin_label=NULL)),
    rollup_nest=function() dplyr::collect(nest_with_margins(input,.grouping=rollup(g),.margin_label=NULL)))
  for(label in names(probes)) {
    cat(label,": ")
    print(tryCatch({z<-probes[[label]]();paste("OK",nrow(z))},error=function(e) conditionMessage(e)))
  }
}
