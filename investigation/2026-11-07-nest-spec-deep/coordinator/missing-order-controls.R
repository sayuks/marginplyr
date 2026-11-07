pkgload::load_all(".",quiet=TRUE)
Sys.setenv(TESTTHAT_PARALLEL="false")
dat <- tibble::tibble(g=c(2,1,NA_real_,NaN),row_id=1:4)
for(backend in c("local","dtplyr")) {
  input <- if(backend=="local") dat else dtplyr::lazy_dt(dat)
  collect <- function(x) if(inherits(x,"dtplyr_step")) dplyr::collect(x) else x
  native <- collect(dplyr::arrange(input,is.na(g),g))
  for(verb in c("nest_with_margins","nest_by_with_margins")) for(sort in c("first","last")) {
    z <- collect(getExportedValue("marginplyr",verb)(input,.grouping=rollup(g),.margin_label=NULL,.id="set",.sort=sort))
    detail <- z[z$set==1L,]
    ids <- unlist(lapply(detail$data,`[[`,"row_id"),use.names=FALSE)
    stopifnot(identical(ids,native$row_id))
    cat(backend,verb,sort,":",paste(ids,collapse=","),"native-order match\n")
  }
}
