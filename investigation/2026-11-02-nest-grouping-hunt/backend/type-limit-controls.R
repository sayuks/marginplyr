# Dated controls distinguish native backend limits from fresh package defects.
Sys.setenv(TESTTHAT_PARALLEL="false")
pkgload::load_all(quiet=TRUE)
observe <- function(x) tryCatch(force(x),error=identity)
cat("SHA:",system2("git",c("rev-parse","HEAD"),stdout=TRUE),"\n")
for(type in c("raw","complex")) {
  payload <- if(type=="raw") as.raw(c(1,2)) else c(1+1i,2+2i)
  input <- tibble::tibble(g=c("a","b"),p=payload,row_id=1:2)
  lazy <- dtplyr::lazy_dt(input)
  for(verb in c("nest","nest_by")) {
    fn <- if(verb=="nest") nest_with_margins else nest_by_with_margins
    one <- dplyr::collect(fn(lazy,.grouping=grouping_set(g)))
    stopifnot(identical(unlist(lapply(one$data,`[[`,"p"),use.names=FALSE),payload))
    multi <- observe(dplyr::collect(fn(lazy,.grouping=rollup(g))))
    stopifnot(inherits(multi,"error"))
    cat(type,verb,"one-set successful; multi-set:",conditionMessage(multi),"\n")
  }
  native <- dplyr::collect(tidyr::nest(dplyr::group_by(lazy,g),data=c(p,row_id)))
  stopifnot(identical(unlist(lapply(native$data,`[[`,"p"),use.names=FALSE),payload))
  union <- observe(dplyr::collect(dplyr::union_all(lazy,lazy)))
  stopifnot(inherits(union,"error")); cat(type,"native union:",conditionMessage(union),"\n")
}
input <- tibble::tibble(g=c("a","b"),p=list(1L,2L),row_id=1:2)
lazy <- dtplyr::lazy_dt(input)
native <- observe(dplyr::collect(dplyr::summarise(dplyr::group_by(lazy,p),n=dplyr::n())))
stopifnot(inherits(native,"error")); cat("list native grouping:",conditionMessage(native),"\n")
rcrd <- vctrs::new_rcrd(list(x=1:4,y=5:8),class="audit_rcrd")
format.audit_rcrd <- function(x,...) rep("record",vctrs::vec_size(x))
input <- tibble::tibble(g=letters[1:4],p=rcrd,row_id=1:4)
table <- data.table::as.data.table(input)
native <- observe(table[1L])
stopifnot(inherits(native,"error")); cat("rcrd native row selection:",conditionMessage(native),"\n")
