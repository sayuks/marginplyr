pkgload::load_all(quiet = TRUE)
input <- data.frame(x = 1:3, y = 5:7)
for (backend in c("local", "dtplyr", "arrow", "sql")) {
 data <- switch(backend,local=input,dtplyr=dtplyr::lazy_dt(input),arrow=arrow::arrow_table(input),sql=dbplyr::lazy_frame(x=1:3,y=5:7))
 for (margin in c(FALSE,TRUE)) {
  cat(backend, "margin", margin, "\n")
  out <- tryCatch(if(margin) marginplyr::summarize_with_margins(data,dplyr::across(c(x,y),sum,.names="{c('first','second')}")) else dplyr::summarise(data,dplyr::across(c(x,y),sum,.names="{c('first','second')}")),error=function(cnd) c(class(cnd)[[1]],conditionMessage(cnd)))
  if (inherits(out,c("dtplyr_step","arrow_dplyr_query"))) out <- dplyr::collect(out)
  if(inherits(out,"tbl_sql")) print(colnames(out)) else print(out)
 }
}
