# Run from the repository root; exploratory evidence for 2026-10-28.
options(warn=1)
pkgload::load_all('.', quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
normalize <- function(x,key) {
  if (inherits(x,'dtplyr_step')) x <- collect(x)
  x <- ungroup(x)
  out <- as.list(x)
  out[[key]] <- lapply(out[[key]], function(z) { z<-as_tibble(z); if ('row' %in% names(z)) z<-z[order(z$row),,drop=FALSE]; as.list(z) })
  out
}
errors <- list(); tested<-0L
report <- function(label, a,b) { errors[[label]] <<- list(actual=a, expected=b); cat('DIFF',label,'\n'); if (inherits(a,'error')) cat(conditionMessage(a),'\n'); if (inherits(b,'error')) cat('LOCAL',conditionMessage(b),'\n') }
check <- function(label,input,by,grp,key='data',keep=TRUE,sort='last',id='set',margin='Total',verb=nest_with_margins) {
  run<-function(x) normalize(verb(x,.by=all_of(by),.grouping=grp,.key=key,.keep=keep,.sort=sort,.id=id,.margin_label=margin),key)
  local<-tryCatch(run(input),error=identity)
  lazy<-tryCatch(run(dtplyr::lazy_dt(input)),error=identity)
  tested<<-tested+1L
  if (inherits(local,'error')) return(invisible(NULL))
  if (!identical(local,lazy)) report(label,lazy,local)
}
