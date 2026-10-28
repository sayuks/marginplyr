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
special<-c('g','.N','.I','.SD','.GRP','.NGRP','.BY','.EACHI','T','F','..','...','..x','.data','.env','.rows','.by','.groups','list','data','set','j','i','by','keyby','get','eval','..marginplyr_nest_1','..marginplyr_cell_1','a b','a`b','a\nb','あ')
for (nm in special) {
  inp<-tibble(g=c('a','b','a',NA),h=c('x','y','y','x'),row=1:4); names(inp)[1]<-nm
  for (role in c('fixed','dim','payload')) for (keep in c(FALSE,TRUE)) for (verbname in c('nest_with_margins','nest_by_with_margins')) {
    if (role=='fixed') {by<-nm;grp<-rollup(h)} else if(role=='dim') {by<-character();grp<-rollup(all_of(nm))} else {by<-'h';grp<-NULL}
    check(paste(nm,role,keep,verbname),inp,by,grp,keep=keep,verb=get(verbname))
  }
}
for (key in setdiff(special,c('g','set'))) for(id in c('set','.N','.I','.SD','.GRP','.rows','.data','T','..x')) {
  if (key==id) next
  check(paste('key/id',key,id),tibble(g=c('a','b'),row=1:2),character(),rollup(g),key=key,id=id)
}
cat('TESTED',tested,'DIFFS',length(errors),'\n')
saveRDS(errors,'/tmp/marginplyr-4306-dt-hunt-diffs.rds')
