pkgload::load_all(quiet = TRUE)
library(dplyr)
fixtures <- list(
 logical=c(TRUE,FALSE,NA,TRUE), integer=c(1L,2L,NA,1L), double=c(1,2,NA,NaN), complex=c(1+2i,2+3i,NA_complex_,NaN+0i),
 char=c('a','b',NA,'a'), factor=factor(c('a','b',NA,'a'), levels=c('b','a','z')), ordered=ordered(c('a','b',NA,'a'), levels=c('b','a','z')),
 factor_na=factor(c('a','b',NA,'a'), exclude=NULL), date=as.Date(c('2000-01-01','2001-02-01',NA,'2000-01-01')),
 posix=as.POSIXct(c('2000-01-01','2001-02-01',NA,'2000-01-01'),tz='UTC'), difftime=as.difftime(c(1,2,NA,1),units='days'),
 integer64=bit64::as.integer64(c(1,2,NA,1)), list=list(1L,2L,NULL,1L), list_mixed=list(1L,'a',NULL,1L), matrix=I(matrix(c(1,2,NA,1,4,5,NA,4),ncol=2)),
 dataframe=tibble(a=c(1,2,NA,1),b=c('a','b',NA,'a')))
fail <- list()
for (nm in names(fixtures)) for (dim_role in c(TRUE,FALSE)) for (keep in c(TRUE,FALSE)) for (verbname in c('nest_with_margins','nest_by_with_margins')) {
 input <- tibble(g=fixtures[[nm]], h=c('e','e','w','w'), rid=1:4)
 f <- get(verbname)
 msg <- paste(nm,dim_role,keep,verbname)
 tryCatch({
  result <- if(dim_role) f(input,.grouping=rollup(g),.margin_label=NULL,.id='set',.keep=keep) else f(input,.by=g,.grouping=rollup(h),.margin_label=NULL,.id='set',.keep=keep)
  for (id in 1:2) {
   keys <- if(dim_role) if(id==1L) 'g' else character() else if(id==1L) c('g','h') else 'g'
   ref <- summarise(input,members=list(rid),.by=all_of(keys))$members
   actual <- lapply(result$data[result$set==id],function(x) x$rid)
   canon <- function(x) sort(vapply(x,function(z)paste(sort(z),collapse=','),character(1)))
   if(!identical(canon(ref),canon(actual))) stop('members expected ',paste(canon(ref),collapse=';'),' actual ',paste(canon(actual),collapse=';'))
  }
  if(keep) {
    for(cell in result$data) if(!identical(cell$g,vctrs::vec_slice(input$g,cell$rid))) stop('keep g differs')
  }
 },error=function(e){fail[[msg]] <<- conditionMessage(e)})
}
print(fail)
stopifnot(length(fail) == 0L)
cat('PASS: 128 checked cases\n')
