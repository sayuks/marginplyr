Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all('.', quiet=TRUE)
fixtures <- list(
 duration = lubridate::dseconds(c(1,1,2,NA)),
 period = lubridate::seconds(c(1,1,2,NA)),
 interval = lubridate::interval(as.Date('2020-01-01')+c(0,0,1,NA), as.Date('2020-01-03')+c(0,0,1,NA)),
 timeDate = timeDate::timeDate(c('2020-01-01','2020-01-01','2020-01-02',NA)),
 yearmon = zoo::as.yearmon(c(2020,2020,2020.1,NA)),
 yearqtr = zoo::as.yearqtr(c(2020,2020,2020.3,NA)),
 survival = survival::Surv(c(1,1,2,NA), c(1,1,0,1)),
 dist = distributional::dist_normal(c(1,1,2,NA),1),
 dist_sample = distributional::dist_sample(list(1:3,1:3,2:4,numeric())),
 ts = ts(c(1,1,2,NA)),
 addmargins = I(array(c(1,1,2,NA), dim=c(4,1,1))),
 integer64 = bit64::as.integer64(c(1,1,2,NA)),
 factor_int = factor(c('a','a','b',NA), levels=c('b','a','c'))
)
for (nm in names(fixtures)) {
 cat('\nFIXTURE ',nm,' class ',paste(class(fixtures[[nm]]),collapse=','),'\n',sep='')
 x <- tryCatch(tibble::tibble(g=fixtures[[nm]],row=1:4),error=function(e)e)
 if (inherits(x,'error')) {cat('tibble ',conditionMessage(x),'\n');next}
 native <- tryCatch(dplyr::summarise(x, rows=list(row), .by=g),error=function(e)e)
 if (inherits(native,'error')) {cat('native ',conditionMessage(native),'\n');next}
 cat('native groups ',length(native$rows),'\n')
 for (label in list('Total',NULL)) for (v in list(nest_with_margins,nest_by_with_margins)) {
  y <- tryCatch(v(x,.grouping=rollup(g),.margin_label=label,.keep=TRUE,.id='set'),error=function(e)e)
  if (inherits(y,'error')) {cat('margin label=',deparse(label),' error ',class(y)[1],': ',conditionMessage(y),'\n',sep='');next}
  cat('margin label=',deparse(label),' groups=',sum(y$set==1),' rows=',paste(vapply(y$data,function(z)paste(z$row,collapse=':'),character(1)),collapse='|'),' class=',paste(class(y$g),collapse=','),'\n',sep='')
 }
}
print(sessionInfo())
