Sys.setenv(TESTTHAT_PARALLEL='false')
pkgload::load_all('.', quiet=TRUE)
x <- list(duration=lubridate::dseconds(numeric()),period=lubridate::seconds(numeric()),interval=lubridate::interval(as.Date(character()),as.Date(character())),yearmon=zoo::as.yearmon(numeric()),yearqtr=zoo::as.yearqtr(numeric()),dist=distributional::dist_normal(numeric(),numeric()),integer64=bit64::as.integer64(numeric()),difftime=as.difftime(numeric(),units='secs'),hms=hms::hms(numeric()))
for (nm in names(x)) {
 cat(nm,': as.character()=',typeof(as.character(x[[nm]])),'\n',sep='')
 d<-tibble::tibble(g=x[[nm]],row=integer())
 for(v in list(nest_with_margins,nest_by_with_margins)) {
 y<-tryCatch(v(d,.grouping=rollup(g)),error=function(e)e)
 if(inherits(y,'error'))cat('ERROR ',class(y)[1],conditionMessage(y),'\n') else cat('OK ',nrow(y),typeof(y$g),'\n')
 }
}
