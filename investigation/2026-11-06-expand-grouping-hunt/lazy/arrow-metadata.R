pkgload::load_all('.', quiet=TRUE)
library(dplyr,warn.conflicts=FALSE)
values <- list(date=as.Date('2026-01-01')+0:1, time=as.POSIXct('2026-01-01',tz='Asia/Tokyo')+0:1, units=structure(1:2,units='kg'), factor=factor(c('a','b')), ordered=ordered(c('a','b')), difftime=as.difftime(1:2,units='mins'), hms=hms::hms(1:2))
for(nm in names(values)) for(label in list(NULL,'Total')) {
 x <- tibble(g=values[[nm]],rid=1:2)
 a <- arrow::Table$create(x)
 expected <- tryCatch(expand_with_margins(x,.grouping=rollup(g),.margin_label=label,.id='sid'),error=identity)
 got <- tryCatch(collect(expand_with_margins(a,.grouping=rollup(g),.margin_label=label,.id='sid')),error=identity)
 cat(nm,if(is.null(label))'NULL' else label,'expected',paste(class(expected),collapse='/'),'actual',paste(class(got),collapse='/'),'\n')
 if(inherits(got,'error')) cat(conditionMessage(got),'\n') else {dput(got$g); if(!inherits(expected,'error')) dput(expected$g)}
}
