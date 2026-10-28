pkgload::load_all('.',quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
for(type in c('complex','raw')) {
 x <- tibble(g=c('a','a'),v=if(type=='complex') c(1+1i,2+2i) else as.raw(1:2))
 z <- dtplyr::lazy_dt(x)
 cat('\nTYPE',type,'upstream source preserves:',identical(collect(z),x),'\n')
 cat('upstream nest:', nrow(collect(tidyr::nest(group_by(z,g)))),'\n')
 for (verb in list(nest_with_margins,nest_by_with_margins)) {
   single <- verb(z,.by=g); if(inherits(single,'dtplyr_step'))single<-collect(single)
   cat('single cell preserves:',identical(single$data[[1]][['v']], x$v),'\n')
   local <- verb(x,.grouping=rollup(g)); cat('local margins:', nrow(local),'\n')
   err <- tryCatch({r<-verb(z,.grouping=rollup(g));if(inherits(r,'dtplyr_step'))r<-collect(r);r},error=identity)
   cat('margins error:',paste(class(err),collapse='/'),conditionMessage(err),'\n')
 }
 cat('rbindlist type:',typeof(data.table::rbindlist(list(as.data.frame(x),as.data.frame(x)))$v),'\n')
}
