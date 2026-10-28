suppressPackageStartupMessages(library(dplyr));pkgload::load_all(if (length(commandArgs(TRUE))) commandArgs(TRUE)[1L] else getwd(),quiet=TRUE)
dat<-data.frame(f=character(),g=character(),x=numeric());sources<-list(true=quote(TRUE),logical=quote(any(x)),date=quote(as.Date('2026-01-01')),character=quote('s'),double=quote(1.0),zero=quote(double()),multi=quote(c(1,2)),frame=quote(tibble::tibble(v=1)))
for(b in c('local','dt')) for(fixed in c(FALSE,TRUE)) for(src in names(sources)) {
 d<-if(b=='local')dat else dtplyr::lazy_dt(dat)
 dots<-rlang::quos(value=!!sources[[src]],t=share_of_total(value),p=share_of_parent(value))
 cat('CASE',b,fixed,src,' ')
 q<-tryCatch(if(fixed) summarize_with_margins(d,!!!dots,.by=f,.grouping=rollup(g)) else summarize_with_margins(d,!!!dots,.grouping=rollup(g)),error=identity)
 r<-if(inherits(q,'error'))q else tryCatch(if(b=='local') q else collect(q),error=identity)
 if(inherits(r,'error'))cat(class(r)[1],gsub('\n',' ',conditionMessage(r)),'\n') else {cat(nrow(r),paste(vapply(r,typeof,character(1)),collapse=','),'\n')}
}
