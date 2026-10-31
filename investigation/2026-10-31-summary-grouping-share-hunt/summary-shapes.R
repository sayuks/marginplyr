pkgload::load_all('.',quiet=TRUE)
options(dplyr.summarise.inform=FALSE)
library(dplyr)
values <- list(
 logical=TRUE,missing=NA,unspecified=vctrs::unspecified(1),posixct=as.POSIXct('2026-10-31',tz='UTC'),posixlt=as.POSIXlt('2026-10-31',tz='UTC'),diff=as.difftime(3,units='days'),factor=factor('a'),ordered=ordered('a'),i64=bit64::as.integer64(1),asis=I(1),asislist=I(list(1)),listof=vctrs::list_of(1),dt=data.table::data.table(z=1),tibble=tibble(z=1),frame=data.frame(z=1),emptyframe=data.frame(),sfc=sf::st_sfc(sf::st_point(c(1,2))),sf=sf::st_sf(z=1,geometry=sf::st_sfc(sf::st_point(c(1,2)))),rcrd=vctrs::new_rcrd(list(a=1,b=2),class='foo'))
d <- tibble(g=c('a','a','b'),x=1:3)
cap <- function(x) tryCatch(suppressWarnings(x),error=identity)
count <- 0L;diffs <- 0L
for(a in names(values)) for(b in names(values)) for(named in c(FALSE,TRUE)) {
 f <- local({aa <- values[[a]];bb <- values[[b]];function(x) if(length(x)>1L) aa else bb})
 dots <- list(rlang::quo(f(x)));if(named)names(dots)<-'out'
 x <- cap(summarise(d,!!!dots,.by=g));y <- cap(summarize_with_margins(d,!!!dots,.by=g))
 count <- count+1L
 eq <- if(inherits(x,'error')&&inherits(y,'error'))TRUE else isTRUE(all.equal(x,y))
 if(!eq) {diffs <- diffs+1L;cat('DIFF',a,b,named,'\n');print(x);print(y)}
}
cat('checks=',count,'differences=',diffs,'\n')
