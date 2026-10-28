# Run from the repository root; exploratory evidence for 2026-10-28.
pkgload::load_all('.',quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
vals<-list(logical=c(TRUE,FALSE),integer=1:2,double=c(1,2),character=c('a','b'),factor=factor(c('a','b'),levels=c('c','b','a')),ordered=ordered(c('a','b'),levels=c('c','b','a')),date=as.Date(c('2020-01-01','2020-01-02')),posix=as.POSIXct(c('2020-01-01','2020-01-02')),difftime=as.difftime(1:2,units='days'),AsIs=I(1:2),list=list(1L,2L))
errors<-list();n<-0L
for(a in names(vals))for(b in names(vals))for(dst in c('x','z')) {
 inp<-tibble(x=vals[[a]],y=vals[[b]],row=1:2)
 lazy<-dtplyr::lazy_dt(inp) |> mutate('{dst}' := .data$y)
 n<-n+1L
 # Inspect this pinned implementation's private metadata seam, not an API contract.
 proxy_fn <- getFromNamespace('dtplyr_selection_proxy', 'marginplyr')
 p<-tryCatch(proxy_fn(lazy),error=identity)
 full<-tryCatch(collect(lazy),error=identity)
 if(inherits(p,'error')||inherits(full,'error'))next
 proto<-lapply(full,vctrs::vec_ptype)
 pp<-lapply(p,vctrs::vec_ptype)
 if(!identical(proto,pp)) {cat('DIFF',a,b,dst,'\n');errors[[paste(a,b,dst)]]<-list(proxy=pp,full=proto)}
}
cat('TESTED',n,'DIFFS',length(errors),'\n');saveRDS(errors,'/tmp/marginplyr-4306-dt-transform-diffs.rds')
