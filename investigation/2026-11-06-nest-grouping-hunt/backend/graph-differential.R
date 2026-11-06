Sys.setenv(TESTTHAT_PARALLEL='false')
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", unset = "."), quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
normalize <- function(x) {
 if(inherits(x,'dtplyr_step')) x <- collect(x)
 x <- tibble::as_tibble(ungroup(x)); x$data <- lapply(x$data, function(z) {z <- tibble::as_tibble(z); if(nrow(z)&&ncol(z)) z <- arrange(z,across(everything())); z}); x
}
records <- list(); k <- 0L
for(n in c(0L,1L,4L)) {
 a <- data.table::data.table(g=c('b','a',NA,'a')[seq_len(n)],h=c(2L,1L,NA,1L)[seq_len(n)],v=seq_len(n))
 b <- data.table::data.table(g=c('b','c',NA,'a')[seq_len(n)],h=c(2L,4L,NA,1L)[seq_len(n)],w=rev(seq_len(n)))
 x <- dtplyr::lazy_dt(a); y <- dtplyr::lazy_dt(b)
 gs <- list(root=x,filter=filter(x,is.na(h)|h==1L),project=select(x,g,v),rename=rename(x,j=g),reorder=select(x,v,h,g),arrange=arrange(x,desc(g)),head=head(x,2L),slice=slice(x,1L),distinct=distinct(x),copy=mutate(x,j=g),arithmetic=mutate(x,j=h/2),constant=mutate(x,j='z'),remove=mutate(x,h=NULL),left=left_join(x,y,by=c('g','h')),right=right_join(x,y,by=c('g','h')),inner=inner_join(x,y,by=c('g','h')),full=full_join(x,y,by=c('g','h')),semi=semi_join(x,y,by=c('g','h')),anti=anti_join(x,y,by=c('g','h')),union=union(x,x),union_all=union_all(x,x),intersect=intersect(x,x),setdiff=setdiff(x,x))
 for(gn in names(gs)) { z <- gs[[gn]]; local <- collect(z)
  for(selector in c('character','numeric','logical')) for(role in c('grouping','by')) for(consumer in c('inspect','nest','nest_by')) {
   pred <- get(paste0('is.',selector),baseenv()); spec <- grouping_set(where(pred))
   make <- function(input) { if(consumer=='inspect') {if(role=='grouping') inspect_grouping(input,.grouping=spec,.format='list') else inspect_grouping(input,.by=where(pred),.format='list')} else {f <- get(paste0(consumer,'_with_margins')); normalize(if(role=='grouping') f(input,.grouping=spec,.id='set',.keep=TRUE,.sort='last',.margin_label=NULL) else f(input,.by=where(pred),.id='set',.keep=TRUE,.sort='last',.margin_label=NULL))} }
   expected <- tryCatch(make(local),error=identity); actual <- tryCatch(make(z),error=identity)
   status <- if(inherits(actual,'error')) {if(inherits(expected,'error')) 'both_error' else if(inherits(actual,'marginplyr_error')) 'safe_refusal' else 'backend_error'} else if(inherits(expected,'error')) 'local_error_only' else if(isTRUE(all.equal(actual,expected,check.attributes=TRUE))) 'match' else 'mismatch'
   k<-k+1L; records[[k]]<-data.frame(n,graph=gn,selector,role,consumer,status,detail=if(inherits(actual,'error')) conditionMessage(actual) else if(status=='mismatch') paste(all.equal(actual,expected),collapse=';') else '')
  }
 }
}
out<-do.call(rbind,records);write.csv(out,Sys.getenv("MARGINPLYR_HUNT_OUTPUT", unset = tempfile(fileext = ".csv")),row.names=FALSE); print(table(out$status));print(subset(out,status %in% c('mismatch','backend_error','local_error_only')))
