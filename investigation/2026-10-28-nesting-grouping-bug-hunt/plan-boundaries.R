# Run from the repository root; exploratory evidence for 2026-10-28.
pkgload::load_all('.',quiet=TRUE)
for(n in c(0L,1L,2L,30L,31L,32L,64L,128L)) {
 vars<-if(n)paste0('v',seq_len(n))else character(); d<-tibble::as_tibble(stats::setNames(rep(list('x'),n),vars),.name_repair='minimal')
 spec<-if(n)rollup(tidyselect::all_of(vars))else grouping_set()
 result<-inspect_grouping(d,.grouping=spec,.format='list')
 stopifnot(nrow(result)==n+1L)
 if(n<=31L)stopifnot(identical(result$grouping_id,as.integer(2^(0:n)-1)))else stopifnot(all(is.na(result$grouping_id)))
 stopifnot(length(result$grouping_bits[[n+1L]])==n)
 if(n<=31L) {
  out<-summarize_with_margins(d, g=grouping_id(),.grouping=spec,.id='set',.margin_label=NULL,.sort='none')
  stopifnot(identical(out$g,result$grouping_id[out$set]))
 } else {
  e<-tryCatch(summarize_with_margins(d,g=grouping_id(),.grouping=spec),error=identity)
  stopifnot(inherits(e,'marginplyr_error'),grepl('at most 31',conditionMessage(e)))
 }
}
cat('PASS: dimension boundaries 0/1/2/30/31/32/64/128, helper/inspection identity and expected >31 helper refusal\n')
