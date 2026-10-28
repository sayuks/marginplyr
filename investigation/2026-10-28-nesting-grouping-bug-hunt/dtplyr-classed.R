# Run from the repository root; exploratory evidence for 2026-10-28.
source('investigation/2026-10-28-nesting-grouping-bug-hunt/dtplyr-common.R')
vectors<-list(integer64=bit64::as.integer64(c('9007199254740992','9007199254740993',NA,'9007199254740992')),hms=hms::hms(c(1,2,NA,1)),units=units::set_units(c(1,2,NA,1),'m'),DateI=I(as.Date(c('2020-01-01','2020-01-02',NA,'2020-01-01'))))
for(type in names(vectors))for(rows in c(0L,1L,4L))for(role in c('fixed','dim','payload'))for(keep in c(FALSE,TRUE))for(sort in c('first','last'))for(margin in list('Total',NULL,NA_character_))for(verb in list(nest_with_margins,nest_by_with_margins)){
 inp<-tibble(g=vctrs::vec_slice(vectors[[type]],seq_len(rows)),h=rep(c('x','y'),length.out=rows),row=seq_len(rows))
 if(role=='fixed') {by<-'g';grp<-rollup(h)} else if(role=='dim') {by<-character();grp<-rollup(g)} else {by<-'h';grp<-NULL}
 check(paste(type,rows,role,keep,sort,deparse(margin),identical(verb,nest_with_margins)),inp,by,grp,keep=keep,sort=sort,margin=margin,verb=verb)
}
cat('TESTED',tested,'DIFFS',length(errors),'\n');saveRDS(errors,'/tmp/marginplyr-4306-dt-classed-diffs.rds')
