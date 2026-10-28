# Run from the repository root; exploratory evidence for 2026-10-28.
source('investigation/2026-10-28-nesting-grouping-bug-hunt/dtplyr-common.R', local=TRUE, echo=FALSE, max.deparse.length=0)
errors<-list();tested<-0L
f<-factor(c('a',NA,'a','b'),levels=c('z','b','a'),ordered=TRUE)
fn<-addNA(f);fn[3]<-NA
vectors<-list(
 logical=c(TRUE,NA,FALSE,TRUE),integer=c(1L,NA,2L,1L),double=c(1,NA_real_,2,1),near=c(1,1+1e-15,1e20,1e20+1e5),
 number=c(NaN,NA_real_,Inf,-Inf),character=c('a',NA,'b','a'),factor=f,factor_na=fn,
 date=as.Date(c('2000-01-01',NA,'2020-01-01','2000-01-01')),
 posix=as.POSIXct(c('2000-01-01 00:00:00',NA,'2020-01-01 00:00:00','2000-01-01 00:00:00'),tz='Asia/Tokyo'),
 difftime=as.difftime(c(1,NA,2,1),units='days'),complex=c(1+1i,NA_complex_,2+2i,1+1i),raw=as.raw(1:4),
 list=list(1L,NULL,'x',1L),list_of=vctrs::list_of(1L,integer(),2L,1L),
 matrix=matrix(1:8,nrow=4),AsIs_matrix=I(matrix(1:8,nrow=4)))
for(type in names(vectors)) for(rows in c(0L,1L,4L)) {
  inp<-tibble(g=vctrs::vec_slice(vectors[[type]],seq_len(rows)),h=rep(c('x','y'),length.out=rows),row=seq_len(rows))
  root<-tryCatch(collect(dtplyr::lazy_dt(inp)),error=identity)
  # A backend loses some unsupported representations before marginplyr sees them.
  if(inherits(root,'error') || !identical(as.list(inp),as.list(root))) {cat('ROOT DIFF',type,rows,'\n'); next}
  for (role in c('fixed','dim','payload')) for(keep in c(FALSE,TRUE)) for(sort in c('none','first','last')) for(margin in list('Total',NULL,NA_character_)) {
    if(role=='fixed') {by<-'g';grp<-rollup(h)} else if(role=='dim') {by<-character();grp<-rollup(g)} else {by<-'h';grp<-NULL}
    check(paste(type,rows,role,keep,sort,deparse(margin)),inp,by,grp,keep=keep,sort=sort,margin=margin)
  }
}
cat('TYPE TESTED',tested,'DIFFS',length(errors),'\n');saveRDS(errors,'/tmp/marginplyr-4306-dt-types-diffs.rds')
