pkgload::load_all(quiet=TRUE)
set.seed(281329)
fixtures <- list(chr=c('b',NA,'a'),int=c(2L,NA_integer_,1L),dbl=c(2,NaN,NA_real_),log=c(TRUE,NA,FALSE),factor=factor(c('b',NA,'a'),levels=c('a','unused','b')),ordered=ordered(c('b',NA,'a'),levels=c('a','unused','b')),date=as.Date(c('2026-01-01',NA,'2025-01-01')),posixct=as.POSIXct(c('2026-01-01',NA,'2025-01-01'),tz='Asia/Tokyo'),list=list(2L,NULL,1L),packed=tibble::tibble(a=c(2L,NA,1L),b=c('x',NA,'y')),matrix=matrix(c(2L,NA,1L,4L,NA,3L),ncol=2))
failed<-list(); checked<-0L
for(type in names(fixtures)) for(sort1 in c('none','first','last')) for(sort2 in c('none','first','last')) for(n in c(0L,1L,3L)) {
 x<-tibble::tibble(g=vctrs::vec_slice(fixtures[[type]],seq_len(n)),f=c('c',NA,'d')[seq_len(n)],rowid=seq_len(n))
 a<-expand_with_margins(x,.grouping=rollup(g),.margin_label=NULL,.id='set1',.sort=sort1)
 b<-tryCatch(expand_with_margins(a,.by=f,.grouping=rollup(g),.margin_label=NULL,.id='set2',.sort=sort2),error=identity)
 checked<-checked+1L
 if(inherits(b,'error')) {failed[[paste(type,sort1,sort2,n)]]<-conditionMessage(b);next}
 for(i in 1:2)for(j in 1:2) {
  rows<-b$set1==i&b$set2==j
  z<-b[rows,,drop=FALSE];z<-z[order(z$rowid),,drop=FALSE]
  want<-x$g;if(i==2||j==2)want<-vctrs::vec_init(want,n)
  if(!identical(z$g,want)||!identical(z$f,x$f)||!identical(z$rowid,x$rowid))failed[[paste(type,sort1,sort2,n,i,j)]]<-'value mismatch'
 }
}
cat('reexpand cells',checked,'signals',length(failed),'\n'); print(failed)
