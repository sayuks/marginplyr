source('investigation/2026-10-29-summary-spec-share-hunt/summary-one-set.R')
set.seed(20261029)
multiset<-function(d) {d<-tibble::as_tibble(d);d[vctrs::vec_order(d),,drop=FALSE]}
compare_rollup<-function(d,dots,i){
 exp<-tryCatch(suppressWarnings({
   a<-rlang::eval_tidy(rlang::expr(dplyr::summarise(d,!!!dots,.by=g)))
   b<-rlang::eval_tidy(rlang::expr(dplyr::summarise(d[setdiff(names(d),'g')],!!!dots)))
   b<-dplyr::mutate(b,g=vctrs::vec_init(d$g,nrow(b)),.before=1L)
   dplyr::bind_rows(a,b)
 }),error=identity)
 got<-tryCatch(suppressWarnings(rlang::eval_tidy(rlang::expr(summarize_with_margins(d,!!!dots,.grouping=rollup(g),.margin_label=NULL)))),error=identity)
 if(inherits(exp,'error')&&inherits(got,'error'))return(TRUE)
 if(!inherits(exp,'error')) exp<-multiset(exp)
 if(!inherits(got,'error')) got<-multiset(got)
 if(!isTRUE(all.equal(exp,got,check.attributes=TRUE))){
 cat('ROLLUP FAIL',i,'\n');str(d$g);print(dots);print(exp);print(got)
 saveRDS(list(d=d,dots=dots,expected=exp,actual=got),sprintf('/tmp/marginplyr-rollup-fail-%s.rds',i)); return(FALSE)
 }
 TRUE
}
keys<-list(
 c('b','a','b'), c('b',NA,'a'),
 factor(c('b',NA,'a'),levels=c('a','b','unused')),
 ordered(c('b','b','a'),levels=c('b','a','unused')),
 c(TRUE,NA,FALSE),c(NA_real_,NaN,Inf),as.Date(c('2020-01-01',NA,'2020-01-02')),
 as.POSIXct(c('2020-01-01 00:00:00',NA,'2020-01-02 01:00:00'),tz='Asia/Tokyo'),
 list(NULL,NA,1),list(list(a=1),list(a=2),NULL),
 matrix(c(1,1,2,NA,1,2),ncol=2),
 tibble::tibble(a=c(NA,1,2),b=c(NA,NA,3))
)
allframes<-list()
for(g in keys){d<-tibble::tibble(g=g,x=1:3,y=c(4,NA,5),z=c('q','r','s'));allframes<-append(allframes,list(d,d[0,]))}
fails<-0L;n<-0L
for(d in allframes)for(dots in exprs){n<-n+1L;if(!compare_rollup(d,dots,n))fails<-fails+1L}
cat('ROLLUP SUMMARY',n,'cases;',fails,'mismatches\n')
