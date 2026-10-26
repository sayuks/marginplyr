# Dated investigation evidence, investigated 2026-10-26 at 6b91254016dbcce60cc2cc69071f01f69eb8084e.
# Run from the repository root, or pass its path as the first argument.
args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args)) args[[1L]] else "."
pkgload::load_all(repo, quiet = TRUE)
set.seed(61)
constructors <- list(grouping_set(g),rollup(g,h),cube(g,h),grouping_sets(grouping_set(h),grouping_set(),grouping_set(g,h)),grouping_spec(rollup(g),grouping_set(h)))
fixtures <- list(chr=function(n)sample(c('x','y',NA),n,TRUE), factor=function(n)factor(sample(c('x','y',NA),n,TRUE),levels=c('z','y','x')), factor_na=function(n)structure(sample(c(1L,2L,NA_integer_),n,TRUE),levels=c('x',NA_character_),class='factor'), double=function(n)sample(c(0,1,NaN,NA_real_),n,TRUE),list=function(n)lapply(sample(1:4,n,TRUE),function(i)list(NULL,1L,c(1L,2L),integer())[[i]]),matrix=function(n)matrix(sample(c(1L,2L,NA_integer_),n*2,TRUE),nrow=n,ncol=2),packed=function(n)tibble::tibble(a=sample(c(1L,2L,NA_integer_),n,TRUE),b=sample(c('x','y',NA),n,TRUE)))
checks<-0L
failures<-0L
for (iteration in 1:80) {
 n<-sample(0:8,1)
 for(typ in names(fixtures)) {
  x<-tibble::tibble(g=fixtures[[typ]](n),h=sample(c('a','b',NA_character_),n,TRUE),f=sample(c('m','n',NA_character_),n,TRUE),rowid=seq_len(n),list_payload=lapply(seq_len(n),function(i)seq_len(i)))
  spec<-constructors[[sample(seq_along(constructors),1)]]
  fixed<-sample(c(FALSE,TRUE),1);keep<-sample(c(FALSE,TRUE),1)
  args<-list(.data=x,.grouping=spec,.margin_label=NULL,.id='setid',.keep=keep,.sort=sample(c('none','first','last'),1))
  plan<-if(fixed)inspect_grouping(x,.by=f,.grouping=spec,.format="list") else inspect_grouping(x,.grouping=spec,.format="list")
  z<-tryCatch(if(fixed)nest_with_margins(x,.by=f,.grouping=spec,.margin_label=NULL,.id='setid',.keep=keep) else do.call(nest_with_margins,args),error=identity)
  if(inherits(z,'error')){failures<-failures+1L;cat('UNEXPECTED ERROR',iteration,typ,conditionMessage(z),'\n');next}
  for(seti in seq_len(nrow(plan))) {
   groupcols<-plan$included[[seti]]
   if(fixed)groupcols<-unique(c('f',groupcols))
   expected<-if(!length(groupcols))list(seq_len(n))else vctrs::vec_group_loc(x[groupcols])$loc
   if(n==0)expected<-list()
   actual<-z$data[z$setid==seti]
   expected<-sort(vapply(expected,function(rows)paste(rows,collapse=','),character(1)))
   observed<-sort(vapply(actual,function(cell)paste(cell$rowid,collapse=','),character(1)))
   if(!identical(expected,observed)){failures<-failures+1L;cat('FAIL GROUP',iteration,typ,seti,'n=',n,'fixed=',fixed,'keep=',keep,'\n')}
   for(cell in actual) {
    cols<-if(keep)unique(c(if(fixed)'f',names(plan$grouping_bits[[1]]),names(x)))else setdiff(names(x),unique(c(if(fixed)'f',names(plan$grouping_bits[[1]]))))
    if(!identical(cell$list_payload,x$list_payload[cell$rowid])){failures<-failures+1L;cat('FAIL PAYLOAD',iteration,typ,seti,'\n')}
   }
   checks<-checks+1L
  }
 }
}
cat('CHECKS',checks,'\n')
stopifnot(failures == 0L)
