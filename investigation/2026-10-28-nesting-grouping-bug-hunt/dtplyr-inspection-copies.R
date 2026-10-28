# Run from the repository root; exploratory evidence for 2026-10-28.
pkgload::load_all('.',quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
x<-tibble(i=1:3,d=c(1,2,3),ch=letters[1:3],f=factor(letters[1:3]),l=c(TRUE,FALSE,NA),date=as.Date('2026-01-01')+0:2)
make_plan <- function(x,p) rlang::eval_tidy(rlang::expr(inspect_grouping(!!x,.grouping=grouping_set(where(!!p)),.format='list')))
ps<-list(is.integer,is.double,is.factor,is.character,is.logical,function(x)inherits(x,'Date'))
n<-0L; bad<-0L; refused<-0L
for(a in names(x)) for(b in names(x)) {
 q<-mutate(dtplyr::lazy_dt(x), !!a := .data[[b]])
 real<-collect(q)
 for(p in ps) {
  n<-n+1L
  r<-tryCatch(make_plan(q,p),error=identity)
  e<-make_plan(real,p)
  if(inherits(r,'error')) {refused<-refused+1L; next}
  if(!identical(r,e)) {bad<-bad+1L;cat('MISMATCH',a,'<-',b,'\n'); print(r$included);print(e$included)}
 }
}
cat('CHECKED',n,'MISMATCH',bad,'REFUSED',refused,'\n')
