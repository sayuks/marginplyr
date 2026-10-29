pkgload::load_all(quiet=TRUE)
set.seed(2813)
errs<-list();n<-0L
for(rows in c(0L,1L,6L))for(drop in c(TRUE,FALSE))for(sort in c('none','first','last'))for(specname in c('single','rollup','cube','repeated'))for(input in c('data.frame','tibble','data.table')) {
 d<-data.frame(fixed=sample(c('b','a',NA),rows,replace=TRUE),f2=sample(c(2L,1L,NA),rows,replace=TRUE),g=sample(c('b','a',NA),rows,replace=TRUE),h=sample(c(2L,1L,NA),rows,replace=TRUE),rid=seq_len(rows));if(input=='tibble')d<-tibble::as_tibble(d);if(input=='data.table')d<-data.table::as.data.table(d)
 spec<-switch(specname,single=grouping_set(g),rollup=rollup(g,h),cube=cube(g,h),repeated=grouping_sets(grouping_set(g),grouping_set(g),grouping_set()))
 grouped<-dplyr::group_by(d,fixed,f2,.drop=drop)
 a<-tryCatch(expand_with_margins(grouped,.grouping=spec,.duplicates='keep',.margin_label=NULL,.id='sid',.sort=sort),error=identity)
 b<-tryCatch(expand_with_margins(dplyr::ungroup(grouped),.by=c(fixed,f2),.grouping=spec,.duplicates='keep',.margin_label=NULL,.id='sid',.sort=sort),error=identity)
 n<-n+1L
 if(!drop) {if(!inherits(a,'marginplyr_error') || !grepl('.drop = FALSE',conditionMessage(a),fixed=TRUE)) errs[[paste(rows,drop,sort,specname,input)]]<-'missing declared .drop refusal';next}
 if(inherits(a,'error')||inherits(b,'error'))errs[[paste(rows,drop,sort,specname,input)]]<-c(if(inherits(a,'error'))conditionMessage(a),if(inherits(b,'error'))conditionMessage(b))else if(!identical(a,b)||length(dplyr::group_vars(a))!=0L)errs[[paste(rows,drop,sort,specname,input)]]<-all.equal(a,b)
}
cat('grouped equivalence',n,'signals',length(errs),'\n');print(errs)
# Encoding equivalence is byte-distinct but comparison-equal in R.
u <- intToUtf8(233L); l<-iconv(u,from='UTF-8',to='latin1'); stopifnot(u==l,!identical(charToRaw(u),charToRaw(l)))
for(input in c('data.frame','tibble','data.table'))for(sort in c('none','first','last')) {
 x<-data.frame(g=c(2L,NA,1L),rid=1:3);names(x)[1L]<-l;if(input=='tibble')x<-tibble::as_tibble(x);if(input=='data.table')x<-data.table::as.data.table(x)
 spec<-grouping_set(dplyr::all_of(u))
 got<-expand_with_margins(x,.grouping=spec,.margin_label=stats::setNames(list(NULL),u),.id='sid',.sort=sort)
 stopifnot(identical(names(got)[1L],names(x)[1L]))
 rejected<-tryCatch(expand_with_margins(x,.id=u),marginplyr_error=identity)
 stopifnot(inherits(rejected,'marginplyr_error'))
 cat('PASS encoding',input,sort,'\n')
}
# Canonically distinct Unicode names stay distinct; no normalization is implied.
composed<-intToUtf8(233L);decomposed<-paste0('e',intToUtf8(769L))
x<-tibble::tibble(g=1:2,rid=1:2);names(x)[1L]<-composed
z<-expand_with_margins(x,.grouping=rollup(dplyr::all_of(composed)),.margin_label=NULL,.id=decomposed,.sort='last')
stopifnot(identical(names(z),c(composed,decomposed,'rid')),identical(z[[decomposed]],rep(1:2,each=2L)))
cat('PASS canonical distinction\n')
