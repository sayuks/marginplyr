pkgload::load_all(quiet=TRUE)
weird<-c('.data','.env','.rows','.N','.I','.SD','.GRP','.NGRP','.BY','T','F','..marginplyr_nest_1','..marginplyr_column_1')
checks<-0L
for(a in weird) for(b in setdiff(weird,a)) for(sort in c('none','first','last')) for(keep in c(FALSE,TRUE)) {
 x<-tibble::tibble(f=c('a','b','a','b'),g=structure(c(1L,2L,NA,1L),levels=c('..marginplyr_missing_factor_code',NA_character_),class='factor'),h=matrix(c(1,NA,1,NA,2,NA,2,NA),nrow=4),value=1:4)
 names(x)[1:2]<-c(a,b)
 out<-nest_with_margins(x,.by=dplyr::all_of(a),.grouping=cube(dplyr::all_of(b),h),.keep=keep,.sort=sort,.margin_label=NULL,.id='set')
 for(set in 1:4) {
  included<-list(c(a,b,'h'),c(a,b),c(a,'h'),a)[[set]]
  expected<-vctrs::vec_group_loc(x[included])$loc
  actual<-out$data[out$set==set]
  sig<-function(rows)paste(sort(rows),collapse=',')
  stopifnot(identical(sort(vapply(actual,function(cell)sig(cell$value),character(1))),sort(vapply(expected,sig,character(1)))))
  for(cell in actual) if(keep)stopifnot(identical(cell,tibble::as_tibble(x[cell$value,c(a,b,'h','value')])) )
 }
 checks<-checks+1L
}
cat('PASS',checks,'pairwise special fixed/dimension name combinations, 4 plan sets each, with NA-level/sentinel factor and matrix key\n')
