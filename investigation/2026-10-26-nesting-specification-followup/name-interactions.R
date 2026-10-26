# Dated investigation evidence: 2026-10-26, source bfcdaf76a36d102132cd039c68ba8bc05a4fd465.
args <- commandArgs(TRUE)
repo <- if (length(args)) args[[1]] else "."
pkgload::load_all(repo, quiet=TRUE)
set.seed(264013)
names_pool <- c('.N','.I','.SD','.GRP','.NGRP','.BY','.EACHI','.SDcols','.rows','.data','.env','T','F','i','j','by','key','check.names','keep.rownames','stringsAsFactors','...x','..marginplyr_nest_1','..marginplyr_column_1','..marginplyr_cell_1','..marginplyr_sort_1','a b','a`b','a\nb','a\tb','a\rb','日本語','ä','a,b','a=b','{x}','"x"','NA','TRUE','NULL','function','row.names','set','data','q')
base <- tibble::tibble(f=c('x','x','y','y'),g=factor(c('a',NA,'a','b'),levels=c('b','a','unused')),h=c(1L,2L,1L,1L),p=c('u','v','w','x'),r=1:4)
normalize <- function(x, key, map, id) {
  x<-dplyr::ungroup(tibble::as_tibble(x)); names(x)[match(id,names(x))]<-'setid'; names(x)[match(key,names(x))]<-'cell'
  for(i in seq_along(map)) {hit<-match(map[i],names(x)); if(!is.na(hit))names(x)[hit]<-names(map)[i]}
  x$cell <- lapply(x$cell,function(y){y<-tibble::as_tibble(y); for(i in seq_along(map)){hit<-match(map[i],names(y));if(!is.na(hit))names(y)[hit]<-names(map)[i]}; y[order(y$r),,drop=FALSE]})
  x <- x[order(x$setid,vapply(x$cell,function(y)if(nrow(y))min(y$r)else 0L,integer(1))),,drop=FALSE]
  x
}
failures<-list(); checks<-0L
for(i in 1:150) {
  picked<-sample(names_pool,7); map<-setNames(picked[1:5],names(base)); key<-picked[6]; id<-picked[7]; input<-base;names(input)<-unname(map)
  keep<-i%%2==0; kind<-if(i%%3==0)'by' else 'nest'; sort<-c('none','first','last')[1+(i%%3)]; verb<-if(kind=='by')nest_by_with_margins else nest_with_margins
  if(kind=='by' && '.rows'%in%c(unname(map[c('f','g','h')]),id))next
  control<-verb(base,.by=f,.grouping=cube(g,h),.keep=keep,.id='setid',.key='cell',.sort=sort,.margin_label=NULL)
  expected<-normalize(control,'cell',setNames(names(base),names(base)),'setid')
  for(backend in c('local','dtplyr')) {
    data<-if(backend=='dtplyr')dtplyr::lazy_dt(input)else input
    observed<-tryCatch(verb(data,.by=dplyr::all_of(unname(map['f'])),.grouping=cube(dplyr::all_of(unname(map[c('g','h')]))),.keep=keep,.id=id,.key=key,.sort=sort,.margin_label=NULL),error=identity)
    if(!inherits(observed,'error'))observed<-tryCatch(normalize(dplyr::collect(observed),key,map,id),error=identity)
    checks<-checks+1L
    ok<-!inherits(observed,'error') && isTRUE(all.equal(observed,expected,check.attributes=TRUE))
    if(!ok) {message('FAIL ',i,' ',backend,' ',paste(picked,collapse=' | '),' : ',if(inherits(observed,'error'))conditionMessage(observed)else paste(all.equal(observed,expected),collapse='; '));failures[[length(failures)+1]]<-list(i=i,backend=backend,picked=picked,observed=observed,expected=expected)}
  }
}
cat('name interaction checks:',checks,'failures:',length(failures),'\n')
stopifnot(length(failures)==0)
