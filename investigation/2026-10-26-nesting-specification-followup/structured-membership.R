# Dated investigation evidence: 2026-10-26, source bfcdaf76a36d102132cd039c68ba8bc05a4fd465.
args<-commandArgs(TRUE);repo<-if(length(args))args[[1]]else'.';pkgload::load_all(repo,quiet=TRUE)
set.seed(264012)
fixture<-list(
  nested_packed=function(n)tibble::tibble(a=sample(c(1L,2L,NA_integer_),n,TRUE),b=tibble::tibble(z=factor(sample(c('a','b',NA),n,TRUE),levels=c('b','a','z')))),
  array3=function(n)array(sample(c(1L,2L,NA_integer_),n*4,TRUE),dim=c(n,2L,2L)),
  array1=function(n)array(sample(c(1L,2L,NA_integer_),n,TRUE),dim=n),
  complex=function(n)sample(c(1+1i,1-1i,NA_complex_,NaN+0i),n,TRUE),
  raw=function(n)as.raw(sample(0:2,n,TRUE)),
  list_dataframe=function(n)lapply(sample(1:3,n,TRUE),function(i)list(tibble::tibble(a=1L),tibble::tibble(a=2L),NULL)[[i]]),
  list_expression=function(n)lapply(sample(1:3,n,TRUE),function(i)list(quote(a+b),quote(a-b),NULL)[[i]]),
  difftime=function(n)as.difftime(sample(c(1,2,NA),n,TRUE),units='hours'),
  POSIXct=function(n)as.POSIXct(sample(c(0,1,NA_real_),n,TRUE),origin='1970-01-01',tz='Asia/Tokyo'),
  list_factor=function(n)lapply(sample(1:3,n,TRUE),function(i)list(factor('a',levels=c('a','b')),factor('a',levels=c('b','a')),NULL)[[i]]),
  factor_na=function(n)structure(sample(c(1L,2L,NA_integer_),n,TRUE),levels=c('a',NA_character_),class=c('ordered','factor')))
checks<-0L;failures<-list(); unsupported<-list()
for(iter in 1:30)for(typ in names(fixture)) {
  n<-sample(0:8,1);x<-tibble::tibble(f=sample(c('p','q',NA_character_),n,TRUE),g=fixture[[typ]](n),h=sample(c('x','y',NA_character_),n,TRUE),rid=seq_len(n))
  # Only source types accepted by dplyr's grouping enter the nesting oracle.
  source_groups<-tryCatch(vctrs::vec_group_loc(x['g']),error=identity)
  if(inherits(source_groups,'error')){unsupported[[typ]]<-conditionMessage(source_groups);next}
  keep<-iter%%2==0;spec<-if(iter%%2)cube(g,h)else grouping_sets(grouping_set(g),grouping_set(h),grouping_set())
  plan<-inspect_grouping(x,.by=f,.grouping=spec,.format='list')
  for(verb in list(nest_with_margins,nest_by_with_margins)) {
    result<-tryCatch(verb(x,.by=f,.grouping=spec,.margin_label=NULL,.id='sid',.keep=keep,.sort=c('none','first','last')[1+iter%%3]),error=identity)
    if(inherits(result,'error')){failures[[length(failures)+1]]<-list(iter=iter,type=typ,error=conditionMessage(result));next}
    for(s in seq_len(nrow(plan))) {
      cols<-c('f',plan$included[[s]]); groups<-vctrs::vec_group_loc(x[cols])$loc
      expected<-sort(vapply(groups,function(rows)paste(rows,collapse=','),character(1)))
      actual<-sort(vapply(result$data[result$sid==s],function(cell)paste(cell$rid,collapse=','),character(1)))
      checks<-checks+1L
      if(!identical(expected,actual))failures[[length(failures)+1]]<-list(iter=iter,type=typ,set=s,expected=expected,actual=actual)
      for (cell in result$data[result$sid == s]) {
        if (keep && !identical(cell$g, vctrs::vec_slice(x$g, cell$rid))) {
          failures[[length(failures) + 1L]] <- list(iter = iter, type = typ, kind = 'kept values')
        }
      }
    }
  }
}
cat('structured source membership checks:',checks,'failures:',length(failures),'\n');print(failures);cat('upstream unsupported types:\n');print(unsupported)
stopifnot(!length(failures))
