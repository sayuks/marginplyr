pkgload::load_all('.',quiet=TRUE)
f <- structure(c(1L,2L,NA_integer_,3L,2L),levels=c('x',NA_character_,'unused'),class='factor')
fixtures <- list(packed=tibble::tibble(f=f),recursive=tibble::tibble(df=tibble::tibble(f=f)),list=lapply(seq_along(f),function(i)f[i]),factor_array=array(as.integer(f),dim=c(5L,1L,1L)))
fixtures$factor_array <- structure(fixtures$factor_array,levels=levels(f),class='factor')
calls <- checks <- accepted <- 0L; problems <- list(); expected_refusals <- list()
for (what in names(fixtures)) for(role in c('payload','dimension','fixed')) for(verb in c('nest_with_margins','nest_by_with_margins')) for(keep in c(FALSE,TRUE)) for(sort in c('none','first','last')) {
  x<-tibble::tibble(g=c('a','a','b','b','a'),row_id=1:5)
  x[[what]]<-fixtures[[what]]
  args<-list(.data=x,.keep=keep,.id='set',.sort=sort,.margin_label=NULL)
  group<-if(role=='dimension')rlang::call2('rollup',rlang::sym(what)) else rlang::call2('rollup',rlang::sym('g'))
  by<-if(role=='fixed')rlang::sym(what) else NULL
  calls<-calls+1L
  z<-tryCatch(rlang::inject((get(verb))(.data=x,.by=!!by,.grouping=!!group,.keep=keep,.id='set',.sort=sort,.margin_label=NULL)),error=identity)
  tag<-paste(what,role,verb,keep,sort,sep='/')
  if(inherits(z,'error')) {
    if (what == 'factor_array' && role != 'payload' && inherits(z, 'expressionStackOverflowError')) expected_refusals[[tag]] <- conditionMessage(z) else problems[[tag]] <- conditionMessage(z)
    next
  }
  accepted <- accepted + 1L
  if(role!='payload'&&!keep)next
  for(cell in z$data) {
    checks<-checks+1L
    if(!identical(cell[[what]],vctrs::vec_slice(fixtures[[what]],cell$row_id)))problems[[tag]]<-list(actual=cell[[what]],expected=vctrs::vec_slice(fixtures[[what]],cell$row_id))
  }
}
cat('calls', calls, 'accepted', accepted, 'projections', checks, 'upstream recursion controls', length(expected_refusals), 'unexpected differences', length(problems), '\n')
if (length(problems)) print(problems)
stopifnot(calls == 144L, accepted == 120L, checks == 378L, length(expected_refusals) == 24L, length(problems) == 0L)
cat('PASS: nested NA-level factors retained exact original values and levels\n')
