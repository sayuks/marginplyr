# Run from the repository root; exploratory evidence for 2026-10-28.
wt <- '.'
pkgload::load_all(wt, quiet=TRUE)
types <- list(
 logical=c(TRUE,FALSE,NA,TRUE),integer=c(1L,2L,NA,1L),double=c(1,1+1e-15,NaN,NA_real_),signed=c(-0,0,-Inf,Inf),complex=c(1+1i,1+2i,NA_complex_,complex(real=NaN,imaginary=2)),
 character=c('x','y',NA,'x'),factor=factor(c('x','y',NA,'x'),levels=c('y','x','z')),ordered=ordered(c('x','y',NA,'x'),levels=c('y','x','z')),
 factor_na=factor(c('x',NA,'y',NA),levels=c('x','y',NA),exclude=NULL),factor_na_missing=structure(c(1L,3L,NA_integer_,3L),levels=c('x','y',NA),class='factor'),
 date=as.Date(c('2020-01-01','2020-01-02',NA,'2020-01-01')),datetime=as.POSIXct(c('2020-01-01','2020-01-02',NA,'2020-01-01'),tz='America/New_York'),
 difftime=as.difftime(c(1,2,NA,1),units='hours'),list=list(1L,2L,NULL,1L),list_mix=list(1,'1',NA,NULL),
 packed=tibble::tibble(a=c(1,2,NA,1),b=c('x','y',NA,'x')),
 packed_nested=tibble::tibble(a=tibble::tibble(z=c(1,2,NA,1)),b=c('x','y',NA,'x')),
 matrix=matrix(c(1:6,NA,1),4,2),array=array(c(1:6,NA,1),c(4,1,2)),raw=as.raw(c(1,2,0,1)),
 posixlt=as.POSIXlt(c('2020-01-01','2020-01-02',NA,'2020-01-01'),tz='UTC'),
 asis=I(c(1,2,NA,1)))
if(requireNamespace('bit64',quiet=TRUE))types$integer64<-bit64::as.integer64(c('9007199254740992','9007199254740993',NA,'9007199254740992'))
checks<-0L;bad<-list();errs<-list()
for(t in names(types))for(verb in list(nest_with_margins,nest_by_with_margins))for(keep in c(FALSE,TRUE))for(size in c(0L,4L))for(s in c('none','last','first'))for(fixed in c(FALSE,TRUE)) {
 x<-tibble::tibble(g=vctrs::vec_slice(types[[t]],seq_len(size)), h=c('b','a','b',NA)[seq_len(size)], payload=seq_len(size))
 checks<-checks+1L
 actual<-tryCatch(rlang::inject(verb(x,.by=!!(if(fixed)rlang::sym('h')else NULL),.grouping=!!(if(fixed)rollup(g)else cube(g,h)),.margin_label=NULL,.id='SET',.keep=keep,.sort=s)),error=identity)
 tag<-paste(t,if(identical(verb,nest_by_with_margins))'by'else'nest',keep,size,s,fixed)
 if(inherits(actual,'error')){errs[[tag]]<-conditionMessage(actual);cat('ERROR',tag,conditionMessage(actual),'\n');next}
 sets<-if(fixed)list(c('h','g'),'h')else list(c('g','h'),'g','h',character())
 expected_membership<-lapply(sets,function(keys){
  if(size==0L)return(list())
  if(!length(keys))return(list(seq_len(size)))
  vctrs::vec_group_loc(x[keys])$loc
 })
 actual_membership<-lapply(seq_along(sets),function(id)lapply(actual$data[actual$SET==id],function(z)sort(z$payload)))
 norm<-function(z)sort(vapply(z,function(x)paste(x,collapse=','),''))
 if(!identical(lapply(actual_membership,norm),lapply(expected_membership,norm))){bad[[tag]]<-list(actual=actual_membership,expected=expected_membership);cat('BAD',tag,'\n');print(bad[[tag]])}
 if(keep)for(z in actual$data){wanted<-x[z$payload,];if(!identical(as.list(z)[names(wanted)],as.list(wanted))){cat('KEEP BAD',tag,'\n');bad[[paste(tag,'KEEP')]]<-list(actual=z,expected=wanted)}}
}
cat('TOTAL',checks,'BAD',length(bad),'ERROR',length(errs),'\n');saveRDS(list(bad=bad,errs=errs),'/tmp/marginplyr-4306-local-type-results.rds')
