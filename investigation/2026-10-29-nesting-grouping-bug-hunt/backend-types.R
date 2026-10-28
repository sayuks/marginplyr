counts <- c(attempt=0L,both_error=0L,local_only_error=0L,dt_only_error=0L,difference=0L,equal=0L)
pkgload::load_all('.',quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
vectors <- list(logical=c(TRUE,FALSE,TRUE,NA), integer=c(1L,2L,1L,NA_integer_), double=c(1,2,1,NA_real_), numeric_special=c(0,-0,NaN,NA_real_), date=as.Date(c('2020-01-01','2020-01-02','2020-01-01',NA)), posix=as.POSIXct(c('2020-01-01','2020-01-02','2020-01-01',NA),tz='America/New_York'), difftime=as.difftime(c(1,2,1,NA),units='hours'), factor=factor(c('x','y','x',NA),levels=c('z','y','x')), ordered=ordered(c('x','y','x',NA),levels=c('z','y','x')), factorNA=addNA(factor(c('x','y','x',NA))), complex=c(1+1i,2+2i,1+1i,NA_complex_), raw=as.raw(c(1,2,1,0)), list=list(1,2,1,NULL), matrix=matrix(1:8,nrow=4), packed=tibble(a=1:4,b=letters[1:4]), array=array(1:16,dim=c(4,2,2)))
if(requireNamespace('bit64',quietly=TRUE)) vectors$integer64 <- bit64::as.integer64(c('9007199254740992','9007199254740993','9007199254740992',NA))
if(requireNamespace('hms',quietly=TRUE)) vectors$hms <- hms::as_hms(c(1,2,1,NA))
if(requireNamespace('vctrs',quietly=TRUE)) vectors$rcrd <- vctrs::new_rcrd(list(a=1:4))
for(type in names(vectors)) for(size in c(0,4)) for(role in c('payload','fixed','dimension')) for(keep in c(TRUE,FALSE)) for(label in list(NULL,'Total')) {
  input <- tibble(g=c('a','b','a',NA),v=1:4)
  input$x <- vectors[[type]]
  input <- vctrs::vec_slice(input, seq_len(size))
  run <- function(z) nest_with_margins(z,.by=if(role=='fixed') all_of('x'),.grouping=if(role=='dimension') rollup(x) else rollup(g),.keep=keep,.margin_label=label,.id='set',.sort='last')
  counts[['attempt']] <- counts[['attempt']] + 1L
  a <- tryCatch(run(input), error=identity)
  b <- tryCatch(collect(run(dtplyr::lazy_dt(input))), error=identity)
  norm <- function(z) { v<-as.list(z);v$data<-lapply(v$data,as.list); v }
  desc <- paste(type,size,role,keep,if(is.null(label))'NULL' else label)
  if(inherits(a,'error') || inherits(b,'error')) {
    key <- if(inherits(a,'error') && inherits(b,'error')) 'both_error' else if(inherits(a,'error')) 'local_only_error' else 'dt_only_error'; counts[[key]] <- counts[[key]] + 1L
    if(inherits(b,'error')&&!inherits(a,'error'))cat('DTERR',desc,conditionMessage(b),'\n')
  } else if(!identical(norm(a),norm(b))) { counts[['difference']]<-counts[['difference']]+1L; cat('DIFF',desc,'\n'); print(all.equal(norm(a),norm(b))) } else counts[['equal']]<-counts[['equal']]+1L
}

print(counts)
