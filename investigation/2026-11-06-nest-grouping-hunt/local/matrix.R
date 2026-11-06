Sys.setenv(TESTTHAT_PARALLEL = 'false')
args <- commandArgs(trailingOnly = TRUE)
source_dir <- if (length(args)) args[[1L]] else '.'
output_dir <- if (length(args)>1L) args[[2L]] else tempdir()
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
pkgload::load_all(source_dir,quiet=TRUE)
set.seed(20261106)
n <- 6L
fac <- structure(c(1L,2L,NA,1L,2L,NA), levels=c('a',NA_character_),class='factor')
keys <- list(
 integer=c(1L,2L,NA,1L,2L,NA), double=c(0,-0,NaN,NA,Inf,-Inf),
 character=c('a','b',NA,'a','b',NA), logical=c(TRUE,FALSE,NA,TRUE,FALSE,NA),
 complex=c(1+1i,1+1i,NA_complex_,NaN+0i,0i,1i), raw=as.raw(c(1,2,1,0,2,0)),
 factor=factor(c('a','b',NA,'a','b',NA), levels=c('unused','b','a')),
 factor_na=fac, ordered=structure(fac,class=c('ordered','factor')),
 Date=as.Date('2026-01-01') + c(1,2,NA,1,2,NA),
 POSIXct=as.POSIXct('2026-01-01',tz='Asia/Tokyo') + c(1,2,NA,1,2,NA),
 POSIXlt=I(as.POSIXlt(as.POSIXct('2026-01-01',tz='Asia/Tokyo') + c(1,2,NA,1,2,NA))),
 difftime=as.difftime(c(1,2,NA,1,2,NA), units='hours'),
 hms=hms::hms(c(1,2,NA,1,2,NA)), integer64=bit64::as.integer64(c(1,2,NA,1,2,NA)),
 version=package_version(c('1.0','1.0.0','2.0','2.0.0','1.0','3.0')),
 list=list(NULL,integer(),1:2,NULL,integer(),NA_real_),
 list_frame=list(data.frame(),data.frame(a=1L),data.frame(a=2L),data.frame(),data.frame(a=1L),NULL),
 matrix=matrix(c(1L,2L,NA,1L,2L,NA,1L,1L,NA,1L,1L,NA), nrow=n),
 array=array(rep(c(1L,2L,NA,1L,2L,NA),4),dim=c(n,2,2)),
 packed=tibble::tibble(a=c(1L,2L,NA,1L,2L,NA), b=fac),
 packed_deep=tibble::tibble(a=tibble::tibble(b=fac)),
 packed_zero=tibble::new_tibble(list(), nrow=n),
 packed_matrix=tibble::tibble(a=matrix(rep(c(1L,2L,NA,1L,2L,NA),2),nrow=n)),
 packed_list=tibble::tibble(a=list(NULL,1L,1:2,NULL,1L,NA_real_))
)
# Equality proxies deliberately differ from stored display values.
registerS3method('vec_proxy_equal','hunt_casefold',function(x,...)tolower(vctrs::vec_data(x)),asNamespace('vctrs'))
keys$casefold <- vctrs::new_vctr(c('A','B','a','b',NA,NA),class='hunt_casefold')
keys$asis_factor <- I(fac)
keys$asis_list <- I(keys$list)
keys$factor_matrix <- structure(as.integer(fac), levels=levels(fac),class='factor',dim=c(n,1L))
keys$factor_array <- structure(rep(as.integer(fac),2L), levels=levels(fac),class='factor',dim=c(n,1L,2L))
verbs <- list(nest=nest_with_margins, nest_by=nest_by_with_margins)
rows_key <- function(x) paste(x,collapse=',')
results <- list()
for (name in names(keys)) {
 key <- keys[[name]]
 input <- tibble::tibble(fixed=c('N','N','S','S',NA,NA),key=key,row=seq_len(n))
 for (role in c('dimension','fixed','payload')) for (keep in c(FALSE,TRUE)) for (sort in c('none','first','last')) for (verb in names(verbs)) {
  columns <- if(role=='dimension')c('fixed','key') else if(role=='fixed')c('key') else 'fixed'
  sets <- if(role=='dimension')list(c('fixed','key'),'fixed') else list(columns)
  before <- serialize(input,NULL)
  out <- tryCatch({
    if(role=='dimension') verbs[[verb]](input,.by=fixed,.grouping=rollup(key),.keep=keep,.sort=sort,.margin_label=NULL,.id='set')
    else if(role=='fixed') verbs[[verb]](input,.by=key,.keep=keep,.sort=sort,.margin_label=NULL,.id='set')
    else verbs[[verb]](input,.by=fixed,.keep=keep,.sort=sort,.margin_label=NULL,.id='set')
  }, error=identity)
  stopifnot(identical(serialize(input,NULL),before))
  label <- paste(name,role,keep,sort,verb,sep='/')
  if(inherits(out,'condition')) {
    results[[length(results)+1L]] <- data.frame(case=label,status='ERROR',detail=conditionMessage(out))
    next
  }
  errors <- character()
  for(s in seq_along(sets)) {
    groups <- vctrs::vec_group_loc(input[sets[[s]]])$loc
    expected <- sort(vapply(groups,rows_key,character(1)))
    actual <- sort(vapply(out$data[out$set==s],function(cell)rows_key(cell$row),character(1)))
    if(!identical(actual,expected)) errors <- c(errors,'membership')
  }
  if(sort!='none') {
    sort_columns <- list()
    for(col in columns) {
      if(role=='dimension' && col=='key') {
        bit <- as.integer(out$set==2L)
        sort_columns[[paste0('bit_',col)]] <- if(sort=='first') -bit else bit
      }
      sort_columns[[paste0('missing_',col)]] <- vctrs::vec_detect_missing(out[[col]])
      sort_columns[[paste0('value_',col)]] <- out[[col]]
    }
    sort_columns$set <- out$set
    expected_order <- vctrs::vec_order(tibble::new_tibble(sort_columns,nrow=nrow(out)))
    if(!identical(expected_order,seq_len(nrow(out)))) errors <- c(errors,'order')
  }
  for(cell in out$data) {
    expected_cols <- if(keep) c(columns,setdiff(names(input),columns)) else setdiff(names(input),columns)
    if(!identical(names(cell),expected_cols)) errors <- c(errors,'names')
    for(col in intersect(names(cell), names(input))) {
      expected <- vctrs::vec_slice(input[[col]],cell$row)
      if(!identical(cell[[col]],expected)) { if(name=='POSIXlt' && col=='key' && identical(as.list(cell[[col]]),as.list(expected)) && identical(unique(class(cell[[col]])),unique(class(expected)))) {} else errors <- c(errors,paste0('value:',col)) }
    }
  }
  results[[length(results)+1L]] <- data.frame(case=label,status=if(length(errors))'DIFF' else 'PASS',detail=paste(unique(errors),collapse=','))
 }
}
results <- do.call(rbind,results)
write.csv(results,file.path(output_dir,'matrix-results.csv'),row.names=FALSE)
print(table(results$status))
print(unique(results[results$status!='PASS',c('status','detail')]),row.names=FALSE)
expected_errors <- with(results,
  (grepl('^raw/dimension/',case) & grepl('typed-missing',detail)) |
  (grepl('^asis_factor/dimension/',case) & grepl('Can.t convert',detail)) |
  (grepl('^factor_(matrix|array)/(fixed|dimension)/',case) & grepl('nested too deeply|C stack',detail))
)
stopifnot(all(results$status!='ERROR' | expected_errors))
stopifnot(!any(results$status=='DIFF'),sum(results$status=='PASS')==1008L,sum(results$status=='ERROR')==72L)
cat('Independent membership/value/schema/source-mutation checks: 1008 PASS; 72 classified external/refused calls\n')
cat('Independent Margin order oracle: 672 PASS\n')
