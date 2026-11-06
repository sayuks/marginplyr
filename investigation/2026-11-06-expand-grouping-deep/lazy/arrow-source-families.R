Sys.setenv(TESTTHAT_PARALLEL = 'false')
pkgload::load_all('.', quiet = TRUE)
library(dplyr, warn.conflicts = FALSE)
library(arrow, warn.conflicts = FALSE)
output <- 'investigation/2026-11-06-expand-grouping-deep/lazy'
tmp <- tempfile('arrow-source-families-'); dir.create(tmp)
fixtures <- list(
  int64 = Array$create(c('9007199254740995', '-9007199254740995', NA_character_))$cast(int64()),
  uint64 = Array$create(c('18446744073709551615', '9007199254740995', NA_character_))$cast(uint64()),
  decimal = Array$create(c('12345678901234567.98765432','0.00000001',NA_character_))$cast(decimal128(25,8)),
  struct = Array$create(data.frame(a=c('a','b',NA_character_), b=c(1L,NA_integer_,3L))),
  list = Array$create(list(1:2,integer(),NULL),type=list_of(int32())),
  fixedlist = Array$create(list(1:2,3:4,NULL),type=fixed_size_list_of(int32(),2)),
  map = Array$create(list(data.frame(key=c('a','b'),value=1:2),data.frame(key='c',value=3L),NULL),type=map_of(utf8(),int32())),
  dict = DictionaryArray$create(Array$create(c(0L,1L,NA_integer_)), Array$create(c('b','a','unused'))),
  ordered = Array$create(ordered(c('b','a',NA),levels=c('a','b','unused'))),
  timestamp = Array$create(bit64::as.integer64(c(-1,1,NA)))$cast(timestamp('ns','America/New_York')),
  binary = Array$create(c('abc','def',NA_character_))$cast(fixed_size_binary(3))
)
families <- list(
  total_first=list(spec=grouping_sets(grouping_set(),grouping_set(g)),sets=list(character(),'g')),
  repeated=list(spec=grouping_sets(grouping_set(),grouping_set(g),grouping_set()),sets=list(character(),'g',character())),
  composite=list(spec=rollup(grouping_set(g,rid)),sets=list(c('g','rid'),character()))
)
canonical <- function(x) collect(arrange(x,sid,row_id),as_data_frame=FALSE)
check_table <- function(x,y) setequal(names(x),names(y)) && all(vapply(names(y),function(n) x$GetColumnByName(n)$Equals(y$GetColumnByName(n)),logical(1)))
results <- list(); k <- 0L
capture <- function(expr) tryCatch(force(expr),error=function(e)e)
for(nm in names(fixtures)) {
  ar <- fixtures[[nm]]
  t <- Table$create(g=ar,rid=1:3,row_id=1:3)
  path <- file.path(tmp,paste0(nm,'.arrow')); write_ipc_file(t,path)
  ds <- open_dataset(path,format='ipc')
  sources <- list(table=t,batch=as_record_batch(t),dataset=ds,uniondataset=open_dataset(list(ds,ds)))
  for(shape in names(sources)) for(family in names(families)) for(sort in c('none','first','last')) {
    source <- sources[[shape]]; f <- families[[family]]
    empty <- Scalar$create(NA,type=null())$cast(ar$type)
    branches <- lapply(seq_along(f$sets),function(i) {
      q <- mutate(source,sid=!!as.integer(i))
      if(!'g' %in% f$sets[[i]]) q <- mutate(q,g=!!empty)
      if(family=='composite' && !'rid' %in% f$sets[[i]]) q <- mutate(q,rid=NA_integer_)
      q
    })
    expected <- capture(canonical(Reduce(union_all,branches)))
    result <- capture(expand_with_margins(source,.grouping=f$spec,.id='sid',.duplicates='keep',.margin_label=NULL,.sort=sort))
    for(consumer in c('collect','compute','reader')) {
      actual <- capture(if(inherits(result,'error')) result else switch(consumer,
        collect=canonical(result),compute=canonical(compute(result)),reader=canonical(as_arrow_table(as_record_batch_reader(result)))))
      native_order <- if(inherits(actual,'error') && sort!='none') capture(collect(arrange(source,g),as_data_frame=FALSE)) else NULL
      same_order_boundary <- inherits(actual,'error') && inherits(native_order,'error') && all(vapply(list(actual,native_order),function(e) grepl('sort',conditionMessage(e),ignore.case=TRUE) && grepl(ar$type$ToString(),conditionMessage(e),fixed=TRUE),logical(1)))
      status <- if(inherits(expected,'error')) 'ordinary-error' else if(same_order_boundary) 'ordinary-order-error' else if(inherits(actual,'error')) 'package-error' else if(check_table(actual,expected)) 'match' else 'mismatch'
      k <- k+1L; results[[k]] <- data.frame(type=nm,shape,family,sort,consumer,status,error=if(inherits(actual,'error'))conditionMessage(actual) else '',native_error=if(inherits(native_order,'error'))conditionMessage(native_order) else '')
    }
  }
}
r <- bind_rows(results); write.csv(r,file.path(output,'arrow-source-families.csv'),row.names=FALSE)
print(count(r,status)); print(utils::head(filter(r,status!='match'),30))
unlink(tmp,recursive=TRUE)
stopifnot(!any(r$status %in% c('package-error','mismatch')))
