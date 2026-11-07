args <- commandArgs(trailingOnly=TRUE)
source_dir <- normalizePath(args[[1L]],mustWork=TRUE)
output <- args[[2L]]
dir.create(output,recursive=TRUE,showWarnings=FALSE)
Sys.setenv(TESTTHAT_PARALLEL='false')
pkgload::load_all(source_dir,quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
functions <- c(character='margin_display_character',factor_encode='encode_factor_for_margin',factor_restore='reconstruct_factor_vector')
plans <- list(single=grouping_set(g),rollup=rollup(g),repeated=grouping_sets(grouping_set(g),grouping_set(),grouping_set(g)))
rows <- list()
canon <- function(x) arrange(x,sid,value)
for(kind in names(functions)) for(graph in c('root','filter','cache','renamed_root')) for(plan in names(plans)) for(sort in c('none','first','last')) for(timing in c('early','late')) for(mode in c('count','corrupt','error')) {
 root <- new.env(parent=globalenv())
 root$source <- data.table::data.table(g=if(kind=='character')c('north','south')else factor(c('north','south'),levels=c('south','north','unused')),value=c(13L,29L))
 before <- serialize(root$source,NULL)
 input <- evalq(dtplyr::lazy_dt(source,immutable=TRUE),root)
 input <- switch(graph,root=input,filter=filter(input,value>0L),cache=compute(input,name='copy'),renamed_root=rename(rename(input,key=g),g=key))
 spec <- plans[[plan]]
 expected <- collect(expand_with_margins(input,.grouping=spec,.duplicates='keep',.id='sid',.sort=sort))
 native_expected <- collect(input)
 count <- 0L
 target <- functions[[kind]]
 dispatcher <- function(pkg,name) {
  package <- as.character(substitute(pkg));member <- as.character(substitute(name))
  if(identical(c(package,member),c('marginplyr',target))) {
   count <<- count+1L
   if(mode=='error') return(function(...)stop('LABEL_NAMESPACE_INTERCEPT'))
   if(mode=='corrupt') return(switch(kind,
     character=function(x)rep('MASK',length(x)),
     factor_encode=function(x,...)rep('MASK',length(x)),
     factor_restore=function(x,...)factor(rep('MASK',length(x)),levels='MASK')))
  }
  utils::getFromNamespace(member,package)
 }
 if(timing=='early') assign(':::',dispatcher,envir=root)
 query <- expand_with_margins(input,.grouping=spec,.duplicates='keep',.id='sid',.sort=sort)
 construction_calls <- count
 assign(':::',dispatcher,envir=root)
 count <- 0L;native <- collect(input); native_calls <- count
 count <- 0L;mutation <- collect(mutate(input,value=value+0L));mutation_calls<-count
 count <- 0L;ordinary_union <- collect(union_all(input,input));union_calls<-count
 count <- 0L;got <- tryCatch(collect(query),error=identity);expanded_calls<-count
 rm(list=':::',envir=root)
 restored <- collect(query)
 value_match <- !inherits(got,'error') && identical(canon(got),canon(expected))
 rows[[length(rows)+1L]] <- data.frame(kind,graph,plan,sort,timing,mode,construction_calls,native_calls,mutation_calls,union_calls,expanded_calls,native_match=identical(native,native_expected),mutation_match=identical(mutation,native_expected),union_match=identical(ordinary_union,bind_rows(native_expected,native_expected)),value_match,restored_match=identical(canon(restored),canon(expected)),source_unchanged=identical(before,serialize(root$source,NULL)),error=if(inherits(got,'error'))conditionMessage(got)else'')
}
result<-bind_rows(rows)
write.csv(result,file.path(output,'label-namespace.csv'),row.names=FALSE)
print(count(result,mode,value_match));print(colSums(result[,c('construction_calls','native_calls','mutation_calls','union_calls','expanded_calls')]))
stopifnot(nrow(result)==648L,all(result$construction_calls==0L),all(result$native_calls==0L),all(result$mutation_calls==0L),all(result$union_calls==0L),all(result$expanded_calls>0L),all(result$native_match),all(result$mutation_match),all(result$union_match),all(result$restored_match),all(result$source_unchanged),all(result$value_match[result$mode=='count']),!any(result$value_match[result$mode!='count']))
cat('PASS: 648 observations classified, 216 callback-only, 216 corrupt results, 216 callback errors\n')
