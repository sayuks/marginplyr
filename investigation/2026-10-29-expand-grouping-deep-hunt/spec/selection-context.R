args <- commandArgs(TRUE)
pkgload::load_all(if (length(args)) args[[1]] else '.', quiet=TRUE)
options(lifecycle_verbosity='quiet')
set.seed(10292026)
data <- tibble::tibble(alpha=1:2, beta=c('a','b'), gamma=as.Date('2026-10-29')+1:2, delta=list(1:2,3), epsilon=factor(c('b','a')))
vars <- names(data)
cols <- c('alpha','gamma')
wrap <- function(x,n=1L) {for(i in seq_len(n))x<-rlang::call2('(',x);x}
lexical <- function(x=cols)c(x)
peek_types <- function()names(tidyselect::peek_data())[vapply(tidyselect::peek_data(),is.numeric,logical(1))]
peeks <- list(quote(all_of(names(tidyselect::peek_data()))), quote(all_of(peek_types())),quote(all_of(lexical())),quote(all_of(tidyselect::peek_vars()[c(1,3)])),quote(where(is.numeric)),quote(where(is.factor)),quote(where(is.list)),quote(where(function(x)inherits(x,'Date'))),quote(where(~is.character(.x))))
leaves <- c(lapply(vars,rlang::sym),list(quote(-1),quote(c(1L,3L)),quote(c('alpha','gamma')),quote(NULL),quote(logical()),quote(0),quote(NA),quote(1.5),quote(-99),quote(everything()),quote(last_col()),quote(starts_with('a')),quote(ends_with('a')),quote(matches('a$')),quote(num_range('a',1:2)),quote(any_of(c(cols,'missing'))),quote(all_of(cols)),quote(.data$alpha),quote(.data[['beta']]),quote(.data[[cols]])),peeks)
headforms <- function(name)list(rlang::sym(name),wrap(rlang::sym(name)),rlang::call2('::',quote(base),rlang::sym(name)))
make <- function(depth=3L){
 if(depth==0L||runif(1)<0.5)return(sample(leaves,1)[[1]])
 op<-sample(c('c',':','!','-','|','&','/','&&','||','*','^'),1)
 head<-sample(headforms(op),1)[[1]]
 n<-if(op%in%c('!','-'))1L else 2L
 rlang::call2(head,!!!replicate(n,make(depth-1L),simplify=FALSE))
}
exprs <- c(leaves,replicate(300,make(),simplify=FALSE))
# Injected selection quosures preserve the innermost environment through call
# parentheses; these are distinct from nested specifications.
for (i in seq_along(leaves)){
 e <- rlang::env(environment(),cols=c('beta','epsilon'))
 inner <- rlang::new_quosure(leaves[[i]],e)
 outer <- rlang::new_quosure(wrap(inner,3L),rlang::env(environment(),cols='delta'))
 exprs[[length(exprs)+1L]] <- outer
}
observe<-function(fn)tryCatch(suppressWarnings(fn()),error=identity)
counts<-c(success=0L,error=0L,rename=0L,empty_dimensions=0L)
mismatches<-list(); cases<-0L; excluded<-0L
for (expr_index in seq_along(exprs)){
 expr <- exprs[[expr_index]]
 if (expr_index %% 50L == 0L) cat('Progress:',expr_index,'of',length(exprs),'expressions\n')
 if(any(grepl('!!',rlang::expr_deparse(expr),fixed=TRUE))){excluded<-excluded+1L;next}
 q<-if(rlang::is_quosure(expr))expr else rlang::new_quosure(expr,environment())
 oracle<-observe(function()tidyselect::eval_select(q,data))
 for(backend in c('local','dtplyr')){
  input<-if(backend=='local')data else dtplyr::lazy_dt(data)
  for(consumer in c('fixed','grouping_set','grouping_sets','grouping_spec','rollup','cube')){
   cases<-cases+1L
   actual<-observe(function(){if(consumer=='fixed')inspect_grouping(input,.by=!!q,.format='list') else {spec<-eval(rlang::call2(consumer,q));inspect_grouping(input,.grouping=spec,.format='list')}})
   mismatch<-NULL
   if(inherits(oracle,'error')){
    if(!inherits(actual,'error')||!identical(class(actual),class(oracle)))mismatch<-'external_error_class' else counts['error']<-counts['error']+1L
   }else if(any(names(oracle)!=vars[unname(oracle)])){
    if(!inherits(actual,'marginplyr_error'))mismatch<-'rename_refusal' else counts['rename']<-counts['rename']+1L
   }else if(length(oracle)==0L&&consumer%in%c('rollup','cube')){
    if(!inherits(actual,'marginplyr_error')||!grepl('requires at least one dimension',conditionMessage(actual),fixed=TRUE))mismatch<-'empty_refusal' else counts['empty_dimensions']<-counts['empty_dimensions']+1L
   }else{
    expected<-if(consumer=='fixed')list(names(oracle)) else if(consumer%in%c('grouping_set','grouping_sets','grouping_spec'))list(names(oracle)) else if(consumer=='rollup')lapply(rev(seq.int(0L,length(oracle))),function(n)names(oracle)[seq_len(n)]) else {idx<-unlist(lapply(rev(seq.int(0L,length(oracle))),function(n)utils::combn(seq_along(oracle),n,simplify=FALSE)),recursive=FALSE);lapply(idx,function(k)names(oracle)[k])}
    answer<-if(inherits(actual,'error'))actual else if(consumer=='fixed')actual$fixed else actual$included
    if(!identical(answer,expected))mismatch<-'selection_value' else counts['success']<-counts['success']+1L
   }
   if(!is.null(mismatch))mismatches[[length(mismatches)+1L]]<-list(expr=expr,backend=backend,consumer=consumer,kind=mismatch,oracle=oracle,actual=actual)
  }
 }
}
cat('Expressions:',length(exprs),'excluded injection ASTs:',excluded,'consumer comparisons:',cases,'\n');print(counts)
cat('Mismatches:',length(mismatches),'\n')
if(length(mismatches)){saveRDS(mismatches,file.path(tempdir(),'marginplyr-deep-selection-mismatches.rds'));for(m in head(mismatches,30L)){cat(rlang::expr_deparse(m$expr),m$backend,m$consumer,m$kind,'\n');print(m$oracle);print(m$actual)}}
stopifnot(length(mismatches)==0L)
