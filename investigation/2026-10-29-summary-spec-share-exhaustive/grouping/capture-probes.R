suppressPackageStartupMessages(pkgload::load_all(quiet = TRUE))
options(lifecycle_verbosity = 'quiet', width = 120)
observe <- function(expr) tryCatch(force(expr), error = identity)
checks <- 0L
failures <- list()
record <- function(label, truth, detail = NULL) {
  checks <<- checks + 1L
  if (!isTRUE(truth)) failures[[label]] <<- detail
}
constructors <- list(set=grouping_set, sets=grouping_sets, rollup=rollup, cube=cube, product=grouping_spec)
children <- list(set=grouping_set(a), sets=grouping_sets(a), rollup=rollup(a), cube=cube(a), product=grouping_spec(a))
parens <- function(expr, n) {
  if (n) {
    for (i in seq_len(n)) expr <- rlang::call2('(', expr)
  }
  expr
}
for (parent in names(constructors)) for (child in names(children)) for (depth in 0:3) for (column in c(FALSE, TRUE)) for (quo_depth in 0:2) {
  count <- 0L
  env <- rlang::env(globalenv())
  value <- children[[child]]
  makeActiveBinding('s', function() {count <<- count+1L;value}, env)
  expr <- parens(quote(s), depth)
  q <- rlang::new_quosure(expr, env)
  if (quo_depth) {
    for (i in seq_len(quo_depth)) {
      q <- rlang::new_quosure(
        parens(q, depth), rlang::env(globalenv(), s = grouping_set(z))
      )
    }
  }
  spec <- rlang::inject(constructors[[parent]](!!q))
  data <- if(column) data.frame(a=1:2,s=3:4) else data.frame(a=1:2,z=3:4)
  result <- observe(inspect_grouping(data,.grouping=spec,.duplicates='keep',.format='list'))
  accepts <- parent %in% c('sets','product') || (parent %in% c('rollup','cube') && child=='set')
  expected_reads <- if(column && parent=='set') 0L else 1L
  expected_error <- if(column) accepts else !accepts
  id <- paste(parent,child,depth,column,quo_depth,sep='/')
  record(paste0(id,'/count'), count==expected_reads,list(reads=count,result=result))
  record(paste0(id,'/status'), inherits(result,'marginplyr_error')==expected_error,list(result=result))
}
# Alternating ordinary names retain the first reading per argument occurrence.
for (parent in names(constructors)) for (kind in c('character','integer','predicate','language')) for(depth in 0:3) for(quo_depth in 0:2) {
 count<-0L;env<-rlang::env(globalenv())
 values<-switch(kind,character=list('a','b'),integer=list(1L,2L),predicate=list(is.integer,is.character),language=list(quote(a),quote(b)))
 makeActiveBinding('s',function(){count<<-count+1L;values[[min(count,2L)]]},env)
 q<-rlang::new_quosure(parens(quote(s),depth),env)
 if (quo_depth) {
   for (i in seq_len(quo_depth)) {
     q <- rlang::new_quosure(
       parens(q, depth), rlang::env(globalenv(), s = 'b')
     )
   }
 }
 spec<-rlang::inject(constructors[[parent]](!!q));data<-data.frame(a=1:2,b=c('u','v'))
 result<-suppressWarnings(observe(inspect_grouping(data,.grouping=spec,.format='list')))
 expected <- suppressWarnings(observe(tidyselect::eval_select(rlang::new_quosure(quote(s),rlang::env(globalenv(),s=values[[1]])),data)))
 id<-paste('ordinary',parent,kind,depth,quo_depth,sep='/')
 record(paste0(id,'/reads'),count==1L,list(count=count,result=result))
 if(inherits(expected,'error'))record(paste0(id,'/status'),inherits(result,'error'),result) else {
  included<-names(expected)
  record(paste0(id,'/selection'),!inherits(result,'error') && identical(result$included[[1]],included),result)
 }
}
# Delayed promises are not forced by irrelevant colliding selection-only positions.
for(parent in names(constructors))for(column in c(FALSE,TRUE)) {
 env<-rlang::env(globalenv());count<-0L
 delayedAssign('s',{count<<-count+1L;grouping_set(a)},assign.env=env,eval.env=environment())
 spec<-rlang::inject(constructors[[parent]](!!rlang::new_quosure(quote(s),env)))
 data<-if(column)data.frame(s=1L,a=2L)else data.frame(a=1L)
 result<-observe(inspect_grouping(data,.grouping=spec,.format='list'))
 record(paste('promise',parent,column),count==if(column&&parent=='set')0L else 1L,list(result=result,count=count))
}
cat('Capture checks:',checks,'failures:',length(failures),'\n')
if(length(failures)){print(failures);quit(status=1L)}
