pkgload::load_all('.',quiet=TRUE)
input <- tibble::tibble(a=1L,b='b',c=TRUE)
constructors <- c('grouping_sets','grouping_spec','rollup','cube')
consumers <- list(
 inspection=function(s) inspect_grouping(input,.grouping=s,.duplicates='drop',.format='list'),
 ordinary=function(s) nest_with_margins(input,.grouping=s,.duplicates='drop',.id='set'),
 rowwise=function(s) nest_by_with_margins(input,.grouping=s,.duplicates='drop',.id='set')
)
checks <- 0L
for(constructor in constructors) for(parentheses in 0:4) for(consumer in consumers) {
 env <- rlang::env(.GlobalEnv)
 reads <- 0L
 selected <- 'a'
 child <- grouping_set(a)
 makeActiveBinding('child',function() {reads <<- reads+1L;child},env)
 arg <- quote(child)
 for(i in seq_len(parentheses)) arg <- rlang::call2('(',arg)
 spec <- eval(rlang::call2(constructor,arg),env)
 for(nm in c('a','b','c','b','a')) {
   selected <- nm
   child <- eval(rlang::call2('grouping_set',rlang::sym(selected)),env)
   before <- reads
   actual <- consumer(spec)
   stopifnot(reads-before==1L)
   fresh <- eval(rlang::call2(constructor,child),env)
   control <- consumer(fresh)
   stopifnot(identical(actual,control))
   checks <- checks+1L
 }
}
# A public constructor call captured in an injected quosure must retain that
# quosure's own environment across outer environment rebinding.
for(constructor in constructors) for(parentheses in 0:4) for(consumer in consumers) {
 env <- rlang::env(.GlobalEnv, child=grouping_set(a))
 outer <- rlang::env(.GlobalEnv, child=grouping_set(b))
 arg <- rlang::new_quosure(quote(child),env)
 for(i in seq_len(parentheses)) arg <- rlang::call2('(',arg)
 spec <- eval(rlang::call2(constructor,arg),outer)
 for(nm in c('a','b','c','b','a')) {
   env$child <- eval(rlang::call2('grouping_set',rlang::sym(nm)),env)
   outer$child <- grouping_set(a,b,c)
   actual <- consumer(spec)
   control <- consumer(eval(rlang::call2(constructor,env$child),env))
   stopifnot(identical(actual,control))
   checks <- checks+1L
 }
}
cat('capture rebinding comparisons=',checks,'; all matched fresh construction; active nested names read once per operation.\n',sep='')
