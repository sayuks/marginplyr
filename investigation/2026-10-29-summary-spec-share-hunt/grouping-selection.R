pkgload::load_all(quiet=TRUE)
set.seed(6539)
options(lifecycle_verbosity='quiet')
names_pool <- c('a','b','c','d','.data','.env','.','NA','TRUE','NULL','Inf','NaN','a b','a,b','a=b','a`b','a\nb','a\tb','日本語','Å','é','é','\\','"','[',']','(','$','::','1','-1','a/b','foo<bar>','{x}','%>%')
checks <- 0L
for(i in 1:100) {
 vars <- sample(names_pool, sample(3:8,1)); data <- as.data.frame(setNames(rep(list(1:3),length(vars)),vars), check.names=FALSE)
 target <- sample(vars,1)
 exprs <- list(rlang::sym(target), rlang::call2('all_of',target), rlang::call2('any_of',c(target,'nonexistent-column')), rlang::call2('c',!!!rlang::syms(rev(vars))), quote(everything()), quote(1:2), quote(-1), quote(where(is.integer)),quote(where(function(x) length(x)==3L)),rlang::call2('[[',quote(.data),target),quote(last_col()),quote(c(1L,1L,2L)),quote(c()),quote(NULL),quote(-c(1L,2L)),quote(1/2))
 for(expr in exprs) {
   direct <- tryCatch(tidyselect::eval_select(expr,data),error=identity)
   observed <- tryCatch(inspect_grouping(data,.grouping=grouping_set(!!expr),.format='list'),error=identity)
   checks <- checks+1L
   if(inherits(direct,'error')) {
     if(!inherits(observed,'error') || !identical(class(direct),class(observed))) {print(vars); print(expr); print(direct); print(observed); stop('error mismatch')}
   } else if(!identical(observed$included[[1]],names(direct))) { print(vars); print(expr); print(direct); print(observed); stop('selection mismatch') }
 }
}
cat(checks,'selection differential checks PASS\n')
