pkgload::load_all('.', quiet = TRUE)
options(lifecycle_verbosity='quiet')
set.seed(20261029)
data <- tibble::tibble(alpha=1:3,beta=c('x','y','z'),gamma=c(TRUE,FALSE,NA),delta=as.Date('2026-10-29')+1:3)
columns <- names(data)
# Local wrappers and opaque or qualified heads give ordinary R evaluation.
cols <- c('alpha','gamma')
choose <- function(...) c('alpha','gamma')
lookup <- function() names(tidyselect::peek_data())[vapply(tidyselect::peek_data(),is.numeric,logical(1))]
head <- function(x) x
alias_peek <- tidyselect::peek_vars
lexical <- function(x=cols) rev(x)
dots <- function(...) c(...)
leaf <- list(quote(alpha),quote(beta),quote(delta),quote(-1),quote(c(1,3)),quote(c('alpha','delta')),quote(NULL),quote(NA),quote(logical()),quote(0),quote(-99),quote(everything()),quote(starts_with('a')),quote(ends_with('a')),quote(contains('et')),quote(matches('^(alpha|gamma)$')),quote(last_col()),quote(all_of(cols)),quote(any_of(c(cols,'missing'))),quote(where(is.numeric)),quote(where(is.character)),quote(where(function(x)inherits(x,'Date'))),quote(all_of(lookup())),quote(all_of(lexical())),quote(all_of(alias_peek()[c(1,3)])),quote(choose()),quote((choose)()),quote(get('choose')()),quote(identity(cols)),quote(base::rev(cols)),quote(base::c('alpha','delta')),quote(base::`-`(c(1,3))),quote(.data$alpha),quote(.data[['beta']]))
make <- function(depth=2L) {if(depth==0L||runif(1)<0.45)return(sample(leaf,1)[[1]]); op<-sample(c('c','!','-','|','&','/','(',':'),1);if(op%in%c('!','-','('))return(rlang::call2(op,make(depth-1L))); rlang::call2(op,make(depth-1L),make(depth-1L))}
exprs<-c(leaf,replicate(1800,make(),simplify=FALSE))
obs<-function(f)tryCatch(suppressWarnings(f()),error=function(e)e)
counts<-c(success=0L,external_error=0L,mismatch=0L)
skipped<-0L
for(expr in exprs) {
 if(any(grepl('!!',rlang::expr_deparse(expr),fixed=TRUE))){skipped<-skipped+1L;next}
 q<-rlang::new_quosure(expr,environment())
 expected<-obs(function() tidyselect::eval_select(q,data))
 actual<-obs(function() inspect_grouping(data,.grouping=grouping_set(!!q),.format='list'))
 renamed<-!inherits(expected,'error')&&any(names(expected)!=columns[unname(expected)])
 if(renamed) {if(!inherits(actual,'marginplyr_error'))stop('rename mismatch');next}
 if(inherits(expected,'error')) {
  if(!inherits(actual,'error')||!identical(class(expected),class(actual))) {cat('ERROR CLASS MISMATCH',rlang::expr_deparse(expr),'\n');print(expected);print(actual);counts['mismatch']<-counts['mismatch']+1L} else counts['external_error']<-counts['external_error']+1L
 } else if(inherits(actual,'error')||!identical(names(expected),actual$included[[1]])) {cat('VALUE MISMATCH',rlang::expr_deparse(expr),'\n');print(expected);print(actual);counts['mismatch']<-counts['mismatch']+1L} else counts['success']<-counts['success']+1L
}
print(counts)
stopifnot(counts['mismatch']==0L)
cat('PASS selection expression differential:',length(exprs)-skipped,'checked from',length(exprs),'generated;',skipped,'injection ASTs excluded\n')
