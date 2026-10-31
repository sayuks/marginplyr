pkgload::load_all('.', quiet=TRUE)
library(dplyr)
d <- tibble(g=c('a','a','b'),x=1:3,y=c(5,6,7))
cols <- 'y'
forms <- c(
"(function(cols) across(all_of(cols), sum))('x')",
"(function(cols) pick(all_of(cols)))('x')",
"local({ cols <- 'x'; across(all_of(cols), sum) })",
"local({ cols <- 'x'; pick(all_of(cols)) })",
"{ cols <- 'x'; across(all_of(cols), sum) }",
"{ cols <- 'x'; pick(all_of(cols)) }",
"(function(cols) across(all_of(.env$cols), sum))('x')",
"(function(cols) pick(all_of(.env$cols)))('x')",
"(function(cols) across(all_of(get('cols')), sum))('x')",
"(function(cols) across(all_of(eval(quote(cols))), sum))('x')",
"(function(cols) eval(quote(across(all_of(cols), sum))))('x')",
"(function(cols) eval(quote(pick(all_of(cols)))))('x')",
"(function(cols) rlang::eval_tidy(rlang::quo(across(all_of(cols), sum))))('x')",
"sapply('x', function(cols) sum(pick(all_of(cols))[[1]]))",
"sapply('x', function(cols) sum(across(all_of(cols))[[1]]))",
"(function(cols) across(all_of(cols), function(x) sum(x)+length(cols)))('x')",
"(function(cols) across(where(function(x) is.numeric(x) && cols=='x'), sum))('x')",
"(function(cols) (function() across(all_of(cols), sum))())('x')",
"(function(cols) { f <- function() across(all_of(cols), sum); f() })('x')",
"with(list(cols='x'), across(all_of(cols), sum))",
"with(list(cols='x'), pick(all_of(cols)))",
"evalq(across(all_of(cols), sum), list(cols='x'))",
"evalq(pick(all_of(cols)), list(cols='x'))",
"(function(.env) across(all_of(cols), sum))(list(cols='x'))",
"(function(.env) pick(all_of(cols)))(list(cols='x'))",
"(function(.data) across(all_of(cols), sum))(list(cols='x'))",
"(function(.data) pick(all_of(cols)))(list(cols='x'))"
)
run <- function(text, margin=FALSE) tryCatch({
  e <- parse(text=text)[[1]]
  q <- rlang::new_quosure(e, globalenv())
  if(margin) summarize_with_margins(d, !!q, .by=g) else summarise(d, !!q,.by=g)
},error=function(e) e)
count <- 0L
for (i in seq_along(forms)) {
  a <- run(forms[[i]])
  b <- run(forms[[i]], TRUE)
  eq <- if (inherits(a,'error') && inherits(b,'error')) TRUE else identical(a,b)
  cat(i, if(eq) 'PASS' else 'DIFF', forms[[i]],'\n')
  if(!eq) {count <- count+1L; print(a);print(b)}
}
cat('forms=',length(forms),' differences=',count,'\n')
