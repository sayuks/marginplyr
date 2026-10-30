pkgload::load_all('.', quiet = TRUE)
library(dplyr)
data <- tibble(g = c('a', 'a', 'b'), x = 1:3, y = c(3, 1, 2), z = c(TRUE, FALSE, TRUE))
forms <- c(
 'across(c(x,y), sum)', 'across(c(x,y), ~sum(.x))',
 'across(c(x,y), function(v) sum(v))',
 'across(c(x,y), ~sum(.x) + n())',
 'across(c(x,y), ~sum(.x) + nchar(cur_column()))',
 'across(c(x,y), ~data.frame(s=sum(.x), n=length(.x)), .unpack=TRUE)',
 'across(c(x,y), ~tibble(s=sum(.x), n=length(.x)), .unpack="{outer}.{inner}")',
 '(across)(c(x,y), ~sum(.x) + n())',
 'identity(across(c(x,y), ~sum(.x)))',
 'pick(x,y)', 'as.data.frame(pick(x,y))', 'list(pick(x,y))',
 'ncol(pick(where(is.numeric)))', 'sum(unlist(pick(where(is.numeric))))',
 'if_any(c(x,y), ~sum(.x)>2)', 'if_all(c(x,y), ~sum(.x)>2)',
 'list(quote(pick(g)))', 'list(expression(cur_group_id()))',
 'list(substitute(cur_group_id()))',
 'eval(quote(sum(x)))', 'evalq(sum(x))',
 'local(sum(x))', 'with(list(k=2), sum(x)+k)',
 '(function(v=n()) v)()', '(function(v=sum(x)) v)()',
 '{ f <- function(v=sum(x)) v; f() }',
 '{ a <- sum(x); data.frame(a=a, b=a*2) }',
 '{ out <- pick(x,y); lapply(out, sum) }',
 'data.frame(a=sum(x), b=sum(y))', 'tibble(a=sum(x), b=sum(y))',
 'c(a=sum(x), b=sum(y))', 'setNames(sum(x), "foo")',
 'list(across(c(x,y), sum))',
 '((across))(c(x,y), ~sum(.x))'
)
controls <- c('', 'a=sum(x),', 'x=sum(x),', 'x=as.character(sum(x)),')
capture <- function(expr) tryCatch(suppressWarnings(eval(expr)), error=function(e)e)
signals <- 0L
checks <- 0L
for (prefix in controls) for (form in forms) for (named in c(FALSE, TRUE)) {
 tail <- if (named) paste0('out = ', form) else form
 normal <- parse(text=paste0('dplyr::summarise(data,',prefix,tail,', .by=g)'))[[1]]
 margin <- parse(text=paste0('summarize_with_margins(data,',prefix,tail,', .by=g)'))[[1]]
 a <- capture(normal); b <- capture(margin); checks <- checks + 1L
 if (inherits(a,'error') && inherits(b,'error')) next
 eq <- if (inherits(a,'error') || inherits(b,'error')) FALSE else isTRUE(all.equal(as.data.frame(a),as.data.frame(b),check.attributes=TRUE))
 if (!eq) {
  signals <- signals + 1L
  cat('\nSIGNAL', signals, 'PREFIX',prefix,'NAMED',named,'FORM',form,'\n')
  print(a); print(b)
 }
}
cat('\nCHECKS',checks,'SIGNALS',signals,'\n')
