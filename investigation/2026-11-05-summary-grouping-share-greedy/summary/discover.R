Sys.setenv(TESTTHAT_PARALLEL = 'false')
pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
d <- tibble(g = rep(c('a', 'b'), each=2), x = 1:4, y = 11:14)
exprs <- alist(
  tibble(z=sum(x)), data.frame(z=sum(x)), as.data.frame(list(z=sum(x))),
  if (sum(x) < 4) NA else tibble(z=sum(x)),
  if (sum(x) < 4) tibble(z=sum(x)) else NA,
  if (sum(x) < 4) tibble(z=sum(x)) else tibble(w=sum(x)),
  if (sum(x) < 4) tibble(z=sum(x)) else data.frame(z=sum(x)),
  if (sum(x) < 4) NA_real_ else tibble(z=sum(x)),
  structure(sum(x), class='AsIs'), I(matrix(sum(x))),
  matrix(sum(x), dimnames=list(NULL,'z')),
  data.frame(z=I(list(sum(x)))), tibble(z=I(matrix(sum(x)))),
  data.frame(z=sum(x), row.names='foo'),
  across(x:y, ~sum(.x)), (across(x:y, ~sum(.x))),
  {across(x:y, ~sum(.x))}, identity(across(x:y, ~sum(.x))),
  local(across(x:y, ~sum(.x))), (function() across(x:y, ~sum(.x)))(),
  (function(...) pick(...))(x), (function(...) pick(..1))(x),
  (function(x) pick(x))(x), (function(...) across(..., sum))(x),
  (function(...) across(..1, sum))(x),
  list(sum(x)), c(a=sum(x)), setNames(sum(x), 'a'),
  expression(sum(x)), quote(sum(x)), pairlist(a=sum(x)),
  factor('a'), ordered('a'), as.Date('2020-01-01'), as.POSIXlt('2020-01-01'),
  structure(1, class=c('foo','numeric')),
  tibble(), data.frame(), NULL, logical(),
  if (sum(x)<4) NULL else tibble(z=sum(x)),
  if (sum(x)<4) tibble() else tibble(z=sum(x))
)
get <- function(e, m) tryCatch(suppressWarnings(eval(if(m) rlang::expr(summarize_with_margins(d, !!e, .by=g)) else rlang::expr(summarize(d, !!e, .by=g)))), error=identity)
for(i in seq_along(exprs)) {
 a <- get(exprs[[i]], FALSE); b <- get(exprs[[i]], TRUE)
 eq <- if(inherits(a,'error') || inherits(b,'error')) inherits(a,'error') && inherits(b,'error') else isTRUE(all.equal(a,b,check.attributes=FALSE))
 if(!eq) { cat('\nDIFF',i,deparse1(exprs[[i]]),'\n');print(a);print(b) }
}
cat('completed',length(exprs),'comparisons\n')
