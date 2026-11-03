options(lifecycle_verbosity = "quiet", width = 140)
pkgload::load_all(quiet = TRUE)
library(dplyr)
data <- tibble(g = c("a", "a", "b"), x = 1:3, y = 5:7)
forms <- c(
 'across(x, (function(v) { force(v); function(z) sum(z) + sum(v) })(x))',
 'across(x, (function(v) { force(v); function(z) sum(z) + v })(n()))',
 'across(x, (function(v) { force(v); function(z) sum(z) + v })(sum(x)))',
 'across(x, local({v <- sum(x); function(z) sum(z) + v}))',
 'across(x, local({v <- n(); function(z) sum(z) + v}))',
 'across(x, ~ sum(.x) + sum(y))',
 'across(x, function(z) sum(z) + sum(y))',
 'across(x, list(total = (function(v) { force(v); function(z) sum(z) + v })(sum(y))))'
)
cap <- function(expr, margin, shares) {
 tryCatch({
  q <- rlang::new_quosure(expr, environment())
  dots <- list(q)
  if (shares) dots <- c(dots, list(fraction = rlang::quo(share_of_total(x))))
  if (margin) summarize_with_margins(data, !!!dots, .grouping=rollup(g), .id="set") else summarise(data, !!q, .by=g)
 },error=function(cnd) conditionMessage(cnd))
}
for(txt in forms) {
 cat("\n",txt,"\n")
 for(shares in c(FALSE,TRUE)) {cat("shares",shares,"\n");print(cap(str2lang(txt),FALSE,FALSE));print(cap(str2lang(txt),TRUE,shares))}
}
