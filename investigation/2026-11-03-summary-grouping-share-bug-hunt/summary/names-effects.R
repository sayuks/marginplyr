pkgload::load_all(quiet=TRUE)
library(dplyr)
run <- function(margin, shares, variable) {
  d <- data.frame(g=c("a","a","b"),x=1:3)
  calls <- 0L
  prefix <- function() { calls <<- calls+1L; if (variable && calls > 1L) "b" else "a" }
  q <- rlang::quo(across(x, sum, .names="{prefix()}_{.col}"))
  dots <- list(q)
  if(shares) dots <- c(dots,list(fraction=rlang::quo(share_of_total(a_x))))
  out <- tryCatch(if(margin) summarize_with_margins(d,!!!dots,.by=g) else summarise(d,!!!dots,.by=g), error=function(cnd) conditionMessage(cnd))
  list(out=out,calls=calls)
}
for(variable in c(FALSE,TRUE)) for(shares in c(FALSE,TRUE)) {
 cat("variable:",variable,"shares:",shares,"\n")
 print(run(FALSE,FALSE,variable)); print(run(TRUE,shares,variable))
}
