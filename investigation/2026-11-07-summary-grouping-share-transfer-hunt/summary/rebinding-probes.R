Sys.setenv(TESTTHAT_PARALLEL="false")
pkgload::load_all(commandArgs(trailingOnly=TRUE)[1L],quiet=TRUE)
library(dplyr)
data <- tibble(x=c(1,3))
run <- function(margin, where, initial, final) {
  fns <- initial
  cols <- function() { fns <<- final; "x" }
  template <- function() { fns <<- final; "answer_{.fn}" }
  e <- if (where == "cols") rlang::expr(across(cols(), fns, .names="answer_{.fn}")) else rlang::expr(across(x, fns, .names=template()))
  tryCatch(if(margin) summarize_with_margins(data, !!e, .id="answer_1") else summarise(data, !!e),error=function(e)conditionMessage(e))
}
for(where in c("cols","names")) for(initial in list(sum,list(s=sum))) for(final in list(sum,list(s=sum,m=max))) {
  print(list(where=where, initial=initial,final=final,a=run(FALSE,where,initial,final),b=run(TRUE,where,initial,final)))
}
