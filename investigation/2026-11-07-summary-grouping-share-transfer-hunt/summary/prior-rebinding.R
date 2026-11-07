Sys.setenv(TESTTHAT_PARALLEL="false")
pkgload::load_all(commandArgs(trailingOnly=TRUE)[1L],quiet=TRUE)
library(dplyr)
d <- tibble(x=c(1,3))
run <- function(margin, level=0L) {
 fns <- sum
 tryCatch(if(margin) summarize_with_margins(d,
   ready={fns <<- list(s=sum,m=max);0L}, across(x,fns,.names="answer_{.fn}"), .id="answer_1") else summarise(d,
   ready={fns <<- list(s=sum,m=max);0L}, across(x,fns,.names="answer_{.fn}")),error=function(e) conditionMessage(e))
}
print(run(FALSE));print(run(TRUE))
