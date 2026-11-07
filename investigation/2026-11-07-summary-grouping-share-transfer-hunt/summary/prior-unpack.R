Sys.setenv(TESTTHAT_PARALLEL="false")
pkgload::load_all(commandArgs(trailingOnly=TRUE)[1L], quiet=TRUE)
library(dplyr)
d<-tibble(x=c(1,3))
run<-function(margin) {
 unpack<-FALSE
 f<-function(x)tibble(z=sum(x))
 tryCatch(if(margin)summarize_with_margins(d,
  ready={unpack<<-TRUE;0L},across(x,f,.unpack=unpack,.names="answer"),.id="answer") else
  summarise(d,ready={unpack<<-TRUE;0L},across(x,f,.unpack=unpack,.names="answer")),
  error=function(e)conditionMessage(e))
}
print(run(FALSE));print(run(TRUE))
