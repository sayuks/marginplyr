# Dated investigation evidence: 2026-10-26, source bfcdaf76a36d102132cd039c68ba8bc05a4fd465.
args <- commandArgs(trailingOnly=TRUE)
root <- if(length(args)) args[[1L]] else '.'
pkgload::load_all(root,quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
x <- tibble(a=c('x','y'), b=c(1,1))
control <- function(a) names(select(x,base::c(a)))
actual <- function(a) inspect_grouping(x,.by=base::c(a),.format='list')$fixed[[1L]]
expected <- control(if(is.numeric(tidyselect::peek_data()$a)) 'a' else 'b')
observed <- actual(if(is.numeric(tidyselect::peek_data()$a)) 'a' else 'b')
cat('expected fixed:',expected,'\n')
cat('actual fixed:',observed,'\n')
stopifnot(identical(observed,expected))
