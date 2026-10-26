# Dated investigation evidence: 2026-10-26, source bfcdaf76a36d102132cd039c68ba8bc05a4fd465.
args <- commandArgs(trailingOnly=TRUE)
root <- if(length(args)) args[[1L]] else '.'
pkgload::load_all(root,quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
x <- dtplyr::lazy_dt(tibble(g=c('b','a'),v=1:2))
control <- nest_with_margins(arrange(x,g),.grouping=rollup(g))
stopifnot(nrow(collect(control))==3L)
actual <- tryCatch(nest_with_margins(arrange(filter(x,v>0),g),.grouping=rollup(g)),error=identity)
if(inherits(actual,'error'))cat(conditionMessage(actual),'\n')
stopifnot(!inherits(actual,'error'))
