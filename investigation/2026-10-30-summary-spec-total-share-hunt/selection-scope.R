pkgload::load_all('.', quiet=TRUE)
library(dplyr)
data <- tibble(g=c('a','a','b'), x=1:3, y=c(5,6,7))
cols <- 'y'
ordinary <- dplyr::summarise(data, (function(cols) across(all_of(cols), sum))('x'), .by=g)
margin <- summarize_with_margins(data, (function(cols) across(all_of(cols), sum))('x'), .by=g)
cat('ORDINARY\n'); print(ordinary)
cat('MARGIN\n'); print(margin)
if ('--assert' %in% commandArgs(TRUE)) stopifnot(identical(ordinary, margin))
