Sys.setenv(TESTTHAT_PARALLEL='false')
pkgload::load_all('/private/tmp/marginplyr-bughunt-20261107-share/source',quiet=TRUE)
library(dplyr)
data <- data.frame(g=c('a','b'),x=c(1,3),prior=c(10,20))
reads <- alist(
(function(prior) { carrier <- list(list(function() sum(prior))); rm(prior); carrier[[1L]][[1L]]() })(1),
(function(prior) { carrier <- list(outer=list(read=function() sum(prior))); rm(prior); carrier$outer$read() })(1),
(function(prior) { carrier <- list(outer=list(read=function() sum(prior))); rm(prior); carrier[[c('outer','read')]]() })(1),
(function(prior) { carrier <- list(list(function() sum(prior))); copied <- carrier[[1L]]; rm(prior); copied[[1L]]() })(1),
(function(prior) { carrier <- pairlist(read=function() sum(prior)); rm(prior); carrier$read() })(1),
(function(prior) { carrier <- list(function() sum(prior)); copied <- carrier; rm(prior); copied[[1L]]() })(1),
(function(prior) { carrier <- list(function() sum(prior)); copied <- (function(x) x)(carrier); rm(prior); copied[[1L]]() })(1)
)
for (i in seq_along(reads)) {
 result <- tryCatch(summarize_with_margins(data,source=sum(x),prior=share_of_total(source),observed=!!reads[[i]],.grouping=rollup(g)),error=identity)
 cat(i,': ', if(inherits(result,'error')) paste(class(result)[1],conditionMessage(result)) else paste(result$observed,collapse=','),'\n')
}
