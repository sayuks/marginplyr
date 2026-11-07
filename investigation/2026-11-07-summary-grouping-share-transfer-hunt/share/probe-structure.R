Sys.setenv(TESTTHAT_PARALLEL='false')
pkgload::load_all('/private/tmp/marginplyr-bughunt-20261107-share/source',quiet=TRUE)
library(dplyr)
data <- data.frame(g=c('a','b'),x=c(1,3),prior=c(10,20))
reads <- alist(
(function(prior) { carrier <- list(read=function() sum(prior)); names(carrier) <- 'other'; rm(prior); carrier$other() })(1),
(function(prior) { carrier <- list(function() sum(prior)); names(carrier) <- 'read'; rm(prior); carrier$read() })(1),
(function(prior) { carrier <- list(read=function() sum(prior)); attr(carrier,'names') <- 'other'; rm(prior); carrier$other() })(1),
(function(prior) { carrier <- list(read=function() sum(prior)); attributes(carrier) <- list(names='other'); rm(prior); carrier$other() })(1),
(function(prior) { carrier <- list(read=function() 7, other=function() sum(prior)); names(carrier) <- c('other','read'); rm(prior); carrier$read() })(1),
(function(prior) { carrier <- list(read=function() 7, other=function() sum(prior)); length(carrier) <- 1L; rm(prior); carrier$read() })(1),
(function(prior) { carrier <- list(read=function() sum(prior)); length(carrier) <- 0L; rm(prior); 7 })(1),
(function(prior) { carrier <- list(read=function() sum(prior)); names(carrier) <- 'other'; rm(prior); carrier[[1L]]() })(1),
(function(prior) { carrier <- list(read=function() sum(prior)); dim(carrier) <- c(1L,1L); rm(prior); carrier[[1L]]() })(1)
)
for (i in seq_along(reads)) {
 result <- tryCatch(summarize_with_margins(data,source=sum(x),prior=share_of_total(source),observed=!!reads[[i]],.grouping=rollup(g)),error=identity)
 cat(i,': ', if(inherits(result,'error')) paste(class(result)[1],conditionMessage(result)) else paste(result$observed,collapse=','),'\n')
}
