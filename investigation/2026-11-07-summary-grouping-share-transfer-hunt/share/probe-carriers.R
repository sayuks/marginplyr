Sys.setenv(TESTTHAT_PARALLEL = 'false')
pkgload::load_all('/private/tmp/marginplyr-bughunt-20261107-share/source', quiet = TRUE)
library(dplyr)
data <- data.frame(g = c('a','b'), x = c(1,3), prior = c(10,20))
carriers <- alist(
 original,
 unname(original),
 setNames(original, 'read'),
 structure(original, names = 'read'),
 as.list(original),
 rep(original, 1L),
 rev(original),
 head(original, 1L),
 tail(original, 1L),
 unlist(list(original), recursive = FALSE),
 c(list(), original),
 identity(original),
 invisible(original),
 force(original),
 original[TRUE],
 original[-integer()],
 original[seq_along(original)],
 lapply(original, identity),
 Map(identity, original)
)
for (i in seq_along(carriers)) {
 read <- substitute((function(prior) {
   original <- list(function() sum(prior))
   copied <- CARRIER
   rm(prior)
   copied[[1L]]()
 })(1), list(CARRIER = carriers[[i]]))
 result <- tryCatch(summarize_with_margins(data, source=sum(x), prior=share_of_total(source), observed=!!read, .grouping=rollup(g)), error=identity)
 cat(i, paste(deparse(carriers[[i]]), collapse=''), ': ', if(inherits(result,'error')) paste(class(result)[1],conditionMessage(result)) else paste(result$observed,collapse=','), '\n')
}
