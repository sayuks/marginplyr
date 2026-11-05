pkgload::load_all(quiet = TRUE)
n <- as.integer(commandArgs(TRUE)[[1L]])
policy <- commandArgs(TRUE)[[2L]]
spec <- cube(!!!rep(list(quote(a)), n))
input <- data.frame(a = 1L)
output <- tryCatch(inspect_grouping(input, .grouping = spec, .duplicates = policy), error = identity)
if (inherits(output, 'error')) {
  cat(n, policy, class(output)[[1L]], strsplit(conditionMessage(output), '\n', fixed = TRUE)[[1L]][[1L]], '\n')
} else {
  stopifnot(identical(output$included, c('(a)', '()')))
  cat(n, policy, 'two sets\n')
}
