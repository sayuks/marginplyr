Sys.setenv(TESTTHAT_PARALLEL = 'false')
pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
d <- tibble(g = c(1L, 1L, 2L), x = c(1, 2, 3))
wrappers <- alist(
  VALUE, identity(VALUE), force(VALUE), invisible(VALUE), {VALUE},
  local(VALUE), eval(quote(VALUE)), evalq(VALUE),
  if (TRUE) VALUE else stop('unused'), switch('yes', yes = VALUE),
  tryCatch(VALUE, error = function(e) e),
  withCallingHandlers(VALUE, warning = function(w) invokeRestart('muffleWarning')),
  (function(z) z)(VALUE), (function(z = VALUE) z)(),
  (function() return(VALUE))(), (function() {on.exit(invisible(NULL)); VALUE})(),
  (function() {for (i in 1:2) {if (i == 2) return(VALUE)}})(),
  (function() {repeat {return(VALUE)}})(),
  do.call(function(z) z, list(VALUE)), list(VALUE)
)
values <- alist(sum(x), data.frame(out = sum(x)), data.frame(out = list(I(list(sum(x))))))
results <- list(); i <- 0L
for (input in c('data.frame', 'tibble', 'data.table')) {
  original <- switch(input, data.frame = as.data.frame(d), tibble = d,
                     data.table = data.table::as.data.table(d))
  for (v in seq_along(values)) for (w in seq_along(wrappers)) {
    expr <- do.call(substitute, list(wrappers[[w]], list(VALUE = values[[v]])))
    ordinary <- tryCatch(rlang::eval_tidy(rlang::call2(dplyr::summarise,original,!!!list(expr),.by=quote(g))),error=identity)
    margin <- tryCatch(rlang::eval_tidy(rlang::call2(summarize_with_margins,original,!!!list(expr),.by=quote(g))),error=identity)
    matched <- if(inherits(ordinary,'error') || inherits(margin,'error')) inherits(ordinary,'error') && inherits(margin,'error') else isTRUE(all.equal(ordinary,margin,check.attributes=FALSE))
    i <- i+1L; results[[i]] <- data.frame(input,value=v,wrapper=w,matched,ordinary_error=inherits(ordinary,'error'),margin_error=inherits(margin,'error'))
    if(!matched) {cat('DIFF',input,v,w,deparse1(expr),'\n');print(ordinary);print(margin)}
  }
}
results <- do.call(rbind,results)
write.csv(results,'investigation/2026-11-06-summary-grouping-share-exhaustive/summary/control-flow-controls.csv',row.names=FALSE)
cat('CONTROL FLOW',nrow(results),'DIFFS',sum(!results$matched),'BOTH REFUSED',sum(results$ordinary_error & results$margin_error),'\n')
stopifnot(all(results$matched))
