pkgload::load_all('.', quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = 'false')
assert_contract <- '--assert' %in% commandArgs(trailingOnly = TRUE)
rows <- list()
for (helper in c('share_of_parent', 'share_of_total')) {
 for (fixed in c(FALSE, TRUE)) for (source_kind in c('direct', 'across')) {
  for (timing in c('early', 'late')) for (operator in c('::', ':::')) {
   for (mode in c('count', 'error', 'corrupt')) {
    root <- new.env(parent = globalenv())
    root$source <- data.frame(p = c('A', 'A', NA, NA), g = rep(c('a', 'b'), 2), x = c(1, 3, 2, 6))
    input <- evalq(dtplyr::lazy_dt(source), root)
    original <- serialize(root$source, NULL)
    state <- new.env(parent = emptyenv()); state$count <- 0L
    callback <- local({
      target_operator <- operator; target_mode <- mode; target_state <- state
      function(pkg, name) {
       package <- as.character(substitute(pkg)); member <- as.character(substitute(name))
       matched <- if (target_operator == '::') identical(package, 'base') && identical(member, 'as.double') else identical(package, 'marginplyr') && identical(member, 'dtplyr_share_denominator')
       if (matched) {
        target_state$count <- target_state$count + 1L
        if (target_mode == 'error') stop('NAMESPACE_INTERCEPT')
        if (target_mode == 'corrupt') return(if (target_operator == '::') function(x) rep(10, length(x)) else function(value, identifier, target) 10)
       }
       if (target_operator == '::') getExportedValue(package, member) else getFromNamespace(member, package)
      }
    })
    if (timing == 'early') assign(operator, callback, root)
    ordinary <- if (source_kind == 'direct') rlang::quos(value = sum(x)) else rlang::quos(dplyr::across(x, sum, .names = 'value'))
    q <- summarize_with_margins(input, !!!ordinary, ratio = !!rlang::call2(helper, quote(value)), .by = !!if (fixed) quote(p) else NULL, .grouping = rollup(g), .sort = 'last')
    no_share <- summarize_with_margins(input, !!!ordinary, .by = !!if (fixed) quote(p) else NULL, .grouping = rollup(g), .sort = 'last')
    stopifnot(state$count == 0L)
    if (timing == 'late') assign(operator, callback, root)
    baseline <- dplyr::collect(no_share)
    stopifnot(state$count == 0L)
    result <- tryCatch(dplyr::collect(q), error = identity)
    intercepted <- state$count
    stopifnot(intercepted > 0L)
    rm(list = operator, envir = root)
    restored <- dplyr::collect(q)
    stopifnot(identical(restored$value, baseline$value), identical(original, serialize(root$source, NULL)))
    if (mode == 'error') stopifnot(inherits(result, 'error'), grepl('NAMESPACE_INTERCEPT', conditionMessage(result), fixed = TRUE))
    else if (mode == 'count') stopifnot(identical(result, restored))
    else stopifnot(identical(result$value, restored$value), !identical(result$ratio, restored$ratio))
    if (assert_contract) stopifnot(intercepted == 0L, identical(result, restored))
    rows[[length(rows) + 1L]] <- data.frame(helper, fixed, source_kind, timing, operator, mode, intercepted, status = if (mode == 'corrupt') 'wrong_ratio' else if (mode == 'error') 'collection_error' else 'callback_invoked')
   }
  }
 }
}
out <- do.call(rbind, rows)
output_dir <- Sys.getenv('MP_SHARE_HUNT_OUTPUT', unset = '')
if (nzchar(output_dir)) write.csv(out, file = file.path(output_dir, 'namespace-results.csv'), row.names = FALSE)
print(table(out$status))
cat('cases:', nrow(out), '; construction callbacks: 0; no-share callbacks: 0; restored values and source bytes: all agreed\n')
