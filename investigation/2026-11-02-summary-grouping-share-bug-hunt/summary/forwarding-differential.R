pkgload::load_all(quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
suppressPackageStartupMessages(library(dplyr))
fixture <- tibble(g = c("a", "a", "b"), x = 1:3, y = c(7, 9, 11))
capture <- function(expr, env) {
  value <- tryCatch(suppressWarnings(eval(expr, env)), error = identity)
  if (inherits(value, "error")) list(error = conditionMessage(value)) else list(value = value)
}
expressions <- c(
  "(function(...) across(c(...), sum))('x')",
  "(function(...) pick(c(...)))('x')",
  "(function(...) across(..., sum))('x')",
  "(function(cols) across({ force(cols); all_of(cols) }, sum))('x')",
  "(function(cols) across(all_of(force(cols)), sum))('x')",
  "(function(cols) across(all_of(cols), sum))(if (n() > 1L) 'x' else 'y')",
  "(function() { delayedAssign('cols', if (n() > 1L) 'x' else 'y'); across(all_of(cols), sum) })()",
  "(function() { e <- environment(); e$cols <- 'x'; across(all_of(e$cols), sum) })()",
  "(function() { e <- new.env(); e$cols <- 'x'; across(all_of(e$cols), sum) })()",
  "(function() { makeActiveBinding('cols', function() 'x', environment()); across(all_of(cols), sum) })()",
  "(function(cols) { q <- enquo(cols); across(!!q, sum) })(x)",
  "(function(cols) { q <- enquo(cols); pick(!!q) })(x)",
  "(function(cols) across({{ cols }}, sum))(x)",
  "(function(cols) pick({{ cols }}))(x)",
  "(function(cols) if_any({{ cols }}, ~ .x > 3))(x)",
  "(function(cols) { cols <- 'x'; across(all_of(.env$cols), sum) })('y')",
  "(function(cols) { cols <- 'x'; across(all_of(get('cols')), sum) })('y')",
  "(function(cols) { cols <- 'x'; across(all_of(eval(quote(cols))), sum) })('y')",
  "(function(cols) { cols <- 'x'; across(all_of(evalq(cols)), sum) })('y')",
  "(function(cols) { cols <- 'x'; across(all_of(rlang::eval_bare(quote(cols))), sum) })('y')",
  "(function(cols) { cols <- 'x'; across(where(function(z) is.numeric(z)), sum) })('y')",
  "(function(cols) { pred <- function(z) is.numeric(z); across(where(pred), sum) })('y')",
  "(function(cols) { pred <- function(z) is.numeric(z); pick(where(pred)) })('y')",
  "(function(cols) { pred <- function(z) is.numeric(z); sum(if_any(where(pred), ~ .x > 3)) })('y')",
  "(function(cols) { old <- getOption('tmp.selector'); options(tmp.selector='x'); on.exit(options(tmp.selector=old)); across(all_of(getOption('tmp.selector')), sum) })('y')",
  "(function(cols) { f <- function() cols; across(all_of(f()), sum) })('x')",
  "(function(cols) { f <- function(x=cols) x; across(all_of(f()), sum) })('x')",
  "(function(cols) { f <- function(x) x; across(all_of(f(cols)), sum) })('x')",
  "(function(cols) { f <- function(x) x; across(all_of(do.call(f,list(cols))), sum) })('x')",
  "(function(cols) { f <- function(x) x; across(all_of((get('f'))(cols)), sum) })('x')"
)
rows <- list()
k <- 0L
for (i in seq_along(expressions)) for (prefix in c("", "prior = sum(y), ")) for (named in c(FALSE, TRUE)) {
  k <- k + 1L
  env <- list2env(list(data = fixture, cols = "y"), parent = globalenv())
  dots <- paste0(prefix, if (named) "answer = " else "", expressions[[i]])
  ordinary <- capture(str2lang(paste0("dplyr::summarise(data, ", dots, ", .by = g)")), env)
  margin <- capture(str2lang(paste0("summarize_with_margins(data, ", dots, ", .by = g)")), env)
  verdict <- if (!is.null(ordinary$error) && !is.null(margin$error)) "paired-error" else if (isTRUE(all.equal(ordinary$value, margin$value)) && is.null(ordinary$error) && is.null(margin$error)) "match" else "DIVERGENCE"
  rows[[k]] <- data.frame(expr_id = i, prefix = nzchar(prefix), named = named, verdict = verdict, expr = expressions[[i]])
  if (verdict == "DIVERGENCE") {
    cat("CASE", k, expressions[[i]], "prefix", prefix, "named", named, "\n")
    print(ordinary)
    print(margin)
  }
}
result <- bind_rows(rows)
print(table(result$verdict))
write.csv(result, "investigation/2026-11-02-summary-grouping-share-bug-hunt/summary/forwarding-results.csv", row.names = FALSE)
