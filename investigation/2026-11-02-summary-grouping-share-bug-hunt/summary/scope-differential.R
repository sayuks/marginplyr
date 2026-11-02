pkgload::load_all(quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
suppressPackageStartupMessages(library(dplyr))

# Compare one grain with ordinary dplyr before investigating divergences.
fixture <- tibble(g = c("a", "a", "b"), x = 1:3, y = c(7, 9, 11))
capture <- function(expr, env) {
  warnings <- character()
  value <- tryCatch(withCallingHandlers(eval(expr, env), warning = function(cnd) {
    warnings <<- c(warnings, conditionMessage(cnd))
    invokeRestart("muffleWarning")
  }), error = identity)
  list(value = value, warnings = warnings)
}
bodies <- c(
  "across(all_of(cols), sum)",
  "pick(all_of(cols))",
  "sum(if_any(all_of(cols), ~ .x > 5))",
  "sum(if_all(all_of(cols), ~ .x > 5))",
  "across(where(pred), sum)",
  "nrow(pick(all_of(cols)))"
)
wrappers <- c(
  "%s",
  "{ cols <- 'x'; pred <- is.numeric; %s }",
  "local({ cols <- 'x'; pred <- is.numeric; %s })",
  "(function(cols, pred) %s)('x', is.numeric)",
  "(function(...) { cols <- 'x'; pred <- is.numeric; %s })()",
  "eval(quote({ cols <- 'x'; pred <- is.numeric; %s }))",
  "eval(substitute({ cols <- 'x'; pred <- is.numeric; %s }))",
  "eval(parse(text = %s))",
  "(function(cols, pred) { force(cols); force(pred); %s })('x', is.numeric)",
  "(function(cols, pred) { z <- function() %s; z() })('x', is.numeric)",
  "(function(cols, pred) { z <- function(arg) arg; z(%s) })('x', is.numeric)",
  "(function(cols, pred) { delayedAssign('z', %s); z })('x', is.numeric)",
  "with(list(cols = 'x', pred = is.numeric), %s)",
  "within(list(cols = 'x', pred = is.numeric), { out <- %s })$out"
)
rows <- list()
k <- 0L
for (b in seq_along(bodies)) for (w in seq_along(wrappers)) {
  body <- bodies[[b]]
  if (w == 8L) body <- deparse(sprintf("{ cols <- 'x'; pred <- is.numeric; %s }", body))
  text <- sprintf(wrappers[[w]], body)
  for (prefix in c("", "prior = sum(y), ")) for (named in c(FALSE, TRUE)) {
    k <- k + 1L
    env <- new.env(parent = globalenv())
    env$cols <- "y"
    env$pred <- is.character
    env$data <- fixture
    dots <- paste0(prefix, if (named) "answer = " else "", text)
    ordinary <- capture(str2lang(paste0("dplyr::summarise(data, ", dots, ", .by = g)")), env)
    margin <- capture(str2lang(paste0("summarize_with_margins(data, ", dots, ", .by = g)")), env)
    ordinary_error <- inherits(ordinary$value, "error")
    margin_error <- inherits(margin$value, "error")
    verdict <- if (ordinary_error && margin_error) "paired-error" else if (!ordinary_error && !margin_error && isTRUE(all.equal(ordinary$value, margin$value))) "match" else "DIVERGENCE"
    rows[[k]] <- data.frame(body = b, wrapper = w, prefix = nzchar(prefix), named = named, verdict = verdict, expr = text)
    if (verdict == "DIVERGENCE") {
      cat("CASE", k, text, "prefix", prefix, "named", named, "\n")
      print(ordinary)
      print(margin)
    }
  }
}
result <- bind_rows(rows)
print(table(result$verdict))
write.csv(result, "investigation/2026-11-02-summary-grouping-share-bug-hunt/summary/scope-results.csv", row.names = FALSE)
