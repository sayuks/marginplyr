# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# A refused inner Margin verb is blamed on the enclosing verb's call (#1806).
# Candidate 5: a Package condition raised by the caller's own code inside a
# summary expression -- a helper that calls another marginplyr function -- is
# unwrapped from dplyr's argument/group context and re-blamed on the outer
# Margin verb. The handler in summarize_margin_stage()'s tryCatch() was written
# for marginplyr's generated frame checks, and takes every marginplyr_error
# parent; with_margin_error_call() then rewrites its call.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
Sys.setenv(LANGUAGE = "en")
sales <- data.frame(region = c("E", "E", "W"), store = c("A", "B", "C"), v = 1:3)
lookup <- data.frame(k = 1:2)

# Caller helpers that call marginplyr themselves and are refused there.
lookup_sets <- function() nrow(inspect_grouping(lookup, .grouping = "k"))
is_total <- function() grouping_bit(region) == 1L

report <- function(label, expr) {
  cnd <- tryCatch({ force(expr); NULL }, error = identity)
  cat(" ", label, "\n")
  if (is.null(cnd)) { cat("    RETURNED\n"); return(invisible()) }
  cat("    class: ", paste(class(cnd), collapse = "/"), "\n", sep = "")
  cat("    blamed call: ", deparse(conditionCall(cnd), width.cutoff = 200L)[[1]], "\n", sep = "")
  lines <- strsplit(cli::ansi_strip(conditionMessage(cnd)), "\n")[[1]]
  cat(paste0("    | ", lines, collapse = "\n"), "\n")
  cat("    names the argument `n = ...`/`t = ...`: ",
      any(grepl("In argument", lines, fixed = TRUE)), "\n", sep = "")
}
cat("Expected: the inner refusal keeps a context naming the argument it arose in\n",
    "(as dplyr gives it) and is not blamed on the outer call, whose own\n",
    "`.grouping = rollup(region)` is valid.\n\n", sep = "")
inputs <- list(data.frame = sales, data.table = data.table::as.data.table(sales))
for (nm in names(inputs)) {
  d <- inputs[[nm]]
  cat("==", nm, "\n")
  report("native summarise(n = n() * lookup_sets(), .by = region):",
         summarise(d, n = n() * lookup_sets(), .by = region))
  report("summarize_with_margins(n = n() * lookup_sets(), .grouping = rollup(region)):",
         summarize_with_margins(d, n = n() * lookup_sets(), .grouping = rollup(region)))
}
cat("== data.frame, a helper wrapping grouping_bit()\n")
report("native summarise(t = is_total(), .by = region):",
       summarise(sales, t = is_total(), .by = region))
report("summarize_with_margins(t = is_total(), .grouping = rollup(region)):",
       summarize_with_margins(sales, t = is_total(), .grouping = rollup(region)))
cat("\n== control: a caller error of another class keeps dplyr's context\n")
plain <- function() stop("lookup table unavailable")
report("summarize_with_margins(n = n() * plain(), .grouping = rollup(region)):",
       summarize_with_margins(sales, n = n() * plain(), .grouping = rollup(region)))
cat("\n== dtplyr evaluates the summary at collect(), outside the verb\n")
report("collect(summarize_with_margins(lazy_dt, n = n() * lookup_sets())):",
       collect(summarize_with_margins(dtplyr::lazy_dt(sales), n = n() * lookup_sets(),
                                      .grouping = rollup(region))))
