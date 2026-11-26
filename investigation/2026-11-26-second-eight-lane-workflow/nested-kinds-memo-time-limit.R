# Investigated SHA: 0fe5f1e40daa14e2ba2b5246452fa668bc330f80
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# The session memo of which kinds a nested position admits records any error as not admitted, a time limit included.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
args <- commandArgs(TRUE)
if (length(args) == 0L) {
  self <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  for (part in c("control", "trace_only", "poison", "nested_read")) system2(file.path(R.home("bin"), "Rscript"), c(shQuote(self), part))
  quit(status = 0)
}
suppressPackageStartupMessages({library(dplyr)})
invisible(Sys.setlocale("LC_MESSAGES", "C")); Sys.setenv(LANGUAGE = "en")
df <- data.frame(a = c("x", "y"), b = c("p", "q"), s = c("m", "n"), v = 1:2)
s <- grouping_set(a, b)
plan <- function(label) {
  r <- tryCatch({x <- inspect_grouping(df, .grouping = rollup(s))
    paste("RETURNED, sets:", paste(vapply(x$included, function(z) paste0("(", paste(z, collapse = ","), ")"), ""), collapse = " "))},
    error = function(e) paste0("ERROR <", class(e)[1], ">: ", substr(gsub("\n", " ", conditionMessage(e)), 1, 110)))
  cat("  ", label, r, "\n")
}
arm <- function() {
  assign(".armed", TRUE, globalenv())
  invisible(suppressMessages(trace("quo", tracer = quote(if (isTRUE(get0(".armed", globalenv())) && is.null(substitute(expr))) {
    assign(".armed", FALSE, globalenv()); for (i in 1:20) Sys.sleep(0.025) }), where = asNamespace("rlang"), print = FALSE)))
}
part <- args[[1]]
if (part == "control") {
  cat("== control: no trace, no limit (ADR 0026 refusal expected every time)\n")
  plan("first call:"); plan("second call:")
}
if (part == "trace_only") {
  cat("== trace without a limit: the delay alone does not change the answer\n")
  arm(); plan("first call (delayed):"); suppressMessages(untrace("quo", where = asNamespace("rlang"))); plan("second call:")
}
if (part == "poison") {
  cat("== limit 0.2 s lands in the memo's first computation\n")
  arm(); t0 <- Sys.time()
  r <- tryCatch({setTimeLimit(elapsed = 0.2, transient = TRUE); inspect_grouping(df, .grouping = rollup(s)); "RETURNED"},
    error = function(e) paste("ERROR", class(e)[1], gsub("\n", " ", conditionMessage(e))))
  setTimeLimit(elapsed = Inf)
  cat("   expected: an error (time limit, or the ambiguity refusal)\n   observed:", r, sprintf("(%.2f s)\n", as.numeric(Sys.time() - t0, units = "secs")))
  suppressMessages(untrace("quo", where = asNamespace("rlang")))
  cat("   later calls, no trace, no limit; expected: the ambiguity refusal\n")
  plan("observed:")
  r2 <- tryCatch({x <- summarize_with_margins(df, n = n(), .grouping = rollup(s)); paste(names(x), collapse = ",")}, error = function(e) paste("ERROR", class(e)[1]))
  cat("   observed, summarize_with_margins() columns:", r2, "\n")
}
if (part == "nested_read") {
  cat("== neighbour: the ambiguity check's own read of a wrapper's lazy argument (no trace)\n")
  slow <- function() { for (i in 1:20) Sys.sleep(0.025); "unused" }
  f <- function(data, a) inspect_grouping(data, .grouping = grouping_sets(a, NULL))
  t0 <- Sys.time()
  r <- tryCatch({setTimeLimit(elapsed = 0.2, transient = TRUE); f(df, slow()); "RETURNED"}, error = function(e) paste("ERROR", class(e)[1], conditionMessage(e)))
  setTimeLimit(elapsed = Inf)
  cat("   expected: an error; observed:", r, sprintf("(%.2f s)\n", as.numeric(Sys.time() - t0, units = "secs")))
  t0 <- Sys.time()
  r <- tryCatch({setTimeLimit(elapsed = 0.2, transient = TRUE); x <- slow(); "RETURNED"}, error = function(e) paste("ERROR", class(e)[1], conditionMessage(e)))
  setTimeLimit(elapsed = Inf)
  cat("   native control, forcing the same promise in plain R:", r, sprintf("(%.2f s)\n", as.numeric(Sys.time() - t0, units = "secs")))
}
