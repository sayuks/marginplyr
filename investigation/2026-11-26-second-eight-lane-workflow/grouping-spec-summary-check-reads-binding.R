# Investigated SHA: 0fe5f1e40daa14e2ba2b5246452fa668bc330f80
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# The check for a Grouping specification written as a summary evaluates a caller argument or an active binding earlier and more often than dplyr does.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages({library(dplyr)})
d <- data.frame(g = c("a", "a", "b"), x = c(1, 2, 5))
log <- character()
note <- function(tag, value = 1) { log <<- c(log, tag); value }
quiet <- function(e) tryCatch(e, error = function(c) cat("  error:", conditionMessage(c), "\n"))

cat("1. Wrapper argument used as a bare summary, earlier summary fails.\n")
cat("   Expected (dplyr evaluates summaries in order and stops at the first failure): `w` never evaluated\n")
report_n <- function(data, w) list(
  summarise(data, bad = stop("boom"), total = w, .by = g),
  summarise(select(data, -g), bad = stop("boom"), total = w))
report_m <- function(data, w) summarize_with_margins(data, bad = stop("boom"), total = w, .grouping = rollup(g))
log <- character(); quiet(report_n(d, note("w evaluated"))); cat("   native  :", length(log), "evaluation(s)\n")
log <- character(); quiet(report_m(d, note("w evaluated"))); cat("   margin  :", length(log), "evaluation(s)\n")
report_m0 <- function(data, w) summarize_with_margins(data, bad = stop("boom"), total = w + 0, .grouping = rollup(g))
log <- character(); quiet(report_m0(d, note("w evaluated"))); cat("   control `total = w + 0`:", length(log), "evaluation(s)\n")

cat("\n2. Order of side effects on success: earlier summary logs per group, then `w`\n")
report_n2 <- function(data, w) list(
  summarise(data, a = note("summary a"), total = w, .by = g),
  summarise(select(data, -g), a = note("summary a"), total = w))
report_m2 <- function(data, w) summarize_with_margins(data, a = note("summary a"), total = w, .grouping = rollup(g))
log <- character(); invisible(report_n2(d, note("w"))); cat("   native :", paste(log, collapse = " > "), "\n")
log <- character(); invisible(report_m2(d, note("w"))); cat("   margin :", paste(log, collapse = " > "), "\n")

cat("\n3. Active binding read as a bare summary: invocations\n")
e <- new.env(); makeActiveBinding("w", function() note("w read"), e)
log <- character(); invisible(eval(quote(list(summarise(d, n = n(), total = w, .by = g),
  summarise(select(d, -g), n = n(), total = w))), e)); cat("   native, both grains:", length(log), "\n")
log <- character(); res <- eval(quote(summarize_with_margins(d, n = n(), total = w, .grouping = rollup(g))), e)
cat("   margin             :", length(log), "\n")
log <- character(); res <- eval(quote(summarize_with_margins(d, n = n(), total = w + 0, .grouping = rollup(g))), e)
cat("   margin, `w + 0`    :", length(log), "\n")

cat("\n4. Control for 2: the same wrapper with `total = w + 0`\n")
report_m3 <- function(data, w) summarize_with_margins(data, a = note("summary a"), total = w + 0, .grouping = rollup(g))
log <- character(); invisible(report_m3(d, note("w"))); cat("   margin :", paste(log, collapse = " > "), "\n")
