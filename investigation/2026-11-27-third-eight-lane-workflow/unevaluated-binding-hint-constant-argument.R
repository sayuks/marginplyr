# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# The unevaluated-binding hint for a wrapper's constant argument appears on the interpreted call only (#1811).
# Candidate 5: the #1773 unevaluated-binding hint is offered whenever a later
# unnamed across() reads a lazy binding, without asking whether evaluating it
# would make the outputs known: its force() remedy then fails, and for a
# constant argument the hint appears on the interpreted first call only.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressPackageStartupMessages(library(dplyr))

d <- data.frame(g = c("a", "a", "b"), x = c(1, 2, 5), y = c(10, 20, 30))
msg <- function(expr) tryCatch({
  r <- expr
  paste("accepted, sh =", paste(round(r$sh, 3), collapse = " "))
}, error = function(e) gsub("\n", "\n      ", conditionMessage(e)))

cat("Expected (#1772: 'The hint is true'; #1773: 'each remedy, applied to the reproduction,\n",
    "is accepted'; #1773 What to build 1: the outcome must not depend on byte-compilation):\n",
    "a remedy the hint names makes the call accepted, and one source line gets one message.\n\n", sep = "")

cat("== (i) a constant argument read by a later across() whose .names template is the reason ==\n")
report <- function(data, measures) summarize_with_margins(
  data, n = n(), across(all_of(measures), sum, .names = "{sub('q', '', .col)}_t"),
  sh = share_of_total(x_t), .grouping = rollup(g))
forced <- function(data, measures) {
  force(measures)
  summarize_with_margins(
    data, n = n(), across(all_of(measures), sum, .names = "{sub('q', '', .col)}_t"),
    sh = share_of_total(x_t), .grouping = rollup(g))
}
reordered <- function(data, measures) summarize_with_margins(
  data, across(all_of(measures), sum, .names = "{sub('q', '', .col)}_t"), n = n(),
  sh = share_of_total(x_t), .grouping = rollup(g))
caller <- function() report(d, "x")
for (i in 1:3) cat(sprintf("   caller() call %d: %s\n", i, msg(caller())))
cat("   force(measures) as hinted: ", msg(forced(d, "x")), "\n")
cat("   across() written first:    ", msg(reordered(d, "x")), "\n")
cat("   by hand 3/8, 5/8, 8/8:      0.375 0.625 1\n\n")

cat("== (ii) a .fns argument that holds a list of functions ==\n")
fns_report <- function(data, fn) summarize_with_margins(
  data, across(c(x, y), fn), sh = share_of_total(y_s), .grouping = rollup(g))
fns_forced <- function(data, fn) {
  force(fn)
  summarize_with_margins(data, across(c(x, y), fn), sh = share_of_total(y_s),
                         .grouping = rollup(g))
}
cat("   fns_report(d, list(s = sum, m = mean)):\n      ", msg(fns_report(d, list(s = sum, m = mean))), "\n")
cat("   force(fn) as hinted:\n      ", msg(fns_forced(d, list(s = sum, m = mean))), "\n")
cat("\nNative control: plain dplyr names these outputs in the same functions:\n")
print(names(summarise(d, n = n(), across(all_of("x"), sum, .names = "{sub('q', '', .col)}_t"), .by = g)))
print(names((function(data, fn) summarise(data, across(c(x, y), fn), .by = g))(d, list(s = sum, m = mean))))
invisible(0)
