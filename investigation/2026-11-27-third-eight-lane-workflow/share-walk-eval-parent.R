# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# eval.parent() is not placed by the share dependency walk (#1808).
# Candidate 2: base eval.parent() is not one of the evaluators the share
# dependency walk places, so language it evaluates in the data mask is unseen.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressPackageStartupMessages(library(dplyr))

outcome <- function(expr) {
  tryCatch(expr, error = function(e) {
    paste0("ERROR <", class(e)[[1L]], ">: ", gsub("\n", " | ", conditionMessage(e)))
  })
}
show <- function(label, value) {
  cat(label, "\n")
  if (is.character(value)) cat("   ", value, "\n") else print(value)
}

hidden <- data.frame(g = c("a", "a", "b"), v = c(1, 2, 5), sh = c(100, 200, 300))

cat("== A later ordinary summary reads an earlier share through eval.parent() ==\n\n")
cat("Expected (ADR 0010: 'A later summary expression cannot refer to a Parent share created\n",
    "earlier in the same summarize_with_margins() call'): the Package refusal the same\n",
    "closure gets when it is written with evalq(sh, parent.frame()).\n\n", sep = "")
show("control: f <- function() sum(evalq(sh, parent.frame())):", outcome(summarize_with_margins(
  hidden, s = sum(v), sh = share_of_total(s),
  z = { f <- function() sum(evalq(sh, parent.frame())); f() }, .grouping = rollup(g)
)))
show("control: f <- function() sum(eval(quote(sh), parent.frame())):", outcome(summarize_with_margins(
  hidden, s = sum(v), sh = share_of_total(s),
  z = { f <- function() sum(eval(quote(sh), parent.frame())); f() }, .grouping = rollup(g)
)))
show("observed: f <- function() sum(eval.parent(quote(sh))):", outcome(summarize_with_margins(
  hidden, s = sum(v), sh = share_of_total(s),
  z = { f <- function() sum(eval.parent(quote(sh))); f() }, .grouping = rollup(g)
)))
show("observed: sum(eval.parent(quote(sh), n = 0)) written directly:", outcome(summarize_with_margins(
  hidden, s = sum(v), sh = share_of_total(s), z = sum(eval.parent(quote(sh), n = 0)),
  .grouping = rollup(g)
)))
cat("   (z = 300, 300, 600 are sums of the hidden INPUT column sh = 100, 200, 300)\n\n")

cat("Native control: plain dplyr with sh an ordinary earlier summary:\n")
print(summarise(hidden, s = sum(v), sh = s / 8,
                z = { f <- function() sum(eval.parent(quote(sh))); f() }, .by = g))
cat("   eval.parent() from f evaluates in f's caller, the data mask, so z is the earlier summary.\n\n")

cat("== A share source reads an earlier alias through eval.parent() ==\n\n")
d <- data.frame(g = c("a", "a", "b"), x = c(1, 2, 5), y = c(10, 20, 30))
show("control: value = sum(x) + { f <- function() evalq(prior, parent.frame()); f() }:",
     outcome(summarize_with_margins(
       d, prior = sum(y), value = sum(x) + { f <- function() evalq(prior, parent.frame()); f() },
       total = share_of_total(value), .grouping = rollup(g)
     )))
show("observed: value = sum(x) + { f <- function() eval.parent(quote(prior)); f() }:",
     outcome(summarize_with_margins(
       d, prior = sum(y), value = sum(x) + { f <- function() eval.parent(quote(prior)); f() },
       total = share_of_total(value), .grouping = rollup(g)
     )))
invisible(0)
