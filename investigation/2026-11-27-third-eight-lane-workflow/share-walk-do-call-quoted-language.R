# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# Quoted language in a do.call() argument list is not read by the share dependency walk (#1807).
# Candidate 1: the share dependency walk reads do.call()'s arguments as values,
# so language handed to do.call() (quote(), as.name(), rlang::syms(),
# str2lang(), call()) is evaluated in the data mask unseen.
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

# An input that carries a column named like the share, and one that does not.
hidden <- data.frame(g = c("a", "a", "b"), v = c(1, 2, 5), sh = c(100, 200, 300))
plain <- data.frame(g = c("a", "a", "b"), v = c(1, 2, 5))

cat("== Direction 1: a later ordinary summary reads an earlier share ==\n\n")
cat("Expected (ADR 0010: 'A later summary expression cannot refer to a Parent share created\n",
    "earlier in the same summarize_with_margins() call'): the Package refusal that the\n",
    "same read written as eval(quote(sh)) or sum(sh) gets.\n\n", sep = "")
show("control eval(quote(sh)) on `hidden`:", outcome(summarize_with_margins(
  hidden, s = sum(v), sh = share_of_total(s), z = sum(eval(quote(sh))),
  .grouping = rollup(g)
)))
for (z in c("do.call(sum, list(quote(sh)))", "do.call(\"sum\", rlang::syms(\"sh\"))",
            "do.call(sum, lapply(\"sh\", as.name))", "do.call(sum, list(str2lang(\"sh\")))")) {
  code <- sprintf(
    "summarize_with_margins(hidden, s = sum(v), sh = share_of_total(s), z = %s, .grouping = rollup(g))",
    z
  )
  show(paste0("observed z = ", z, " on `hidden`:"), outcome(eval(parse(text = code))))
}
cat("   (z = 300, 300, 600 are sums of the hidden INPUT column sh = 100, 200, 300;\n",
    "    the visible sh column is the share 0.375, 0.625, 1)\n\n", sep = "")
show("observed z = do.call(sum, list(quote(sh))) on `plain` (no input column sh):",
     outcome(summarize_with_margins(
       plain, s = sum(v), sh = share_of_total(s), z = do.call(sum, list(quote(sh))),
       .grouping = rollup(g)
     )))

cat("\nNative control: plain dplyr, sh an ordinary earlier summary, same do.call():\n")
print(summarise(hidden, s = sum(v), sh = s / 8, z = do.call(sum, list(quote(sh))), .by = g))
cat("   do.call() evaluates the quoted symbol in the data mask, so z is the earlier summary sh.\n\n")

cat("== Direction 2: a share source reads an earlier alias ==\n\n")
d <- data.frame(g = c("a", "a", "b"), x = c(1, 2, 5), y = c(10, 20, 30))
cat("Expected (?share_of_parent, Eligible source summaries: 'self-contained: it may aggregate\n",
    "input columns, but it cannot refer to an earlier summary'): the alias-dependency refusal.\n\n", sep = "")
show("control value = sum(x) + eval(quote(prior)):", outcome(summarize_with_margins(
  d, prior = sum(y), value = sum(x) + eval(quote(prior)), total = share_of_total(value),
  .grouping = rollup(g)
)))
show("observed value = sum(x) + do.call(identity, list(quote(prior))):",
     outcome(summarize_with_margins(
       d, prior = sum(y), value = sum(x) + do.call(identity, list(quote(prior))),
       total = share_of_total(value), .grouping = rollup(g)
     )))
show("observed on a data.table:", outcome(summarize_with_margins(
  data.table::as.data.table(d), prior = sum(y),
  value = sum(x) + do.call(identity, list(quote(prior))),
  total = share_of_total(value), .grouping = rollup(g)
)))
invisible(0)
