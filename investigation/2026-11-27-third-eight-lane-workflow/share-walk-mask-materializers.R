# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# Second round: as.list.environment(), lapply() and eapply() over the data mask are not read as as.list() is (#1825).
# The mask environment materialized by a callee other than base as.list() (as.list.environment(), lapply()/sapply()/Map() over it) is not read as the mask
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressPackageStartupMessages(library(dplyr))
hidden <- data.frame(g = c("a", "a", "b"), v = c(1, 2, 5), sh = c(100, 200, 300))
plain <- data.frame(g = c("a", "a", "b"), v = c(1, 2, 5))
d <- data.frame(g = c("a", "a", "b"), v = c(1, 2, 4))
msg <- function(e) gsub("\\s+", " ", conditionMessage(e))
run <- function(label, expr) {
  out <- tryCatch({
    x <- eval(expr, parent.frame())
    keep <- intersect(c("g", "sh", "z", "zz", "src", "p", "lbl"), names(x))
    paste("value:", paste(capture.output(print(as.data.frame(x)[keep], row.names = FALSE)), collapse = " | "))
  }, error = function(e) paste0("error [", paste(class(e)[1:2], collapse = "/"), "]: ", substr(msg(e), 1, 160)))
  cat(sprintf("  %-10s %s\n", label, out))
}
forward <- function(code, data = "hidden") {
  cat("forward z =", code, "on", data, "\n")
  run("marginplyr", str2lang(sprintf("summarize_with_margins(%s, s = sum(v), sh = share_of_total(s), z = %s, .grouping = rollup(g))", data, code)))
  run("native", str2lang(sprintf("summarise(group_by(%s, g), s = sum(v), sh = s / sum(%s$v), z = %s)", data, data, code)))
}
reverse <- function(code) {
  cat("reverse src =", code, "\n")
  run("marginplyr", str2lang(sprintf("summarize_with_margins(d, zz = sum(v) * 100, src = %s, p = share_of_parent(src), .grouping = rollup(g), .margin_label = NULL)", code)))
  run("native", str2lang(sprintf("summarise(group_by(d, g), zz = sum(v) * 100, src = %s)", code)))
}
cat("Expected: refused as the base as.list() twin is.\n\n")
forward('sum(as.list(parent.env(environment()))$sh)')
forward('sum(as.list.environment(parent.env(environment()))$sh)')
forward('sum(base::as.list.environment(parent.env(environment()))$sh)')
forward('sum(lapply(parent.env(environment()), identity)$sh)')
forward('sum(as.list(rlang::env_clone(parent.env(environment())))$sh)')
cat("\n-- reverse --\n")
reverse('as.list(parent.env(environment()))$zz')
reverse('as.list.environment(parent.env(environment()))$zz')
reverse('lapply(parent.env(environment()), identity)$zz')
cat("\n-- legal: a local binding in the current frame --\n")
reverse('{ zz <- 2; as.list(environment())$zz }')
reverse('{ zz <- 2; as.list.environment(environment())$zz }')

cat("\n-- data.table input --\n")
if (requireNamespace("data.table", quietly = TRUE)) { hidden_dt <- data.table::as.data.table(hidden); forward('sum(as.list.environment(parent.env(environment()))$sh)', data = "hidden_dt") }
