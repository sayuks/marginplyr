# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# Second round: rlang::invoke() is not read as do.call() is; folded into #1820.
# rlang::invoke(), the deprecated do.call() twin, is not read as a deferred call (neighbour of #1820's exec())
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
cat("Expected: refused as do.call(\"get\", list(...)) is.\n\n")
forward('sum(do.call("get", list("sh")))')
forward('sum(rlang::invoke("get", list("sh")))')
forward('sum(rlang::invoke(get, list("sh")))')
forward('sum(rlang::invoke("eval", list(quote(sh))))')
reverse('do.call("get", list("zz"))')
reverse('rlang::invoke("get", list("zz"))')
reverse('rlang::invoke(get, list("zz"))')
reverse('rlang::invoke("sum", list(v))')

cat("\n-- data.table input --\n")
if (requireNamespace("data.table", quietly = TRUE)) { hidden_dt <- data.table::as.data.table(hidden); forward('sum(rlang::invoke("get", list("sh")))', data = "hidden_dt") }
