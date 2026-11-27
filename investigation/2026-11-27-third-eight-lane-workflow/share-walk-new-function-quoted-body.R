# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# Second round: a function built in place by rlang::new_function() or as.function(list(quote())) is not read (#1831).
# A function built from quoted language by rlang::new_function() or as.function(list(quote(...))) reads a share or an earlier alias unseen
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
cat("Expected: refused as the as.function(alist(...)) and body<- twins are.\n\n")
forward('sum(as.function(alist(sh))())')
forward('{ f <- function() 0; body(f) <- quote(sh); sum(f()) }')
for (code in c('sum(as.function(list(quote(sh)))())',
               'sum(rlang::new_function(NULL, quote(sh))())',
               'sum(rlang::new_function(rlang::pairlist2(), quote(sh), rlang::current_env())())',
               '{ f <- rlang::new_function(NULL, quote(sum(sh))); f() }',
               'sum(rlang::as_function(rlang::new_formula(NULL, quote(sh)))())'))
  forward(code)
cat("\n-- reverse --\n")
reverse('as.function(alist(zz))()')
reverse('as.function(list(quote(zz)))()')
reverse('rlang::new_function(NULL, quote(zz))()')
cat("\n-- legal --\n")
reverse('(function(zz) rlang::new_function(NULL, quote(zz))())(2)')
reverse('rlang::new_function(NULL, quote(sum(v)))()')

cat("\n-- data.table input --\n")
if (requireNamespace("data.table", quietly = TRUE)) { hidden_dt <- data.table::as.data.table(hidden); forward('sum(rlang::new_function(NULL, quote(sh))())', data = "hidden_dt") }
