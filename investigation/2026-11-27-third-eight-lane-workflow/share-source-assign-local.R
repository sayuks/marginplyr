# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# Second round: a share source reading a local it bound with assign() is refused as an earlier-alias dependency (#1833).
# A local bound by assign()/delayedAssign()/makeActiveBinding()/env_bind()/env_poke()/local_bindings() is read as an earlier summary alias, so a legal share source is refused
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
cat("Expected: accepted with src = 2, as the `<-` spelling is (plain dplyr returns 2).\n\n")
reverse('{ zz <- 2; zz }')
for (code in c('{ assign("zz", 2); zz }', '{ delayedAssign("zz", 2); zz }',
               '{ makeActiveBinding("zz", function() 2, environment()); zz }',
               '{ rlang::env_bind(environment(), zz = 2); zz }',
               '{ rlang::env_poke(environment(), "zz", 2); zz }',
               '{ rlang::local_bindings(zz = 2); zz }',
               '{ list2env(list(zz = 2), environment()); zz }',
               '(function() { assign("zz", 2); zz })()',
               '(function() { zz <- 2; zz })()'))
  reverse(code)
