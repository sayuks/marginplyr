# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# Second round: a later summary selecting an earlier share by a string gets tidyselect's missing-column error (#1832).
# An ordinary summary selecting an earlier share by a string (pick("sh"), across("sh"), c_across("sh")) gets tidyselect's 'column does not exist' in place of the earlier-share refusal
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
cat("Expected: 'Ordinary summaries cannot use an earlier Total share (`sh`)' as for the symbol spellings.\n\n")
forward('sum(dplyr::pick(sh)$sh)', data = "plain")
forward('sum(dplyr::across(sh, sum))', data = "plain")
for (code in c('sum(dplyr::pick("sh")$sh)', 'sum(dplyr::across("sh", sum))',
               'sum(dplyr::pick(dplyr::all_of("sh"))[[1]])', 'sum(dplyr::pick(c("sh"))[[1]])',
               'sum(dplyr::c_across("sh"))'))
  forward(code, data = "plain")
cat("\n-- with a same-named input column --\n")
forward('sum(dplyr::pick("sh")$sh)')
cat("\n-- an earlier ordinary summary by string is accepted --\n")
run("marginplyr", quote(summarize_with_margins(plain, s = sum(v), z = sum(dplyr::pick("s")$s), .grouping = rollup(g))))
cat("\n-- the share-source direction is refused with the package condition --\n")
reverse('sum(dplyr::pick("zz"))')
