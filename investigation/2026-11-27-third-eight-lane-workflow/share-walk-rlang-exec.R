# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# rlang::exec() is not read as do.call() is by the share dependency walk (#1820).
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
d <- tibble(g = c("a", "a", "b"), v = c(1, 2, 4))
run <- function(src) {
  call <- parse(text = sprintf(paste0(
    "summarize_with_margins(d, zz = sum(v) * 100, src = %s, p = share_of_parent(src), ",
    ".grouping = rollup(g), .margin_label = NULL)"), src))[[1]]
  tryCatch({ x <- eval(call); sprintf("ACCEPTED src = %s ; p = %s", paste(x$src, collapse = ","), paste(round(x$p, 3), collapse = ",")) },
           marginplyr_error = function(e) paste("Package refusal:", substr(gsub("\\s+", " ", conditionMessage(e)), 1, 95)),
           error = function(e) paste("External error:", substr(gsub("\\s+", " ", conditionMessage(e)), 1, 70)))
}
cat("Contract (?share_of_parent, Eligible source summaries): a source summary \"cannot refer to an earlier summary\".\n")
cat("Expected: the source below reads the earlier summary zz, so it is a Package refusal.\n\n")
for (src in c('rlang::exec("get", "zz")', 'do.call("get", list("zz"))', 'get("zz")')) cat(sprintf("%-34s %s\n", src, run(src)))
cat("\nNative control (plain dplyr, no marginplyr): the same source reads the earlier alias zz and evaluates:\n")
x <- summarise(group_by(d, g), zz = sum(v) * 100, src = rlang::exec('get', 'zz'))
cat("  src =", paste(x$src, collapse = ","), "(= zz)\n")
