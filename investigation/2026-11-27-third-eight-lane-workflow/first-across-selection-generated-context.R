# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# Second round: a failed first across() selection carries a generated dplyr::all_of(resolve()) layer when a share reads its outputs (#1828).
# Candidate 1: a selection failure in a first unnamed across() whose outputs a
# later share reads is reported with a layer quoting marginplyr's generated
# `dplyr::all_of(resolve())`, and its cause blamed on the verb, not `across()`.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressPackageStartupMessages(library(dplyr))
d <- data.frame(g = c("a", "a", "b"), v = c(1, 2, 3), w = c(2, 2, 2))
pick_cols <- function() rlang::abort("The column registry is offline.", class = "registry_offline")

layers <- function(expr) {
  e <- tryCatch({ force(expr); NULL }, error = function(e) e)
  if (is.null(e)) return("returned")
  out <- character()
  while (inherits(e, "condition")) {
    first <- strsplit(cli::ansi_strip(conditionMessage(e)), "\n")[[1]][1]
    call <- conditionCall(e)
    out <- c(out, sprintf("%s | %s | call: %s", class(e)[1], first,
                          if (is.null(call)) "NULL" else deparse(call[[1]])))
    e <- e$parent
  }
  out
}
show <- function(label, x) cat(label, "\n", paste0("    ", x, collapse = "\n"), "\n", sep = "")

for (input in list(df = d, dt = data.table::as.data.table(d))) {
  cat("######## input:", class(input)[1], "\n")
  cat("== A. typo in a column name\n")
  show("native summarise():", layers(summarise(input, across(c(v, nope), sum), .by = g)))
  show("marginplyr, no share (control):",
       layers(summarize_with_margins(input, across(c(v, nope), sum), .grouping = rollup(g))))
  show("marginplyr, share of an across output (observed):",
       layers(summarize_with_margins(input, across(c(v, nope), sum), p = share_of_total(v),
                                     .grouping = rollup(g))))
  cat("== B. a caller helper's classed error inside the selection\n")
  show("native summarise():", layers(summarise(input, across(all_of(pick_cols()), sum), .by = g)))
  show("marginplyr, no share (control):",
       layers(summarize_with_margins(input, across(all_of(pick_cols()), sum), .grouping = rollup(g))))
  show("marginplyr, share of an across output (observed):",
       layers(summarize_with_margins(input, across(all_of(pick_cols()), sum), p = share_of_total(v),
                                     .grouping = rollup(g))))
}
cat("\nExpected: the share call reports the layers the no-share call and native dplyr report;",
    "no layer quotes `dplyr::all_of(resolve())`.\n")
cat("\nRendered message of A (observed):\n")
print(tryCatch(summarize_with_margins(d, across(c(v, nope), sum), p = share_of_total(v),
                                      .grouping = rollup(g)), error = function(e) e), backtrace = FALSE)
