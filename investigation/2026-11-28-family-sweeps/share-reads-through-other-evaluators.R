# Investigated SHA: 1680498dbfc37296d0d20ea4352606c48441a16e
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# A later summary reads an earlier share's name through another package's evaluator (#1845).
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
d <- tibble(g = c("a", "a", "b"), v = c(1, 2, 4), sh = c(100, 200, 300))
run <- function(label, expr) {
  out <- tryCatch(suppressWarnings({
    r <- as.data.frame(eval(expr))
    paste("z =", paste(format(unlist(r$z)), collapse = ", "))
  }), marginplyr_error = function(e) {
    paste("Package refusal:", substr(gsub("\\s+", " ", conditionMessage(e)), 1, 70))
  }, error = function(e) {
    paste("Error:", substr(gsub("\\s+", " ", conditionMessage(e)), 1, 100))
  })
  cat(sprintf("%-44s %s\n", label, out))
}
calls <- c(
  'sum(sh)', 'cli::format_error("{sh}")', 'purrr::invoke_map(get, "sh")[[1]]',
  'tibble::tibble_("sum(sh)")[[1]]', 'glue::identity_transformer("sum(sh)")',
  'tidyr::crossing_("sum(sh)")[[1]]'
)
verb <- "summarize_with_margins(%s, s = sum(v), sh = share_of_total(s), z = %s, .grouping = rollup(g))"
cat("Expected: no z calculated from the input column sh (its sums are 300, 300, 600).\n")
cat("\n-- local data frame\n")
for (text in calls) run(text, parse(text = sprintf(verb, "d", text))[[1]])
if (requireNamespace("dtplyr", quietly = TRUE)) {
  cat("\n-- dtplyr\n")
  dt <- dtplyr::lazy_dt(data.table::as.data.table(d))
  for (text in calls) {
    run(text, parse(text = sprintf(paste0("dplyr::collect(", verb, ")"), "dt", text))[[1]])
  }
}
cat("\n-- native control: plain dplyr, where sh is an earlier summary\n")
for (text in calls) {
  run(text, parse(text = sprintf("summarise(group_by(d, g), s = sum(v), sh = s / 7, z = %s)", text))[[1]])
}
cat("\n-- plain dplyr with a marker bound to the share's own name\n")
token <- new.env(parent = emptyenv())
for (text in calls) {
  run(text, parse(text = sprintf("summarise(group_by(d, g), s = sum(v), sh = list(token), z = %s)", text))[[1]])
}
