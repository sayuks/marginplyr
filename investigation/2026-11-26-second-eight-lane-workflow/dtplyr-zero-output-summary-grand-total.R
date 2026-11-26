# Investigated SHA: 0fe5f1e40daa14e2ba2b5246452fa668bc330f80
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# On a dtplyr input, a Margin summary whose every summary expands to no output loses the row of the Grand total set.
# The last block replays one candidate answer in memory; nothing in the checkout is edited.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages({library(dplyr); library(dtplyr)})
d <- data.frame(g = c("a", "a", "b"), h = c("p", "q", "p"), x = c(1, 2, 5))
rows <- function(expr) {
  out <- tryCatch(as.data.frame(collect(expr)), error = function(e) NULL)
  if (is.null(out)) return("<error>")
  paste0(nrow(out), " rows: ", paste(apply(out, 1, paste, collapse = "/"), collapse = " | "))
}
report <- function() {
  for (kind in c("data.frame", "data.table", "dtplyr")) {
    input <- switch(kind, data.frame = d, data.table = data.table::as.data.table(d), dtplyr = lazy_dt(d))
    cat(sprintf("%-10s across(starts_with('zzz'), sum), rollup(g)   : %s\n", kind,
                rows(summarize_with_margins(input, across(starts_with("zzz"), sum), .grouping = rollup(g)))))
    cat(sprintf("%-10s across(where(is.logical), sum), .id          : %s\n", kind,
                rows(summarize_with_margins(input, across(where(is.logical), sum), .grouping = rollup(g), .id = "set"))))
    cat(sprintf("%-10s across(x, list()), rollup(g, h)              : %s\n", kind,
                rows(summarize_with_margins(input, across(x, list()), .grouping = rollup(g, h)))))
  }
  cat("dtplyr     no summary at all                            :", rows(summarize_with_margins(lazy_dt(d), .grouping = rollup(g))), "\n")
  cat("dtplyr     the empty across beside n = n()              :",
      rows(summarize_with_margins(lazy_dt(d), across(starts_with("zzz"), sum), n = n(), .grouping = rollup(g))), "\n")
  cat("dtplyr     the empty across, .margin_label = NULL       :",
      rows(summarize_with_margins(lazy_dt(d), across(starts_with("zzz"), sum), .grouping = rollup(g), .margin_label = NULL)), "\n")
  cat("dtplyr     no summary, no dimension (pinned boundary)   :", rows(summarize_with_margins(lazy_dt(d))), "\n")
}
report()
empty <- summarise(lazy_dt(d), across(starts_with("zzz"), sum))
cat("native dtplyr, ungrouped empty summarise                 :", nrow(collect(empty)), "rows\n")
cat("native dtplyr, the same then mutate(g = \"Total\")         :", nrow(collect(mutate(empty, g = "Total"))), "rows\n")
ns <- asNamespace("marginplyr")
original <- get("summarize_margin_union", ns)
target <- quote(length(branch_dots) == 0L || backend$is_sql)
swap <- function(e) {
  if (!is.call(e)) return(e)
  if (identical(e, target)) return(quote(length(branch_dots) == 0L || backend$is_sql || identical(backend$kind, "dtplyr")))
  as.call(lapply(e, swap))
}
patched <- original
body(patched) <- swap(body(original))
stopifnot(!identical(body(patched), body(original)))
unlockBinding("summarize_margin_union", ns)
assign("summarize_margin_union", patched, ns)
cat("--- with the placeholder also added for a dtplyr branch of the empty set (in memory)\n")
report()
