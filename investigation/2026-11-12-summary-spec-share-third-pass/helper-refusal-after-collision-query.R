# Investigated SHA: 26846d0127f333250668e928e53b200d04c99ca5
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
suppressPackageStartupMessages({
  pkgload::load_all(".", quiet = TRUE)
  library(dplyr)
})
options(marginplyr.audit_sql = TRUE)
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
x <- copy_to(con, data.frame(g = c("a", "b", "c"), v = 1:3), "t")
probe <- function(label, expr) {
  msg <- tryCatch({ force(expr); "no error" },
                  marginplyr_error = function(e) conditionMessage(e)[1])
  q <- last_sent_queries()
  cat(sprintf("%-26s refusal: %-56s collision queries sent first: %d\n",
              label, substr(sub("\n.*", "", msg), 1, 56),
              sum(q$purpose == "observed_label_collision")))
}
S <- function(...) summarize_with_margins(x, ..., .grouping = rollup(g), .check_margin_label = TRUE)
# Controls: refused BEFORE the collision query.
probe("share_of_total(s) * 100",  S(s = sum(v), t = share_of_total(s) * 100))
probe("cur_group_id()",           S(s = cur_group_id()))
probe("g = 1 (overwrites a key)", S(g = 1))
# Refused only AFTER it, though decidable from the call or the resolved plan.
probe("grouping_bit()",           S(s = grouping_bit()))
probe("grouping_bit(g, g)",       S(s = grouping_bit(g, g)))
probe("grouping_bit(g + 1)",      S(s = grouping_bit(g + 1)))
probe("grouping_bit(v)",          S(s = grouping_bit(v)))
probe("grouping_id(g, nope)",     S(s = grouping_id(g, nope)))
cat("\nThe query recorded before `grouping_bit()` was refused:\n")
invisible(tryCatch(S(s = grouping_bit()), error = function(e) NULL))
print(as.data.frame(last_sent_queries()))
DBI::dbWriteTable(con, "u", data.frame(g = c("a", "Total"), v = 1:2))
cat("\nWith a colliding value present, the call error is not the one reported:\n")
cat(tryCatch(summarize_with_margins(tbl(con, "u"), s = grouping_bit(), .grouping = rollup(g),
                                    .check_margin_label = TRUE),
             error = function(e) conditionMessage(e)), "\n")
DBI::dbDisconnect(con)
