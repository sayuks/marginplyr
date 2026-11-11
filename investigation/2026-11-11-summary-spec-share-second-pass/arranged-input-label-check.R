# Investigated SHA: 22043a461c5c48662ff54ea0a1238f00ac316059
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# .check_margin_label = TRUE fails on an arranged Arrow or DuckDB input that the
# same Margin summary accepts without the check.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr)); options(marginplyr.audit_sql = TRUE)
d <- data.frame(g = c("a", "a", "b"), v = c(3L, 1L, 2L))
p <- function(tag, e) cat(sprintf("%-34s %s\n", tag, tryCatch({ r <- as.data.frame(e); paste(nrow(r), "rows:", paste(r$g, r$s, collapse = " | ")) },
  error = function(c) paste("ERROR:", gsub("\n", " ", substr(conditionMessage(c), 1, 150))))))
con <- DBI::dbConnect(duckdb::duckdb(dbdir = ":memory:", shared_home = FALSE)); DBI::dbWriteTable(con, "src", d)
scon <- DBI::dbConnect(RSQLite::SQLite(), ":memory:"); DBI::dbWriteTable(scon, "src", d)
inputs <- list(arrow = arrow::arrow_table(d), duckdb = tbl(con, "src"), sqlite = tbl(scon, "src"),
               dtplyr = dtplyr::lazy_dt(data.table::as.data.table(d)), local = d)
for (n in names(inputs)) {
  q <- arrange(inputs[[n]], v)                      # the caller ordered the detail rows
  p(paste(n, "check FALSE"), collect(summarize_with_margins(q, s = sum(v), .grouping = rollup(g), .check_margin_label = FALSE)))
  p(paste(n, "check TRUE"), collect(summarize_with_margins(q, s = sum(v), .grouping = rollup(g), .check_margin_label = TRUE)))
  p(paste(n, "check TRUE, unarranged"), collect(summarize_with_margins(inputs[[n]], s = sum(v), .grouping = rollup(g), .check_margin_label = TRUE)))
}
# where the error is raised: construction, i.e. the label-check query itself
r <- tryCatch(summarize_with_margins(arrange(inputs$duckdb, v), s = sum(v), .grouping = rollup(g), .check_margin_label = TRUE), error = function(c) c)
cat("raised at construction:", inherits(r, "error"), " marginplyr_error:", inherits(r, "marginplyr_error"), "\n")
q <- last_sent_queries(); cat(q$purpose, "\n", as.character(q$sql), "\n")
DBI::dbDisconnect(con, shutdown = TRUE); DBI::dbDisconnect(scon)
