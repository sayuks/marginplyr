# Investigated SHA: 0fe5f1e40daa14e2ba2b5246452fa668bc330f80
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# On DuckDB a summary-level refusal decidable from the call arrives after the catalog query and the typed zero-row read.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages({ library(dplyr) })
df <- data.frame(g = c("a","a","b","b"), h = c("x","y","x","y"), v = 1:4)
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE)); DBI::dbWriteTable(con, "t", df); x <- tbl(con, "t")
sq  <- DBI::dbConnect(RSQLite::SQLite(), ":memory:"); DBI::dbWriteTable(sq, "t", df); y <- tbl(sq, "t")
.lg <- new.env(); .lg$q <- character()
suppressMessages({
  trace("rapi_prepare", tracer = quote(.lg$q <- c(.lg$q, as.character(query))), where = asNamespace("duckdb"), print = FALSE)
  trace("result_create", tracer = quote(.lg$q <- c(.lg$q, as.character(sql))), where = asNamespace("RSQLite"), print = FALSE)
})
probe <- function(label, tbl, f) {
  .lg$q <- character()
  r <- tryCatch({ f(tbl); "OK" }, error = function(e) paste0(class(e)[1], ": ", substr(gsub("\n", " | ", conditionMessage(e)), 1, 70)))
  cat(sprintf("%-8s %-34s queries=%d  %s\n", if (identical(tbl, x)) "duckdb" else "sqlite", label, length(.lg$q), r))
  if (length(.lg$q)) cat("           first query:", substr(gsub("\\s+", " ", .lg$q[[length(.lg$q)]]), 1, 80), "\n")
}
S_ <- summarize_with_margins
cases <- list(
  "grouping_bit(g, h) arity"      = function(t) S_(t, n = grouping_bit(g, h), .grouping = rollup(g)),
  "grouping_bit(g + 1) not bare"  = function(t) S_(t, n = grouping_bit(g + 1), .grouping = rollup(g)),
  "grouping_id(g, g) duplicate"   = function(t) S_(t, n = grouping_id(g, g), .grouping = rollup(g)),
  "summary named like dimension"  = function(t) S_(t, g = sum(v), .grouping = rollup(g, h)),
  "share_of_parent(nope)"         = function(t) S_(t, s = share_of_parent(nope), .grouping = rollup(g)),
  "-- controls (refused with no query)" = function(t) S_(t, n = n(), .grouping = rollup()),
  "share_of_parent() grammar"     = function(t) S_(t, s = share_of_parent(), .grouping = rollup(g)),
  ".sort = 'bad'"                 = function(t) S_(t, n = n(), .grouping = rollup(g), .sort = "bad")
)
for (nm in names(cases)) { probe(nm, x, cases[[nm]]); probe(nm, y, cases[[nm]]) }
