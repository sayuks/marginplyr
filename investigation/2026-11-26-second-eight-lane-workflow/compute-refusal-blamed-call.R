# Investigated SHA: 0fe5f1e40daa14e2ba2b5246452fa668bc330f80
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# Package conditions from the package's own compute() methods blame the frame above compute(), or the method, and not the compute() call written.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages({ library(dplyr) })
df <- data.frame(g = c("a","a","b","b"), v = 1:4)
show <- function(label, f) {
  e <- tryCatch(f(), error = function(e) e)
  cat(sprintf("%-46s class=%s\n    call=%s\n    msg=%s\n", label, class(e)[1],
              paste(deparse(conditionCall(e)), collapse = " "),
              substr(gsub("\n", " | ", conditionMessage(e)), 1, 120)))
}
sq <- DBI::dbConnect(RSQLite::SQLite(), ":memory:"); DBI::dbWriteTable(sq, "t", df)
r  <- summarize_with_margins(tbl(sq, "t"), n = n(), .grouping = rollup(g), .sort = "last")
nr <- tbl(sq, "t") |> group_by(g) |> summarise(n = n()) |> arrange(g)   # native control, same backend, ordered summary
w  <- function(x, ...) compute(x, ...)
cat("EXPECTED: conditionCall() is the compute() call the caller wrote (as native: `compute(nr, temporary = NA)`).\n\n")
cat("-- SQLite Margin result (marginplyr_error)\n")
show("compute(r, temporary = NA) at top level",  function() compute(r, temporary = NA))
show("wrapper w(r, temporary = NA)",             function() w(r, temporary = NA))
show("compute(r, name = 'sh', temporary = FALSE) with temp 'sh'", function() {
  DBI::dbExecute(sq, "create temp table sh (a int)"); w(r, name = "sh", temporary = FALSE) })
cat("\n-- native control on the same backend\n")
show("compute(nr, temporary = NA) at top level", function() compute(nr, temporary = NA))
show("wrapper w(nr, temporary = NA)",            function() w(nr, temporary = NA))
cat("\n-- DuckDB expansion result (marginplyr_duckdb_env), same mistake class\n")
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
DBI::dbWriteTable(con, "s", data.frame(g = c("a","b"), v = 1:2, .env = c("x","y")))
q <- expand_with_margins(tbl(con, "s"), .grouping = rollup("g"), .margin_label = NULL, .id = "sid")
show("compute(q, indexes = list('zz')) top level", function() compute(q, indexes = list("zz")))
show("wrapper w(q, indexes = list('zz'))",         function() w(q, indexes = list("zz")))
nq <- tbl(con, "s") |> arrange(g)
show("native compute(nq, indexes = list('zz'))",   function() compute(nq, indexes = list("zz")))

cat("\n-- the same mistake written at the top level of a script (no enclosing function)\n")
e <- tryCatch(compute(r, temporary = NA), error = function(e) e)
cat("marginplyr: conditionCall =", paste(deparse(conditionCall(e)), collapse = " "), "\n")
e <- tryCatch(compute(nr, temporary = NA), error = function(e) e)
cat("native:     conditionCall =", paste(deparse(conditionCall(e)), collapse = " "), "\n")
