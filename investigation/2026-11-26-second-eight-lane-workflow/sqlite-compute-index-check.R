# Investigated SHA: 0fe5f1e40daa14e2ba2b5246452fa668bc330f80
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# On SQLite's delayed-publication compute() path a missing index column is refused by a bare rlang::abort() that blames an internal call.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages({ library(dplyr) })
df <- data.frame(g = c("a","a","b","b"), v = 1:4)
show <- function(label, expr) {
  e <- tryCatch(expr, error = function(e) e)
  cat(sprintf("%-44s inherits marginplyr_error=%s\n    call=%s\n    msg=%s\n", label, inherits(e, "marginplyr_error"),
              substr(paste(deparse(conditionCall(e)), collapse = " "), 1, 90),
              substr(gsub("\n", " | ", conditionMessage(e)), 1, 130)))
}
sq <- DBI::dbConnect(RSQLite::SQLite(), ":memory:"); DBI::dbWriteTable(sq, "t", df)
mk <- function() summarize_with_margins(tbl(sq, "t"), n = n(), .grouping = rollup(g), .sort = "last")
nr <- tbl(sq, "t") |> group_by(g) |> summarise(n = n()) |> arrange(g)    # native control, same backend
cat("EXPECTED: one refusal, a marginplyr_error (or at least dbplyr's own), blamed on the compute() call written.\n\n")
show("compute(r, name = 'fresh', temporary = TRUE, indexes = 'zz')", compute(mk(), name = "fresh", temporary = TRUE, indexes = list("zz")))
show("compute(r, name = 't', temporary = TRUE, indexes = 'zz')  [name resolves to the source table]",
     compute(mk(), name = "t", temporary = TRUE, indexes = list("zz")))
show("compute(r, name = 't', temporary = TRUE, unique_indexes = 'zz')",
     compute(mk(), name = "t", temporary = TRUE, unique_indexes = list("zz")))
show("compute(r, name = c('a','b'))", compute(mk(), name = c("a", "b")))
cat("\n-- native control (same backend, ordered summary)\n")
show("native compute(nr, name='fresh2', temporary=TRUE, indexes='zz')", compute(nr, name = "fresh2", temporary = TRUE, indexes = list("zz")))
show("native compute(nr, name='t', temporary=TRUE, indexes='zz')", compute(nr, name = "t", temporary = TRUE, indexes = list("zz")))
show("native compute(nr, name = c('a','b'))", compute(nr, name = c("a", "b")))
