# Investigated SHA: 26846d0127f333250668e928e53b200d04c99ca5
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# E2: on live SQLite, a sorted Margin summary with a text Margin label and
# neither `.id` nor a share loses the declared type of its fixed `.by` key.
suppressPackageStartupMessages({library(dplyr); library(dbplyr)})
pkgload::load_all(".", quiet = TRUE)
d <- function(v) paste(class(v), collapse = "/")

cat("--- (a) extended_types = TRUE, nonempty table, DATE fixed key ---\n")
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:", extended_types = TRUE)
invisible(DBI::dbExecute(con, "CREATE TABLE t (dt DATE, g TEXT, x REAL)"))
invisible(DBI::dbExecute(con, "INSERT INTO t VALUES ('2020-01-01','a',10),('2021-06-01','b',20),('2020-01-01','b',40)"))
src <- tbl(con, "t")
cat("native summarise by dt      :", d(collect(summarise(group_by(src, dt), s = sum(x, na.rm = TRUE)))$dt), "\n")
v <- function(...) d(collect(summarize_with_margins(src, s = sum(x, na.rm = TRUE), .by = dt, .grouping = rollup(g), ...))$dt)
cat("label 'Total', .sort none   :", v(), "\n")
cat("label NULL,    .sort last   :", v(.margin_label = NULL, .sort = "last"), "\n")
cat("label 'Total', .sort last, .id:", v(.sort = "last", .id = "gid"), "\n")
cat("label 'Total', .sort last   :", v(.sort = "last"), "   <-- expected Date\n")
cat("label 'Total', .sort first  :", v(.sort = "first"), "   <-- expected Date\n")
q <- summarize_with_margins(src, s = sum(x, na.rm = TRUE), .by = dt, .grouping = rollup(g), .sort = "last")
cat("class of the lazy result:", class(q)[1], "\n"); print(collect(q))
cat("local data frame result:\n")
print(summarize_with_margins(collect(src), s = sum(x, na.rm = TRUE), .by = dt, .grouping = rollup(g), .sort = "last"))

cat("--- (b) default connection, zero-row and all-missing tables, INTEGER / TEXT / REAL fixed key ---\n")
con2 <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
invisible(DBI::dbExecute(con2, "CREATE TABLE t0 (ki INTEGER, kc TEXT, kr REAL, g TEXT, x REAL)"))
invisible(DBI::dbExecute(con2, "CREATE TABLE tna (ki INTEGER, kc TEXT, kr REAL, g TEXT, x REAL)"))
invisible(DBI::dbExecute(con2, "INSERT INTO tna VALUES (NULL, NULL, NULL, 'a', 1)"))
for (tb in c("t0", "tna")) for (k in c("ki", "kc", "kr")) {
  s2 <- tbl(con2, tb)
  f <- function(...) d(collect(summarize_with_margins(s2, s = sum(x, na.rm = TRUE), .by = all_of(k), .grouping = rollup(g), ...))[[k]])
  cat(sprintf("%-3s %-2s native=%-9s unsorted=%-9s sorted+NULL=%-9s sorted+.id=%-9s sorted+'Total'=%s\n", tb, k,
      d(collect(summarise(group_by(s2, .data[[k]]), s = sum(x, na.rm = TRUE)))[[k]]), f(), f(.margin_label = NULL, .sort = "last"), f(.sort = "last", .id = "gid"), f(.sort = "last")))
}
