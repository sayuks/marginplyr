# Investigated SHA: 0fe5f1e40daa14e2ba2b5246452fa668bc330f80
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# On a generic SQL backend a sorted summary holding a share and a typed-missing Margin label ends in an ORDER BY over the private set identifier, which no operand selects.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressPackageStartupMessages({library(dplyr); library(DBI); library(methods)})
# A DBIConnection subclass that forwards to in-memory DuckDB, so marginplyr and
# dbplyr take their generic ANSI path while a real engine runs the SQL
# (the harness of investigation/2026-11-13-eight-lane-workflow/generic-sql-limited-input-anchor.R).
setClass("GenericConn", contains = "DBIConnection", slots = c(inner = "DBIConnection"))
setMethod("dbSendQuery", c("GenericConn", "character"), function(conn, statement, ...) dbSendQuery(conn@inner, statement, ...))
setMethod("dbGetQuery", c("GenericConn", "character"), function(conn, statement, ...) dbGetQuery(conn@inner, statement, ...))
setMethod("dbExecute", c("GenericConn", "character"), function(conn, statement, ...) dbExecute(conn@inner, statement, ...))
setMethod("dbExistsTable", c("GenericConn", "ANY"), function(conn, name, ...) dbExistsTable(conn@inner, name, ...))
setMethod("dbListFields", c("GenericConn", "ANY"), function(conn, name, ...) dbListFields(conn@inner, name, ...))
setMethod("dbIsValid", "GenericConn", function(dbObj, ...) dbIsValid(dbObj@inner))
setMethod("dbGetInfo", "GenericConn", function(dbObj, ...) list(dbname = "generic", db.version = "1"))
setMethod("dbQuoteIdentifier", c("GenericConn", "character"), function(conn, x, ...) dbQuoteIdentifier(ANSI(), x, ...))
setMethod("dbQuoteIdentifier", c("GenericConn", "SQL"), function(conn, x, ...) x)
setMethod("dbQuoteString", c("GenericConn", "character"), function(conn, x, ...) dbQuoteString(ANSI(), x, ...))
setMethod("dbDataType", "GenericConn", function(dbObj, obj, ...) dbDataType(dbObj@inner, obj, ...))
registerS3method("dbplyr_edition", "GenericConn", function(con) 2L, envir = asNamespace("dbplyr"))
msg <- function(e) { while (!is.null(e$parent)) e <- e$parent; gsub("\\s+", " ", conditionMessage(e)) }
try_collect <- function(x) tryCatch(as.data.frame(collect(x)), error = function(e) paste("<error>", msg(e)))

d <- data.frame(a = c("x", "y", "x", "y", "z", "z"), v = c(1, 2, 3, 4, 5, 6))
duck <- dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
dbWriteTable(duck, "df", d)
con <- new("GenericConn", inner = duck)
t <- tbl(con, "df")

# Independent expectation: plain dplyr at each grain, divided by hand, in the
# documented Margin order (detail rows by a, then the omitted-a row).
detail <- summarise(d, n = sum(v), .by = a) |> arrange(a)
grand <- sum(d$v)
expected <- rbind(detail, data.frame(a = NA_character_, n = grand))
expected$p <- expected$n / grand
cat("expected (per-grain dplyr, hand-divided, Margin order):\n"); print(expected)

f <- function(x, ...) summarize_with_margins(x, n = sum(v, na.rm = TRUE), p = share_of_total(n),
  .grouping = rollup(a), .check_share_source = FALSE, ...)

cat("\nobserved, generic connection, .margin_label = NULL, .sort = 'last':\n")
print(try_collect(f(t, .margin_label = NULL, .sort = "last")))
cat("\nobserved, same with share_of_parent():\n")
print(try_collect(summarize_with_margins(t, n = sum(v, na.rm = TRUE), p = share_of_parent(n),
  .grouping = rollup(a), .check_share_source = FALSE, .margin_label = NULL, .sort = "first")))

cat("\ncontrol: same call, .sort = 'none' (values):\n")
print(try_collect(f(t, .margin_label = NULL, .sort = "none")) |> arrange(is.na(a), a))
cat("\ncontrol: same call, default label 'Total', .sort = 'last':\n")
print(try_collect(f(t, .sort = "last")))
cat("\ncontrol: same call without the share, .margin_label = NULL, .sort = 'last':\n")
print(try_collect(summarize_with_margins(t, n = sum(v, na.rm = TRUE), .grouping = rollup(a), .margin_label = NULL, .sort = "last")))
cat("\ncontrol: same call on the DuckDB connection itself (native path):\n")
print(try_collect(f(tbl(duck, "df"), .margin_label = NULL, .sort = "last")))
cat("\nnative control on the generic connection: per-grain union_all + arrange():\n")
native <- union_all(
  summarise(t, n = sum(v, na.rm = TRUE), .by = a),
  mutate(summarise(t, n = sum(v, na.rm = TRUE)), a = NA_character_) |> select(a, n)
) |> mutate(p = n / sum(n, na.rm = TRUE) * 2) |> arrange(is.na(a), a)
print(try_collect(native))

cat("\nthe tail of the rendered SQL (ORDER BY after the outer UNION ALL):\n")
sql <- strsplit(as.character(dbplyr::sql_render(f(t, .margin_label = NULL, .sort = "last"))), "\n")[[1]]
cat(sql[1:3], "...", sql[(length(sql) - 5):length(sql)], sep = "\n")
dbDisconnect(duck, shutdown = TRUE)
invisible(0)
