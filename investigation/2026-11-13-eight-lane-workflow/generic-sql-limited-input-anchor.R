# Investigated SHA: 6165f4768400a28520cc8977e056da0db49a32ed
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# On a generic SQL backend a summary over a row-limited input puts the input's ORDER BY and LIMIT on the first UNION ALL operand, which the engine refuses.
# The connection is a DBIConnection subclass that forwards to an in-memory DuckDB engine, so the package takes its portable UNION ALL path and the SQL runs.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
suppressMessages(library(DBI))
library(methods)
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
d <- data.frame(k = c(1L, 1L, 2L, 2L, 2L), a = c("x", "y", "x", "y", "z"), v = c(1, 2, 3, 4, 5))
duck <- dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
dbWriteTable(duck, "df", d)
con <- new("GenericConn", inner = duck)
t <- tbl(con, "df")
try_it <- function(expr) tryCatch(
  { out <- collect(expr); paste0(nrow(out), " rows: ", paste(sprintf("%s/%s=%s", out$k, out$a, out$n), collapse = " ")) },
  error = function(e) {
    while (!is.null(e$parent)) e <- e$parent
    paste0("<error> ", substr(gsub("\\s+", " ", conditionMessage(e)), 1, 60))
  }
)
first_operand <- function(query) gsub("\\s+", " ", sub("(?s)UNION ALL.*", "UNION ALL", as.character(dbplyr::sql_render(query)), perl = TRUE))
f <- function(input, ...) summarize_with_margins(input, n = sum(v, na.rm = TRUE), .by = k, .grouping = rollup(a), ...)
top <- head(arrange(t, a), 3)
cat("class the package sees:", class(con)[1], "\n")
cat("expected by hand over the three rows read (1/x, 2/x, 1/y): 1/x=1 2/x=3 1/y=2 1/Total=3 2/Total=3\n")
cat("native per-set summaries, union_all:", try_it(union_all(
  summarise(top, n = sum(v, na.rm = TRUE), .by = c(k, a)),
  mutate(summarise(top, n = sum(v, na.rm = TRUE), .by = k), a = "Total") |> select(k, a, n)
)), "\n")
cat("summarize over arrange(a) |> head(3):", try_it(f(top)), "\n")
cat("  first operand:", first_operand(f(top)), "\n")
cat("summarize over head(3)              :", try_it(f(head(t, 3))), "\n")
cat("summarize, .sort = 'last'           :", try_it(f(top, .sort = "last")), "\n")
cat("control: one grouping set           :", try_it(summarize_with_margins(top, n = sum(v, na.rm = TRUE), .by = c(k, a))), "\n")
cat("control: computed input             :", try_it(f(compute(top))), "\n")
cat("control: unlimited arrange(a)       :", try_it(f(arrange(t, a))), "\n")
cat("control: expand_with_margins        :", tryCatch(paste(nrow(collect(expand_with_margins(top, .by = k, .grouping = rollup(a)))), "rows"), error = function(e) "<error>"), "\n")
for (simulated in c("simulate_mysql", "simulate_mssql", "simulate_oracle", "simulate_postgres")) {
  lazy <- head(arrange(dbplyr::lazy_frame(k = 1L, a = "x", v = 1, con = getExportedValue("dbplyr", simulated)(), .name = "df"), a), 3)
  cat(sprintf("%-17s first operand: %s\n", simulated, first_operand(suppressWarnings(f(lazy)))))
}
dbDisconnect(duck, shutdown = TRUE)
