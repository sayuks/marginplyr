# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# count() and summarise() after a sorted lazy result are refused by the database for some result shapes (#1816); PostgreSQL rows appear only where the investigation's cluster is reachable.
# Candidate 2: a later summarise()/count() on a sorted lazy Margin summary fails
# in the database for a typed-missing Margin label (and for a share with .by),
# while the same verbs work on the text-label sorted result and on a native
# arrange()d summary of the same connection.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
df <- data.frame(a = c("x", "y", "x", "y", "z", "z"), k = c("K1", "K1", "K1", "K2", "K2", "K2"), v = c(1, 2, 3, 4, 5, 6))
st <- function(label, expr) {
  r <- tryCatch(suppressWarnings(collect(expr)), error = function(e) paste("ERROR:", substr(gsub("\n", " ", conditionMessage(e)), 1, 150)))
  cat(sprintf("  %-58s ", label)); if (is.character(r)) cat(r, "\n") else cat("ok:", paste(names(r), unlist(r[1, ]), sep = "=", collapse = " "), if (nrow(r) > 1) paste0("(", nrow(r), " rows)"), "\n")
}
run <- function(con, tname) {
  t <- tbl(con, tname)
  cat("expected: count n = 4 rows (x, y, z, margin); sum(n) = 42 (21 detail + 21 margin)\n")
  q_null <- summarize_with_margins(t, n = sum(v, na.rm = TRUE), .grouping = rollup(a), .margin_label = NULL, .sort = "last")
  q_na <- summarize_with_margins(t, n = sum(v, na.rm = TRUE), .grouping = rollup(a), .margin_label = NA_character_, .sort = "first")
  q_txt <- summarize_with_margins(t, n = sum(v, na.rm = TRUE), .grouping = rollup(a), .sort = "last")
  q_none <- summarize_with_margins(t, n = sum(v, na.rm = TRUE), .grouping = rollup(a), .margin_label = NULL)
  q_shby <- summarize_with_margins(t, n = sum(v, na.rm = TRUE), p = share_of_total(n), .grouping = rollup(a), .by = k, .sort = "last",
                                   .check_share_source = !inherits(con, "SQLiteConnection"))
  st("collect(NULL label, .sort = 'last')", q_null)
  st("count(NULL label, .sort = 'last')", count(q_null))
  st("summarise(sum(n)) (NULL label, .sort = 'last')", summarise(q_null, s = sum(n, na.rm = TRUE)))
  st("group_by(a) |> summarise() (NULL label, .sort = 'last')", summarise(group_by(q_null, a), s = sum(n, na.rm = TRUE)))
  st("count(NA_character_ label, .sort = 'first')", count(q_na))
  st("count(share + .by, text label, .sort = 'last')", count(q_shby))
  st("control: count(text label, .sort = 'last')", count(q_txt))
  st("control: count(NULL label, .sort = 'none')", count(q_none))
  nat <- t |> group_by(a) |> summarise(n = sum(v, na.rm = TRUE), h = 1L) |> ungroup()
  st("native: summarise |> arrange(h, a) |> count()", nat |> arrange(h, a) |> count())
  st("native: summarise |> arrange(h, a) |> select(a, n) |> count()", nat |> arrange(h, a) |> select(a, n) |> count())
  st("native: ... |> select(a, n) |> summarise(sum(n))", nat |> arrange(h, a) |> select(a, n) |> summarise(s = sum(n, na.rm = TRUE)))
  st("counter-control: tbl |> arrange(v) |> select(-v) |> count()", t |> arrange(v) |> select(-v) |> count())
  cat("  rendered count(NULL label):\n"); cat(paste0("    ", strsplit(as.character(suppressWarnings(dbplyr::sql_render(count(q_null)))), "\n")[[1]]), sep = "\n")
}
cat("== DuckDB", as.character(packageVersion("duckdb")), "\n")
duck <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), ":memory:")
DBI::dbWriteTable(duck, "df", df); run(duck, "df"); DBI::dbDisconnect(duck, shutdown = TRUE)
cat("== SQLite", as.character(packageVersion("RSQLite")), "\n")
sq <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
DBI::dbWriteTable(sq, "df", df); run(sq, "df"); DBI::dbDisconnect(sq)
pg <- tryCatch(DBI::dbConnect(RPostgres::Postgres(), host = "/tmp/claude/mp-pg/sock", port = 55432, user = "marginplyr", dbname = "postgres"), error = function(e) NULL)
if (!is.null(pg)) {
  cat("== PostgreSQL (pg_temp table)\n")
  DBI::dbWriteTable(pg, "hunt4_cand2", df, temporary = TRUE); run(pg, "hunt4_cand2"); DBI::dbDisconnect(pg)
}
# Generic SQL path (#1768's own shape): the forwarding connection of
# tests/testthat/helper-generic-connection.R over in-memory DuckDB, inlined.
local({
  where <- new.env(parent = asNamespace("DBI")); cls <- "MarginplyrGenericConnection"
  methods::setClass(cls, contains = "DBIConnection", slots = c(inner = "DBIConnection"), where = where)
  fw <- function(g, s, f) methods::setMethod(g, s, f, where = where)
  fw("dbSendQuery", c(cls, "character"), function(conn, statement, ...) DBI::dbSendQuery(conn@inner, statement, ...))
  fw("dbGetQuery", c(cls, "character"), function(conn, statement, ...) DBI::dbGetQuery(conn@inner, statement, ...))
  fw("dbExecute", c(cls, "character"), function(conn, statement, ...) DBI::dbExecute(conn@inner, statement, ...))
  fw("dbExistsTable", c(cls, "ANY"), function(conn, name, ...) DBI::dbExistsTable(conn@inner, name, ...))
  fw("dbListFields", c(cls, "ANY"), function(conn, name, ...) DBI::dbListFields(conn@inner, name, ...))
  fw("dbQuoteIdentifier", c(cls, "character"), function(conn, x, ...) DBI::dbQuoteIdentifier(DBI::ANSI(), x, ...))
  fw("dbQuoteIdentifier", c(cls, "SQL"), function(conn, x, ...) x)
  fw("dbQuoteString", c(cls, "character"), function(conn, x, ...) DBI::dbQuoteString(DBI::ANSI(), x, ...))
  fw("dbIsValid", cls, function(dbObj, ...) DBI::dbIsValid(dbObj@inner))
  fw("dbGetInfo", cls, function(dbObj, ...) list(dbname = "generic", db.version = "1"))
  fw("dbDataType", cls, function(dbObj, obj, ...) DBI::dbDataType(dbObj@inner, obj, ...))
  registerS3method("dbplyr_edition", cls, function(con) 2L, envir = asNamespace("dbplyr"))
  inner <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), ":memory:")
  DBI::dbWriteTable(inner, "df", df)
  con <- methods::new(cls, inner = inner)
  t <- tbl(con, "df")
  cat("== generic SQL connection over DuckDB (#1768 shape: share + typed-missing label)\n")
  for (sort in c("none", "last")) {
    q <- summarize_with_margins(t, n = sum(v, na.rm = TRUE), p = share_of_total(n), .grouping = rollup(a),
                                .check_share_source = FALSE, .margin_label = NULL, .sort = sort)
    st(paste0("collect(), .sort = '", sort, "'"), q)
    st(paste0("count(), .sort = '", sort, "'"), count(q))
  }
  q <- summarize_with_margins(t, n = sum(v, na.rm = TRUE), .grouping = rollup(a), .sort = "last")
  st("count(), no share, text label, .sort = 'last'", count(q))
  DBI::dbDisconnect(inner, shutdown = TRUE)
})
