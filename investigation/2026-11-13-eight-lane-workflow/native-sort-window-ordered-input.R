# Investigated SHA: 6165f4768400a28520cc8977e056da0db49a32ed
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# With PGHOST set it also runs on that PostgreSQL server, inside a schema it creates and drops.
# On the native GROUPING SETS path a sorted summary keeps the input's window-order term in its ORDER BY when every label is typed-missing.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
try_it <- function(expr) tryCatch(
  { out <- suppressWarnings(collect(expr)); paste0(nrow(out), " rows: ", paste(sprintf("%s=%s", out[[1]], out[[ncol(out)]]), collapse = " ")) },
  error = function(e) paste0("<", class(e)[1], "> ", sub(".*(ERROR: *|Binder Error: )", "", gsub("\\s+", " ", conditionMessage(e))))
)
order_tail <- function(query) {
  sql <- as.character(dbplyr::sql_render(query))
  gsub("\\s+", " ", sub("(?s).*ORDER BY", "ORDER BY", sql, perl = TRUE))
}
d <- data.frame(a = c("x", "x", "y", "y", "x"), v = c(5, 3, 9, 1, 7))
run <- function(con, label) {
  t <- tbl(con, "wo_repro")
  ordered <- dbplyr::window_order(t, v)
  top <- head(arrange(t, v), 3)
  f <- function(input, ...) summarize_with_margins(input, s = sum(v, na.rm = TRUE), .grouping = rollup(a), ...)
  cat("==", label, "\n")
  cat("expected by hand, key bit(a), is.na(a), a: x=15 y=10 NA=25; over the three smallest v: x=8 y=1 NA=9\n")
  cat("window_order(v), NULL label, last  :", try_it(f(ordered, .margin_label = NULL, .sort = "last")), "\n")
  cat("  its ORDER BY                     :", order_tail(f(ordered, .margin_label = NULL, .sort = "last")), "\n")
  cat("window_order(v), NA label, first   :", try_it(f(ordered, .margin_label = NA_character_, .sort = "first")), "\n")
  cat("arrange(v) |> head(3), NULL, last  :", try_it(f(top, .margin_label = NULL, .sort = "last")), "\n")
  cat("arrange(v), NULL label, last       :", try_it(f(arrange(t, v), .margin_label = NULL, .sort = "last")), "\n")
  cat("control: unordered input           :", try_it(f(t, .margin_label = NULL, .sort = "last")), "\n")
  cat("control: default label             :", try_it(f(ordered, .sort = "last")), "\n")
  cat("control: NULL label and .id        :", try_it(f(ordered, .margin_label = NULL, .sort = "last", .id = "id")), "\n")
  cat("control: NULL label, .sort = none  :", try_it(f(ordered, .margin_label = NULL)), "\n")
  cat("control: window_order(a)           :", try_it(f(dbplyr::window_order(t, a), .margin_label = NULL, .sort = "last")), "\n")
  native <- summarise(ordered, s = sum(v, na.rm = TRUE), .by = a)
  cat("native summarise(.by = a)          :", try_it(native), "\n")
  cat("native ... |> arrange(a)           :", try_it(arrange(native, a)), "\n")
  cat("native ... |> window_order() |> arrange(a):", try_it(arrange(dbplyr::window_order(native), a)), "\n\n")
}
duck <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
DBI::dbWriteTable(duck, "wo_repro", d)
run(duck, "DuckDB")
DBI::dbDisconnect(duck, shutdown = TRUE)
if (nzchar(Sys.getenv("PGHOST")) && requireNamespace("RPostgres", quietly = TRUE)) {
  pg <- DBI::dbConnect(RPostgres::Postgres())
  invisible(DBI::dbExecute(pg, "CREATE SCHEMA IF NOT EXISTS mp_note_20261113"))
  invisible(DBI::dbExecute(pg, "SET search_path TO mp_note_20261113"))
  tryCatch({
    DBI::dbWriteTable(pg, "wo_repro", d, overwrite = TRUE)
    run(pg, paste("PostgreSQL", DBI::dbGetQuery(pg, "show server_version")[[1]]))
  }, finally = {
    invisible(DBI::dbExecute(pg, "DROP SCHEMA IF EXISTS mp_note_20261113 CASCADE"))
    DBI::dbDisconnect(pg)
  })
} else {
  cat("PostgreSQL: skipped, PGHOST is not set\n")
}
