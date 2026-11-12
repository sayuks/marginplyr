# Investigated SHA: 03e3cef502f7332b69e50608740033043d27f2b4
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# Arguments: native|off|on [message|warning]. A once-per-session translation message is lost when the SQL audit is on.
mode <- commandArgs(TRUE)[1]; kind <- c(commandArgs(TRUE), "message")[2]
suppressMessages(pkgload::load_all(".", quiet = TRUE))
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
methods::setClass("NoteSQLiteConnection", contains = "SQLiteConnection")
dplyr::copy_to(con, data.frame(g = c("a", "b"), v = c(1, 2)), "src")
dialect <- dbplyr::sql_dialect(con); variant <- dbplyr::sql_translation(con)
registerS3method("sql_dialect", "NoteSQLiteConnection", function(con) {
  structure(dialect, class = c("note_sqlite", class(dialect)))
}, envir = asNamespace("dbplyr"))
registerS3method("sql_translation", "note_sqlite", function(con) {
  dbplyr::sql_variant(variant$scalar, dbplyr::sql_translator(
    .parent = variant$aggregate,
    sum = function(...) {
      signal <- if (kind == "message") rlang::inform else rlang::warn
      signal("SUM here ignores NULL", .frequency = "once", .frequency_id = "note_sum")
      variant$aggregate$sum(...)
    }
  ), variant$window)
}, envir = asNamespace("dbplyr"))
remote <- dplyr::tbl(methods::new("NoteSQLiteConnection", con), "src")
options(marginplyr.audit_sql = identical(mode, "on"))
seen <- character()
note <- function(cnd) {
  seen <<- c(seen, conditionMessage(cnd))
  rlang::cnd_muffle(cnd)
}
out <- withCallingHandlers(
  if (mode == "native") {
    dplyr::collect(dplyr::summarize(remote, total = sum(v, na.rm = TRUE), .by = g))
  } else {
    dplyr::collect(summarize_with_margins(
      remote, total = sum(v, na.rm = TRUE), .grouping = rollup(g)
    ))
  },
  message = note, warning = note
)
cat(mode, kind, "-> delivered to the caller:", length(seen), "\n")
