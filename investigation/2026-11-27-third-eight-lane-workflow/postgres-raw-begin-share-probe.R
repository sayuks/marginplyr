# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# Rejected candidate: a share request inside a raw BEGIN on PostgreSQL aborts the caller's transaction (ADR 0020, #774, scopes the savepoint to DBI transactions).
# A first share request inside a transaction begun with raw SQL `BEGIN` on
# RPostgres: the dialect probe runs without a savepoint, aborts the caller's
# transaction, the share is refused, and the caller's later COMMIT silently
# rolls back earlier work. Native dbplyr's equivalent share leaves it intact.
# Each arm runs in a fresh process so the dialect verdict is never cached.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
if (inherits(try(DBI::dbDisconnect(DBI::dbConnect(RPostgres::Postgres(), host = "/tmp/claude/mp-pg/sock", port = 55432, user = "marginplyr", dbname = "postgres")), silent = TRUE), "try-error")) {
  cat("The temporary PostgreSQL cluster of the investigation is not reachable; nothing was run.\n")
  quit(status = 0)
}
suppressMessages(library(dplyr))
arm <- commandArgs(TRUE)[1]
self <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
if (is.na(arm)) {
  cat("Expected (contract): the share builds as in autocommit or dbBegin(); the caller's\n",
      "transaction stays usable and COMMIT persists the marker row (marker = 1).\n\n", sep = "")
  for (a in c("marginplyr_raw_begin", "native_raw_begin", "marginplyr_dbBegin", "marginplyr_dbBegin_then_raw_commit")) {
    out <- system2(file.path(R.home("bin"), "Rscript"), c(shQuote(self), a), stdout = TRUE, stderr = TRUE)
    cat(grep("^(==|  )", out, value = TRUE), sep = "\n")
  }
  quit(save = "no", status = 0)
}
con <- DBI::dbConnect(RPostgres::Postgres(), host = "/tmp/claude/mp-pg/sock",
                      port = 55432, user = "marginplyr", dbname = "postgres")
sch <- paste0("hunt_lane5_c1_", tolower(substr(gsub("_", "", arm), 1, 30)))
q <- function(...) invisible(DBI::dbExecute(con, paste0(...)))
state <- function() tryCatch({ DBI::dbGetQuery(con, "select 1"); "usable" },
                             error = function(e) sub("\n.*", "", conditionMessage(e)))
main <- function() {
  q("CREATE SCHEMA ", sch)
  q("CREATE TABLE ", sch, ".t (g text, v int)")
  q("INSERT INTO ", sch, ".t VALUES ('a', 1), ('a', 2), ('b', 3)")
  q("CREATE TABLE ", sch, ".marker (x int)")
  t <- tbl(con, DBI::Id(schema = sch, table = "t"))
  if (grepl("raw_begin", arm)) q("BEGIN") else DBI::dbBegin(con)
  q("INSERT INTO ", sch, ".marker VALUES (1)")          # caller's earlier work
  if (grepl("then_raw_commit", arm)) q("COMMIT")         # DBI flag stays TRUE
  r <- tryCatch(
    if (startsWith(arm, "native")) {
      t |> group_by(g) |> summarise(n = sum(v, na.rm = TRUE)) |>
        mutate(p = n / sum(n, na.rm = TRUE)) |> collect()
    } else {
      collect(summarize_with_margins(t, n = sum(v), p = share_of_total(n),
                                     .grouping = rollup(g)))
    },
    error = function(e) e)
  cat("==", arm, "\n")
  if (inherits(r, "error")) {
    cat("  result: ", paste(class(r), collapse = " "), "\n  message: ",
        sub("\n.*", "", conditionMessage(r)), "\n", sep = "")
  } else cat("  result:", nrow(r), "rows, p =", format(r$p, digits = 3), "\n")
  cat("  transaction after the call:", substr(state(), 1, 90), "\n")
  if (!grepl("then_raw_commit", arm)) {
    ok <- tryCatch(if (grepl("raw_begin", arm)) { q("COMMIT"); TRUE } else DBI::dbCommit(con),
                   error = function(e) conditionMessage(e))
    cat("  caller COMMIT returned:", format(ok), "\n")
  }
  m <- DBI::dbGetQuery(con, paste0("select count(*)::int as n from ", sch, ".marker"))$n
  cat("  marker rows after commit:", m, "\n")
}
tryCatch(main(), finally = {
  try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
  suppressMessages(DBI::dbExecute(con, paste("DROP SCHEMA IF EXISTS", sch, "CASCADE")))
  DBI::dbDisconnect(con)
})
