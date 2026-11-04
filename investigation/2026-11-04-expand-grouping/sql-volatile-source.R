pkgload::load_all(quiet = TRUE)
options(marginplyr.audit_sql = TRUE)

probe_duckdb <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, "CREATE SEQUENCE payload_probe")
  DBI::dbExecute(con, "CREATE SEQUENCE predicate_probe")
  DBI::dbExecute(con, "CREATE SEQUENCE payload_control")
  DBI::dbExecute(con, "CREATE SEQUENCE predicate_control")

  expand <- function(source_sql) {
    source <- dplyr::tbl(con, dbplyr::sql(source_sql))
    query <- expand_with_margins(
      source, .grouping = rollup("g"), .margin_label = NULL, .id = "sid"
    )
    counter <- DBI::dbGetQuery(con, "SELECT last_value FROM duckdb_sequences()")
    stopifnot(inherits(query, "tbl_lazy"))
    audit <- last_sent_queries()
    stopifnot(identical(audit$purpose, c("selection_proxy", "result")),
              grepl("LIMIT 0", audit$sql[[1]], fixed = TRUE))
    list(query = query, counter = counter)
  }
  control <- function(source_sql, payload) {
    DBI::dbGetQuery(con, paste0(
      "WITH src AS MATERIALIZED (", source_sql, ") ",
      "SELECT g, 1 AS sid", payload, " FROM src UNION ALL ",
      "SELECT NULL, 2", payload, " FROM src"
    ))
  }

  payload_source <- "SELECT 'a' AS g, nextval('%s') AS token"
  payload <- expand(sprintf(payload_source, "payload_probe"))
  stopifnot(all(is.na(payload$counter$last_value)))
  payload_expected <- control(sprintf(payload_source, "payload_control"),
                              ", token")
  payload_actual <- as.data.frame(dplyr::collect(payload$query))
  payload_actual <- payload_actual[order(payload_actual$sid), , drop = FALSE]
  rownames(payload_actual) <- NULL
  print(payload_expected)
  print(payload_actual)

  predicate_source <- "SELECT 'a' AS g WHERE nextval('%s') = 1"
  predicate <- expand(sprintf(predicate_source, "predicate_probe"))
  predicate_expected <- control(
    sprintf(predicate_source, "predicate_control"), ""
  )
  predicate_actual <- as.data.frame(dplyr::collect(predicate$query))
  predicate_rows <- order(predicate_actual$sid)
  predicate_actual <- predicate_actual[predicate_rows, , drop = FALSE]
  rownames(predicate_actual) <- NULL
  print(predicate_expected)
  print(predicate_actual)
  print(DBI::dbGetQuery(con, paste(
    "SELECT sequence_name, last_value FROM duckdb_sequences()",
    "ORDER BY sequence_name"
  )))

  c(payload = identical(payload_actual, payload_expected),
    predicate = identical(predicate_actual, predicate_expected))
}

probe_sqlite <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  sql <- "SELECT 'a' AS g, hex(randomblob(16)) AS token"
  source <- dplyr::tbl(con, dbplyr::sql(sql))
  query <- expand_with_margins(
    source, .grouping = rollup("g"), .margin_label = NULL, .id = "sid"
  )
  stopifnot(inherits(query, "tbl_lazy"),
            identical(last_sent_queries()$purpose, "result"))
  actual <- dplyr::collect(query)
  expected <- DBI::dbGetQuery(con, paste0(
    "WITH src AS MATERIALIZED (", sql, ") ",
    "SELECT g, 1 AS sid, token FROM src UNION ALL ",
    "SELECT NULL, 2, token FROM src"
  ))
  stopifnot(identical(sort(actual$sid), 1:2),
            identical(sort(expected$sid), 1:2))
  cat("SQLite token equality, control:", length(unique(expected$token)) == 1L,
      ", expansion:", length(unique(actual$token)) == 1L, "\n")
  stopifnot(length(unique(expected$token)) == 1L)
  c(sqlite = length(unique(actual$token)) == 1L)
}

matched <- c(probe_duckdb(), probe_sqlite())
print(matched)
if ("--assert" %in% commandArgs(trailingOnly = TRUE)) stopifnot(all(matched))
