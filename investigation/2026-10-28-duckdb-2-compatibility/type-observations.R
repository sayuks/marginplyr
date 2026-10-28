# SQL type observations; run from the repository root with one output RDS path.
pkgload::load_all('.', quiet = TRUE)
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
options(marginplyr.audit_sql = TRUE)
cat('R package:', as.character(packageVersion('duckdb')), '\n')
print(DBI::dbGetQuery(con, 'PRAGMA version'))
cases <- c(
  untyped_null = 'NULL',
  integer_null = 'CAST(NULL AS INTEGER)',
  decimal = '12.345::DECIMAL(12,3)',
  bigint = '9223372036854775807::BIGINT',
  timestamp = "TIMESTAMP '2026-01-01 12:34:56.123456'",
  timestamp_tz = "TIMESTAMPTZ '2026-01-01 12:34:56+00'",
  timestamp_tz_ns = "CAST(TIMESTAMPTZ '2026-01-01 12:34:56+00' AS TIMESTAMPTZ_NS)",
  tuple = "ROW(1, 'a')",
  empty_struct = "{}"
)
results <- list()
for (name in names(cases)) {
  for (empty in c(FALSE, TRUE)) {
    for (verb in c('summary', 'expand')) {
      label <- paste(name, empty, verb, sep = '-')
      results[[label]] <- tryCatch({
        source <- dplyr::tbl(con, dbplyr::sql(paste(
          'SELECT', cases[[name]], 'AS g, 7 AS value',
          if (empty) 'WHERE FALSE' else ''
        )))
        original <- dplyr::collect(source)
        query <- if (verb == 'summary') {
          summarize_with_margins(source, total = sum(value, na.rm = TRUE),
            .grouping = rollup(g), .margin_label = NULL, .id = 'sid')
        } else {
          expand_with_margins(source, .grouping = rollup(g),
            .margin_label = NULL, .id = 'sid')
        }
        record <- last_sent_queries(details = TRUE)
        schema <- DBI::dbGetQuery(con, paste('DESCRIBE', dbplyr::sql_render(query)))
        collected <- dplyr::collect(query)
        materialized <- dplyr::collect(dplyr::compute(query))
        list(original = original, schema = schema, collected = collected,
          materialized = materialized, audit = record)
      }, error = function(cnd) list(error = conditionMessage(cnd), classes = class(cnd)))
      value <- results[[label]]
      cat(label, if (!is.null(value$error)) value$error else paste(
        'OK', nrow(value$collected), paste(value$schema$column_type, collapse = ',')
      ), '\n')
    }
  }
}
args <- commandArgs(TRUE)
stopifnot(length(args) == 1L)
saveRDS(results, args[[1L]])
DBI::dbDisconnect(con, shutdown = TRUE)
