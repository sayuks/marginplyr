# From the investigated marginplyr checkout:
# Rscript /path/to/duckdb-interval.R [--assert]
pkgload::load_all(quiet = TRUE)

observe <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, paste(
    "CREATE TEMP TABLE facts AS",
    "SELECT 'a' AS g, INTERVAL '1 day' AS duration, 1 AS amount",
    "UNION ALL SELECT 'b', INTERVAL '2 days', 3"
  ))
  remote <- dplyr::tbl(con, "facts")
  query <- summarize_with_margins(
    remote, value = max(duration, na.rm = TRUE), total = share_of_total(value)
  )
  observed <- tryCatch(dplyr::collect(query), error = identity)

  no_share <- dplyr::collect(summarize_with_margins(
    remote, value = max(duration, na.rm = TRUE)
  ))
  numeric <- dplyr::collect(summarize_with_margins(
    remote, value = max(amount, na.rm = TRUE), total = share_of_total(value)
  ))
  local_refusal <- tryCatch(
    summarize_with_margins(
      dplyr::collect(remote),
      value = max(duration, na.rm = TRUE), total = share_of_total(value)
    ),
    error = identity
  )
  detail_query <- summarize_with_margins(
    remote, value = max(duration, na.rm = TRUE), total = share_of_total(value),
    .grouping = rollup(g)
  )
  detail_refusal <- tryCatch(dplyr::collect(detail_query), error = identity)

  types <- DBI::dbGetQuery(con, paste(
    "SELECT typeof(max(duration)) AS source_type,",
    "typeof(max(duration) * 1) AS product_type FROM facts"
  ))

  stopifnot(
    inherits(query, "tbl_lazy"),
    inherits(detail_query, "tbl_lazy"),
    inherits(no_share$value, "difftime"),
    identical(as.numeric(no_share$value, units = "secs"), 172800),
    identical(numeric$total, 1),
    inherits(local_refusal, "marginplyr_error"),
    grepl("plain integer or double scalar", conditionMessage(local_refusal),
          fixed = TRUE),
    inherits(detail_refusal, "error"),
    identical(types$source_type, "INTERVAL"),
    identical(types$product_type, "INTERVAL")
  )
  print(types)
  print(observed)
  cat("Generated query:\n", as.character(dbplyr::sql_render(query)), "\n")
  cat("Local control:", conditionMessage(local_refusal), "\n")
  cat("DuckDB detail control:", conditionMessage(detail_refusal), "\n")
  cat("DuckDB numeric control:", numeric$total, typeof(numeric$total), "\n")

  DBI::dbExecute(con, paste(
    "CREATE TEMP TABLE missing_facts AS",
    "SELECT g, NULL::INTERVAL AS duration FROM facts"
  ))
  missing_remote <- dplyr::tbl(con, "missing_facts")
  missing_query <- summarize_with_margins(
    missing_remote, value = max(duration, na.rm = TRUE),
    total = share_of_total(value), parent = share_of_parent(value),
    .grouping = rollup(g), .id = "set", .margin_label = NULL
  )
  stopifnot(inherits(missing_query, "tbl_lazy"))
  missing <- tryCatch(dplyr::collect(missing_query), error = identity)
  cat("DuckDB declared INTERVAL with all values missing:\n")
  print(missing)

  if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {

    stopifnot(inherits(observed, "error"), inherits(missing, "error"))
  } else {

    stopifnot(
      is.data.frame(observed),
      identical(observed$value, no_share$value),
      identical(observed$total, 1),
      is.data.frame(missing),
      inherits(missing$value, "difftime"),
      all(is.na(missing$value)),
      sum(missing$set == 2L) == 1L,
      sum(missing$set == 1L) == 2L,
      identical(missing$total[missing$set == 1L], rep(NA_real_, 2L)),
      identical(missing$parent[missing$set == 1L], rep(NA_real_, 2L)),
      identical(missing$total[missing$set == 2L], 1),
      identical(missing$parent[missing$set == 2L], 1)
    )
  }
}
observe()
