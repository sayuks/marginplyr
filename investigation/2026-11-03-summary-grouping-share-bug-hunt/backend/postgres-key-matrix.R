pkgload::load_all(quiet = TRUE)
source("investigation/2026-11-03-summary-grouping-share-bug-hunt/backend/postgres-fixture.R")
with_postgres(function(con) {
  types <- list(
    text = c("'a'::text", "'b'::text"),
    padded = c("'a'::char(4)", "'b'::char(4)"),
    numeric = c("1.1::numeric", "2.2::numeric"),
    large_numeric = c("1e400::numeric", "2e400::numeric"),
    tiny_numeric = c("1e-400::numeric", "2e-400::numeric"),
    integer64 = c("9007199254740992::bigint", "9007199254740993::bigint"),
    date = c("DATE '2000-01-01'", "DATE '2001-01-01'"),
    timestamp = c("TIMESTAMPTZ '2000-01-01 00:00:00+00'", "TIMESTAMPTZ '2000-01-01 00:00:01+00'"),
    interval = c("INTERVAL '1 day'", "INTERVAL '2 days'"),
    array = c("ARRAY[1,2]", "ARRAY[3,4]"),
    jsonb = c("'{\"a\":1}'::jsonb", "'{\"a\":2}'::jsonb"),
    bytes = c("'\\x0102'::bytea", "'\\x0304'::bytea"),
    range = c("'[1,2)'::int4range", "'[2,3)'::int4range"),
    boolean = c("TRUE", "FALSE")
  )
  cases <- expand.grid(type = names(types), fixed = c(FALSE, TRUE),
    portable = c(FALSE, TRUE), sorted = c(FALSE, TRUE), stringsAsFactors = FALSE)
  rows <- list()
  for (case in seq_len(nrow(cases))) {
    shape <- cases[case, ]
    values <- types[[shape$type]]
    DBI::dbExecute(con, "DROP TABLE IF EXISTS facts")
    DBI::dbExecute(con, paste0("CREATE TEMP TABLE facts AS SELECT ", values[[1L]],
      " AS k, 'a'::text AS g, 1::integer AS x UNION ALL SELECT ", values[[2L]],
      ", 'b', 3 UNION ALL SELECT NULL, 'a', 5"))
    source <- dplyr::tbl(con, "facts")
    query <- tryCatch(summarize_with_margins(source, value = sum(x, na.rm = TRUE),
      p = share_of_parent(value), t = share_of_total(value),
      .by = if (shape$fixed) "k" else NULL,
      .grouping = if (shape$fixed) rollup(g) else rollup(k, g),
      .duplicates = if (shape$portable) "keep" else "error",
      .margin_label = NULL, .id = "set", .sort = if (shape$sorted) "last" else "none"), error = identity)
    result <- if (inherits(query, "error")) query else tryCatch(dplyr::collect(query), error = identity)
    # Distinct k partitions contain one fact each. Without fixed keys the
    # subtotal Parent ratios pool facts; detail Parent ratios are one.
    expected <- if (shape$fixed) rep(1, 6L) else c(rep(c(1, 3, 5)/9, 2), 1)
    parent_expected <- if (shape$fixed) rep(1, 6L) else c(rep(1, 3L), c(1, 3, 5)/9, 1)
    pass <- !inherits(result, "error") &&
      isTRUE(all.equal(sort(result$t), sort(expected))) &&
      isTRUE(all.equal(sort(result$p), sort(parent_expected)))
    rows[[case]] <- cbind(shape, pass,
      outcome = if (inherits(result, "error")) conditionMessage(result) else paste(result$t, collapse = ","))
  }
  results <- do.call(rbind, rows)
  print(table(results$type, results$pass))
  print(results[!results$pass, ], row.names = FALSE)
  utils::write.csv(results, "investigation/2026-11-03-summary-grouping-share-bug-hunt/backend/postgres-key-matrix.csv", row.names = FALSE)
})
