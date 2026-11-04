test_that("DuckDB rejects a duration source for an own-denominator share", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SEQUENCE duration_read")
  remote <- dplyr::tbl(con, dbplyr::sql(paste(
    "SELECT CASE WHEN nextval('duration_read') > 0",
    "THEN amount * INTERVAL '1 day' END AS duration, amount",
    "FROM (VALUES (1), (2)) AS input(amount)"
  )), vars = c("duration", "amount"))

  query <- summarize_with_margins(
    remote, value = max(duration, na.rm = TRUE), whole = share_of_total(value)
  )
  expect_s3_class(query, "tbl_lazy")
  expect_error(DBI::dbGetQuery(con, "SELECT currval('duration_read')"),
               "not yet defined")
  expect_error(dplyr::collect(query), "Binder Error:.*INTERVAL")

  ordinary <- dplyr::collect(summarize_with_margins(
    remote, value = max(duration, na.rm = TRUE)
  ))
  expect_s3_class(ordinary$value, "difftime")
  expect_identical(as.numeric(ordinary$value, units = "secs"), 172800)

  eligible <- dplyr::collect(summarize_with_margins(
    remote, value = max(amount, na.rm = TRUE), whole = share_of_total(value)
  ))
  expect_identical(eligible$whole, 1)

  local <- dplyr::collect(remote)
  expect_error(
    summarize_with_margins(
      local, value = max(duration, na.rm = TRUE), whole = share_of_total(value)
    ),
    "plain integer or double scalar", class = "marginplyr_error"
  )
})

test_that("DuckDB rejects all-missing duration sources in a rollup", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  remote <- dplyr::tbl(con, dbplyr::sql(paste(
    "SELECT g, NULL::INTERVAL AS duration, NULL::DOUBLE AS amount",
    "FROM (VALUES ('a'), ('b')) AS input(g)"
  )), vars = c("g", "duration", "amount"))

  for (duplicates in c("error", "keep")) {
    # Each helper must reject the source when it is the only share requested.
    for (helper in c("share_of_total", "share_of_parent")) {
      share <- rlang::call2(helper, quote(value))
      eligible <- dplyr::collect(summarize_with_margins(
        remote, value = max(amount, na.rm = TRUE), ratio = !!share,
        .grouping = rollup(g), .duplicates = duplicates,
        .id = "set", .margin_label = NULL
      ))
      expect_identical(
        eligible$ratio[eligible$set == 1L], c(NA_real_, NA_real_)
      )
      expect_identical(eligible$ratio[eligible$set == 2L], 1)
      query <- summarize_with_margins(
        remote, value = max(duration, na.rm = TRUE), ratio = !!share,
        .grouping = rollup(g), .duplicates = duplicates,
        .id = "set", .margin_label = NULL
      )
      expect_s3_class(query, "tbl_lazy")
      expect_error(
        dplyr::collect(query), "Binder Error:.*INTERVAL", info = helper
      )
    }
  }
})
