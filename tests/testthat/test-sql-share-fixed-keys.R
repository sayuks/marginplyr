test_that("SQLite shares preserve fixed keys named like pronouns", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_sql_share_fixed_keys(con)
})

test_that("DuckDB shares preserve fixed keys named like pronouns", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_sql_share_fixed_keys(con)
})
