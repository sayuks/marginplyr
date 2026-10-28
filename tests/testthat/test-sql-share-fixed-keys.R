test_that("SQL share ordering reads restored fixed keys from its input", {
  con <- dbplyr::simulate_postgres()
  remote <- dbplyr::tbl_lazy(
    tibble::tibble(fixed = "a", g = "x", value = 1), con = con
  )
  for (helper in c("share_of_parent", "share_of_total")) {
    for (sort in c("first", "last")) {
      for (id in list(NULL, "set")) {
        share <- rlang::call2(helper, rlang::sym("amount"))
        query <- summarize_with_margins(
          remote, amount = sum(value, na.rm = TRUE), share = !!share,
          .by = fixed, .grouping = rollup(g), .sort = sort, .id = id,
          .check_share_source = FALSE
        )
        built <- dbplyr::sql_build(query)
        info <- paste(helper, sort, id)
        # PostgreSQL cannot read a SELECT alias inside an ORDER BY expression.
        expect_true(any(grepl(
          ' AS "fixed"', built$from$select, fixed = TRUE
        )), info = info)
        expect_true(any(grepl(
          '"fixed" IS NULL', built$order_by, fixed = TRUE
        )), info = info)
      }
    }
  }
})

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
