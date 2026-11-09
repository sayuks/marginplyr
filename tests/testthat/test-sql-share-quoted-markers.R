test_that("quoted comment markers retain direct SQL share cardinality checks", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_sql_share_quoted(con, "duckdb")
})

test_that("PostgreSQL quotes retain guards within the supported SQL boundary", {
  source <- dbplyr::lazy_frame(
    g = "a", x = 1, con = dbplyr::simulate_postgres()
  )
  forms <- c(
    "SUM(x) + UNNEST(ARRAY[10,20]) + LENGTH('--')",
    "SUM(x) + UNNEST(ARRAY[10,20]) + LENGTH('a''/*')",
    "SUM(\"x--\") + UNNEST(ARRAY[10,20])",
    "SUM(\"x\"\"/*\") + UNNEST(ARRAY[10,20])"
  )
  excluded <- c(
    "SUM(x) + UNNEST(ARRAY[10,20]) /* real comment */",
    "SUM(x) + UNNEST(ARRAY[10,20]) -- real comment\n",
    "SUM(x) + UNNEST(ARRAY[10,20]) + LENGTH($$--$$)",
    "SUM(x) + UNNEST(ARRAY[10,20]) + LENGTH(E'\\\\--')",
    "SUM(x) + schema.UNNEST(ARRAY[10,20])",
    "SUM(x) + (SELECT SUM(v) FROM UNNEST(ARRAY[10,20]) AS t(v))"
  )
  for (portable in c(FALSE, TRUE)) {
    for (value in c(forms, excluded)) {
      query <- summarize_with_margins(
        source, value = dbplyr::sql(value),
        parent = share_of_parent(value), total = share_of_total(value),
        .grouping = if (portable) rollup(g, g) else rollup(g),
        .duplicates = if (portable) "keep" else "error",
        .check_share_source = FALSE
      )
      rendered <- as.character(dbplyr::sql_render(query))
      expect_match(rendered, if (portable) "UNION ALL" else "GROUPING SETS",
                   fixed = TRUE)
      expect_identical(grepl("exactly one value per grouping row", rendered,
                             fixed = TRUE), value %in% forms)
    }
  }
})
