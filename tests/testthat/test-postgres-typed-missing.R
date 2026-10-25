# Live type/value acceptance runs in tools/postgres-typed-missing.R.
# Simulators exercise construction without a server. Semantic SQL assertions
# pin typed empty scalar results rather than byte-for-byte query formatting.
test_that("PostgreSQL missing dimensions retain a lazy type witness", {
  source <- dbplyr::lazy_frame(
    g = 1L, h = "a", value = 1, con = dbplyr::simulate_postgres()
  )
  source <- dplyr::mutate(source, g = as.Date(.data$g))
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  for (label in list(NULL, NA_character_)) {
    queries <- list(
      expand_with_margins(
        source, .grouping = rollup(g, h), .margin_label = label,
        .id = "set", .sort = "last"
      ),
      summarize_with_margins(
        source, amount = sum(.data$value),
        .grouping = rollup(g, g, h), .duplicates = "keep",
        .margin_label = label, .id = "set"
      )
    )
    expect_identical(last_sent_queries()$purpose, "result")
    for (query in queries) {
      sql <- as.character(dbplyr::sql_render(query))
      expect_match(sql, "UNION ALL", fixed = TRUE)
      expect_match(sql, '(SELECT "g" FROM (', fixed = TRUE)
      expect_match(sql, "WHERE (FALSE)", fixed = TRUE)
      expect_match(sql, "AS DATE", fixed = TRUE)
      expect_s3_class(query, "tbl_lazy")
    }
  }
})

test_that("PostgreSQL Parent keys retain types behind character displays", {
  source <- dbplyr::lazy_frame(
    g = 1L, h = "a", value = 1, con = dbplyr::simulate_postgres()
  )
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  query <- summarize_with_margins(
    source, amount = sum(.data$value), parent = share_of_parent(amount),
    .grouping = rollup(g, g, h), .duplicates = "keep",
    .check_share_source = FALSE, .id = "set"
  )
  expect_identical(last_sent_queries()$purpose, "result")
  sql <- as.character(dbplyr::sql_render(query))
  expect_match(sql, "UNION ALL", fixed = TRUE)
  expect_match(sql, "'Total'", fixed = TRUE)
  expect_match(sql, '(SELECT "g" FROM (', fixed = TRUE)
  expect_match(sql, "WHERE (FALSE)", fixed = TRUE)
})
