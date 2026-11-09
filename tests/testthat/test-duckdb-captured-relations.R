test_that("captured DuckDB identifiers refuse source-value view inference", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("k", "101"), path)
  DBI::dbExecute(con, paste0(
    'CREATE VIEW "reader.csv" AS SELECT * FROM read_csv(',
    DBI::dbQuoteString(con, path), ", header=true)"
  ))
  source <- dplyr::tbl(con, "reader.csv")
  control <- dplyr::tbl(con, dbplyr::sql('TABLE "reader.csv"'), vars = "k")
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  before <- serialize(source$lazy_query, NULL)
  for (value in c("101", "source_tail")) {
    writeLines(c("k", value), path)
    # Changing only the data row changes the native zero-row result's type.
    expect_identical(is.numeric(dplyr::collect(utils::head(source, 0L))$k),
                     identical(value, "101"))
    for (input in list(control, source)) {
      expect_error(inspect_grouping(
        input, .grouping = grouping_set(where(is.numeric))
      ), "source rows", fixed = TRUE, class = "marginplyr_error")
      expect_duckdb_catalog_only()
    }
    expect_identical(as.character(dplyr::collect(source)$k), value)
  }
  unlink(path)
  expect_identical(inspect_grouping(
    source, .grouping = grouping_set(k)
  )$included, "(k)")
  expect_equal(nrow(last_sent_queries()), 0L)
  expect_identical(serialize(source$lazy_query, NULL), before)
})

test_that("captured tables and established views share one typed snapshot", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("k,label", "not_an_integer,east"), path)
  DBI::dbWriteTable(con, "ordinary.csv", data.frame(k = 101L, label = "east"))
  ordinary <- dplyr::tbl(con, "ordinary.csv")
  for (name in c("established", 'reader"quoted')) {
    DBI::dbExecute(con, paste0(
      "CREATE VIEW ", DBI::dbQuoteIdentifier(con, name),
      " AS SELECT * FROM read_csv(", DBI::dbQuoteString(con, path),
      ", columns={'k':'BIGINT','label':'VARCHAR'},",
      " auto_detect=false, header=true)"
    ))
  }
  inputs <- list(
    ordinary, dplyr::tbl(con, "established"),
    dplyr::tbl(con, 'reader"quoted')
  )
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  for (input in inputs) {
    before <- serialize(input$lazy_query, NULL)
    plan <- inspect_grouping(
      input, .by = where(is.character),
      .grouping = rollup(where(is.numeric))
    )
    expect_identical(plan$fixed, c("(label)", "(label)"))
    expect_identical(plan$included, c("(k)", "()"))
    audit <- last_sent_queries(details = TRUE)
    expect_identical(audit$purpose, c("relation_catalog", "selection_proxy"))
    expect_identical(audit$status, c("returned", "returned"))
    expect_identical(audit$source, c("literal", "render"))
    expect_match(audit$sql[[2L]], "LIMIT 0", fixed = TRUE)
    expect_identical(serialize(input$lazy_query, NULL), before)
  }
  # Type conversion would fail if inspection processed the file's source row.
  expect_error(dplyr::collect(inputs[[2L]]), "Conversion Error", fixed = TRUE)
  expect_identical(dplyr::collect(ordinary)$k, 101L)
})

test_that("qualified and derived captures admit stored views before binding", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("k", "101"), path)
  DBI::dbExecute(con, "CREATE SCHEMA metadata")
  for (name in c('"reader"', 'metadata."reader"', '"reader.part"')) {
    DBI::dbExecute(con, paste0(
      "CREATE VIEW ", name, " AS SELECT * FROM read_csv(",
      DBI::dbQuoteString(con, path), ", header=true)"
    ))
  }
  ordinary <- dplyr::copy_to(con, tibble::tibble(k = 101L), "established")
  sources <- list(
    dplyr::tbl(con, "reader"), dplyr::tbl(con, "reader.part"),
    dplyr::tbl(con, DBI::Id(schema = "metadata", table = "reader"))
  )
  source <- sources[[3L]]
  inputs <- c(sources, list(
    dplyr::filter(source, k == k), dplyr::rename(source, renamed = k),
    dplyr::left_join(ordinary, source, by = "k"),
    dplyr::left_join(source, ordinary, by = "k")
  ))
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  for (value in c("101", "source_tail")) {
    writeLines(c("k", value), path)
    expect_identical(is.numeric(dplyr::collect(utils::head(source, 0L))$k),
                     identical(value, "101"))
    for (audit in c(TRUE, FALSE)) {
      options(marginplyr.audit_sql = audit)
      for (input in inputs) {
        before <- serialize(input$lazy_query, NULL)
        expect_error(inspect_grouping(
          input, .by = where(is.numeric)
        ), "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
        if (audit) expect_duckdb_catalog_only()
        expect_error(inspect_grouping(
          input, .grouping = grouping_set(where(is.numeric))
        ), "source rows", fixed = TRUE, class = "marginplyr_error")
        if (audit) expect_duckdb_catalog_only()
        expect_identical(serialize(input$lazy_query, NULL), before)
      }
    }
  }
  unlink(path)
  options(marginplyr.audit_sql = TRUE)
  for (input in inputs) {
    expected <- paste0("(", paste(dplyr::tbl_vars(input), collapse = ", "), ")")
    expect_identical(inspect_grouping(
      input, .grouping = grouping_set(everything())
    )$included, expected)
    expect_equal(nrow(last_sent_queries()), 0L)
  }
})
