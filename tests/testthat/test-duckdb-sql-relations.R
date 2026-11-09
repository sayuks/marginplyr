test_that("authored SQL view names cannot hide source-value inference", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("k", "101"), path)
  inputs <- lapply(c("ordinary", "reader.csv"), function(name) {
    identifier <- DBI::dbQuoteIdentifier(con, name)
    DBI::dbExecute(con, paste0(
      "CREATE VIEW ", identifier, " AS SELECT * FROM read_csv(",
      DBI::dbQuoteString(con, path), ", header=true)"
    ))
    dplyr::tbl(con, dbplyr::sql(paste("SELECT * FROM", identifier)), vars = "k")
  })
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  for (value in c("101", "source_tail")) {
    writeLines(c("k", value), path)
    for (input in inputs) {
      before <- serialize(input$lazy_query, NULL)
      # Source values change DuckDB's native zero-row type without changing SQL.
      expect_identical(is.numeric(dplyr::collect(utils::head(input, 0L))$k),
                       identical(value, "101"))
      for (audit in c(TRUE, FALSE)) {
        options(marginplyr.audit_sql = audit)
        expect_error(inspect_grouping(
          input, .by = where(is.numeric)
        ), "source rows", fixed = TRUE, class = "marginplyr_error")
        if (audit) expect_duckdb_catalog_only()
        expect_error(inspect_grouping(
          input, .grouping = grouping_set(where(is.numeric))
        ), "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
        if (audit) expect_duckdb_catalog_only()
      }
      expect_identical(serialize(input$lazy_query, NULL), before)
    }
  }
  unlink(path)
  options(marginplyr.audit_sql = TRUE)
  for (input in inputs) {
    expect_identical(inspect_grouping(
      input, .grouping = grouping_set(k)
    )$included, "(k)")
    expect_equal(nrow(last_sent_queries()), 0L)
  }
})

test_that("authored SQL follows ordinary nested views in their own namespace", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("k", "101"), path)
  DBI::dbExecute(con, "CREATE SCHEMA metadata")
  DBI::dbWriteTable(con, "dependency", data.frame(k = 999L))
  DBI::dbExecute(con, paste0(
    "CREATE VIEW metadata.dependency AS SELECT * FROM read_csv(",
    DBI::dbQuoteString(con, path), ", header=true)"
  ))
  DBI::dbExecute(con, "CREATE VIEW metadata.nested AS TABLE dependency")
  source <- dplyr::tbl(con, dbplyr::sql(paste(
    "WITH dependency AS (SELECT 999 AS k)",
    "SELECT * FROM metadata.nested"
  )), vars = "k")
  established <- dplyr::tbl(
    con, dbplyr::sql("TABLE main.dependency"), vars = "k"
  )
  inputs <- list(
    source, dplyr::filter(source, FALSE), dplyr::rename(source, renamed = k),
    dplyr::left_join(source, established, by = "k"),
    dplyr::left_join(established, source, by = "k")
  )
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  for (value in c("101", "source_tail")) {
    writeLines(c("k", value), path)
    expect_identical(is.numeric(dplyr::collect(utils::head(source, 0L))$k),
                     identical(value, "101"))
    filtered <- dplyr::collect(utils::head(inputs[[2L]], 0L))
    expect_identical(is.numeric(filtered$k), identical(value, "101"))
    for (input in inputs) {
      before <- serialize(input$lazy_query, NULL)
      expect_error(inspect_grouping(
        input, .by = where(is.numeric)
      ), "source rows", fixed = TRUE, class = "marginplyr_error")
      expect_duckdb_catalog_only()
      expect_error(inspect_grouping(
        input, .grouping = grouping_set(where(is.numeric))
      ), "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
      expect_duckdb_catalog_only()
      expect_identical(serialize(input$lazy_query, NULL), before)
    }
  }
  unlink(path)
  for (input in inputs) {
    expected <- paste0("(", paste(dplyr::tbl_vars(input), collapse = ", "), ")")
    expect_identical(inspect_grouping(
      input, .grouping = grouping_set(everything())
    )$included, expected)
    expect_equal(nrow(last_sent_queries()), 0L)
  }
})

test_that("ordinary SQL tables CTEs and established views retain typed plans", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("k,label", "not_an_integer,east"), path)
  DBI::dbWriteTable(con, "ordinary", data.frame(k = 101L, label = "east"))
  DBI::dbExecute(con, "CREATE SCHEMA metadata")
  DBI::dbWriteTable(con, DBI::Id(schema = "metadata", table = "dependency"),
                    data.frame(k = 101L, label = "east"))
  DBI::dbExecute(con, "CREATE VIEW metadata.safe AS TABLE dependency")
  for (name in c("with", "west")) {
    DBI::dbExecute(con, paste0(
      "CREATE VIEW ", DBI::dbQuoteIdentifier(con, name),
      " AS SELECT * FROM read_csv(",
      DBI::dbQuoteString(con, path), ", header=true)"
    ))
  }
  DBI::dbExecute(con, paste0(
    "CREATE VIEW established AS SELECT * FROM read_csv(",
    DBI::dbQuoteString(con, path),
    ", columns={'k':'BIGINT','label':'VARCHAR'},",
    " auto_detect=false, header=true)"
  ))
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  for (sql in c(
    "SELECT * FROM ordinary", "TABLE metadata.safe", "TABLE established",
    "WITH ordinary AS (SELECT 101 AS k, 'east' AS label) TABLE ordinary",
    paste("SELECT * FROM (WITH ordinary AS",
          "(SELECT 101 AS k, 'east' AS label) TABLE ordinary) AS nested"),
    "SELECT * FROM (VALUES (101, 'east'), (102, 'west')) AS t(k, label)"
  )) {
    input <- dplyr::tbl(con, dbplyr::sql(sql), vars = c("k", "label"))
    before <- serialize(input$lazy_query, NULL)
    plan <- inspect_grouping(
      input, .by = where(is.character), .grouping = rollup(where(is.numeric))
    )
    expect_identical(plan$fixed, c("(label)", "(label)"))
    expect_identical(plan$included, c("(k)", "()"))
    audit <- last_sent_queries(details = TRUE)
    expected <- c(if (!grepl("WITH|VALUES", sql)) "relation_catalog",
                  "selection_proxy")
    expect_identical(audit$purpose, expected)
    expect_true(all(audit$status == "returned"))
    expect_identical(sum(audit$purpose == "selection_proxy"), 1L)
    expect_identical(serialize(input$lazy_query, NULL), before)
  }
  # Established types permit inspection, while explicit execution sees bad data.
  expect_error(dplyr::collect(dplyr::tbl(con, "established")),
               "Conversion Error", fixed = TRUE)
})
