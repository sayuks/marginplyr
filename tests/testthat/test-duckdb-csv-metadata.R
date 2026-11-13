test_that("non-ASCII catalog and CTE names cannot admit a distinct CSV file", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  directory <- tempfile()
  dir.create(directory)
  previous <- setwd(directory)
  on.exit({
    setwd(previous)
    unlink(directory, recursive = TRUE)
  }, add = TRUE)
  path <- "\u00e4.csv"
  table <- "\u00c4.csv"
  DBI::dbWriteTable(con, table, data.frame(k = 999L))
  identifier <- DBI::dbQuoteIdentifier(con, path)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  for (sql in c(
    paste("SELECT * FROM", identifier),
    paste0("WITH ", DBI::dbQuoteIdentifier(con, table),
           " AS (SELECT 999 AS k) SELECT * FROM ", identifier)
  )) {
    source <- dplyr::tbl(con, dbplyr::sql(sql), vars = "k")
    before <- serialize(source$lazy_query, NULL)
    expect_identical(inspect_grouping(
      source, .grouping = grouping_set(k)
    )$included, "(k)")
    expect_equal(nrow(last_sent_queries()), 0L)
    for (value in c("101", "source_tail")) {
      writeLines(c("k", value), path)
      expect_identical(is.numeric(dplyr::collect(utils::head(source, 0L))$k),
                       identical(value, "101"))
      expect_error(inspect_grouping(
        source, .grouping = grouping_set(where(is.numeric))
      ), "source rows", fixed = TRUE, class = "marginplyr_error")
      expect_duckdb_catalog_only()
    }
    expect_identical(serialize(source$lazy_query, NULL), before)
    unlink(path)
  }
})

test_that("view admission cannot hide source-value CSV inference", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("k", "101"), path)
  quoted <- DBI::dbQuoteString(con, path)
  DBI::dbExecute(con, paste(
    'CREATE VIEW "ordinary.csv" AS SELECT * FROM', quoted
  ))
  DBI::dbExecute(con, 'CREATE VIEW "nested.csv" AS TABLE "ordinary.csv"')
  DBI::dbExecute(con, paste0(
    'CREATE VIEW "reader.csv" AS SELECT * FROM read_csv_auto(', quoted, ")"
  ))
  DBI::dbExecute(con, paste(
    "CREATE MACRO hidden_reader() AS TABLE SELECT * FROM", quoted
  ))
  DBI::dbExecute(con, paste(
    'CREATE VIEW "macro.csv" AS SELECT * FROM hidden_reader()'
  ))
  DBI::dbExecute(con, paste0(
    "CREATE MACRO hidden_scalar() AS (SELECT k FROM ", quoted, ")"
  ))
  DBI::dbExecute(con, 'CREATE VIEW "scalar.csv" AS SELECT hidden_scalar() AS k')
  DBI::dbExecute(con, paste0(
    'CREATE VIEW "query.csv" AS WITH query AS (SELECT 999 AS k) ',
    "SELECT * FROM query(",
    DBI::dbQuoteString(con, paste("SELECT * FROM", quoted)), ")"
  ))
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  unlink(path)
  for (name in c("ordinary.csv", "nested.csv", "reader.csv", "macro.csv",
                 "scalar.csv", "query.csv")) {
    source <- dplyr::tbl(con, dbplyr::sql(paste(
      "SELECT * FROM", DBI::dbQuoteIdentifier(con, name)
    )), vars = "k")
    before <- serialize(source$lazy_query, NULL)
    expect_identical(inspect_grouping(
      source, .grouping = grouping_set(k)
    )$included, "(k)")
    expect_equal(nrow(last_sent_queries()), 0L)
    for (value in c("101", "source_tail")) {
      writeLines(c("k", value), path)
      expect_identical(is.numeric(dplyr::collect(utils::head(source, 0L))$k),
                       identical(value, "101"))
      expect_error(inspect_grouping(
        source, .grouping = grouping_set(where(is.numeric))
      ), class = "marginplyr_error")
      expect_duckdb_catalog_only()
      expect_error(inspect_grouping(
        source, .by = where(is.numeric)
      ), "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
      expect_duckdb_catalog_only()
    }
    expect_identical(serialize(source$lazy_query, NULL), before)
    unlink(path)
  }
})

test_that("view dependencies resolve within their own schema before callers", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("k", "101"), path)
  DBI::dbExecute(con, "CREATE SCHEMA metadata")
  DBI::dbWriteTable(con, "local_source", data.frame(k = 999L))
  DBI::dbWriteTable(con, DBI::Id(schema = "metadata", table = "safe_source"),
                    data.frame(k = 101L))
  for (name in c("metadata.local_source", "main.safe_source")) {
    DBI::dbExecute(con, paste(
      "CREATE VIEW", name, "AS SELECT * FROM", DBI::dbQuoteString(con, path)
    ))
  }
  DBI::dbExecute(con, paste(
    'CREATE VIEW metadata."unsafe.csv" AS TABLE local_source'
  ))
  DBI::dbExecute(con, paste(
    'CREATE VIEW metadata."safe.csv" AS SELECT abs(k) AS k FROM safe_source'
  ))
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  for (value in c("101", "source_tail")) {
    writeLines(c("k", value), path)
    unsafe <- dplyr::tbl(con, dbplyr::sql(paste(
      'WITH local_source AS (SELECT 999 AS k) TABLE metadata."unsafe.csv"'
    )), vars = "k")
    expect_identical(is.numeric(dplyr::collect(utils::head(unsafe, 0L))$k),
                     identical(value, "101"))
    expect_error(inspect_grouping(
      unsafe, .grouping = grouping_set(where(is.numeric))
    ), "source rows", fixed = TRUE, class = "marginplyr_error")
    expect_duckdb_catalog_only()
    safe <- dplyr::tbl(
      con, dbplyr::sql('TABLE metadata."safe.csv"'), vars = "k"
    )
    expect_identical(inspect_grouping(
      safe, .grouping = grouping_set(where(is.numeric))
    )$included, "(k)")
    expect_identical(last_sent_queries(details = TRUE)$purpose,
                     c("relation_catalog", "selection_proxy"))
  }
})

test_that("view alias and CTE declarations do not call same-named macros", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "src", data.frame(k = 101L))
  DBI::dbExecute(con, "CREATE MACRO f(x) AS x")
  DBI::dbExecute(con, 'CREATE MACRO "as"(x) AS x')
  DBI::dbExecute(con, 'CREATE MACRO "in"(x) AS x')
  DBI::dbExecute(con, "CREATE MACRO over(x) AS x")
  DBI::dbExecute(con, "CREATE MACRO filter(x) AS x")
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  for (sql in c(
    "SELECT * FROM src AS f(k)",
    "SELECT * FROM src f(k)",
    'SELECT * FROM src AS "F"(k)',
    "WITH f(k) AS (SELECT k FROM src) SELECT * FROM f",
    "WITH c AS (SELECT k FROM src) SELECT * FROM c",
    "SELECT k FROM src WHERE k IN (101)",
    "SELECT sum(k) OVER () AS k FROM src",
    "SELECT sum(k) FILTER (WHERE k > 0) AS k FROM src"
  )) {
    DBI::dbExecute(con, paste('CREATE OR REPLACE VIEW "alias.csv" AS', sql))
    source <- dplyr::tbl(con, dbplyr::sql('TABLE "alias.csv"'), vars = "k")
    before <- serialize(source$lazy_query, NULL)
    expect_true(is.numeric(dplyr::collect(utils::head(source, 0L))$k))
    expect_identical(inspect_grouping(
      source, .grouping = grouping_set(where(is.numeric))
    )$included, "(k)")
    audit <- last_sent_queries(details = TRUE)
    expect_identical(audit$purpose, c("relation_catalog", "selection_proxy"))
    expect_identical(audit$status, c("returned", "returned"))
    expect_identical(serialize(source$lazy_query, NULL), before)
  }
  for (sql in c(
    "SELECT f(k) AS k FROM src AS f(k)",
    "WITH f(k) AS (SELECT k FROM src) SELECT f(k) AS k FROM f",
    'SELECT "as"(k) AS k FROM src',
    'SELECT "in"(k) AS k FROM src',
    "SELECT main.as(k) AS k FROM src",
    "SELECT main.in(k) AS k FROM src",
    "SELECT over(k) AS k FROM src",
    "SELECT filter(k) AS k FROM src"
  )) {
    DBI::dbExecute(con, paste('CREATE OR REPLACE VIEW "alias.csv" AS', sql))
    source <- dplyr::tbl(con, dbplyr::sql('TABLE "alias.csv"'), vars = "k")
    expect_true(is.numeric(dplyr::collect(utils::head(source, 0L))$k))
    expect_error(inspect_grouping(
      source, .grouping = grouping_set(where(is.numeric))
    ), "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
    expect_duckdb_catalog_only()
  }
})

test_that("view star modifiers do not call unrelated same-named macros", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "src", data.frame(k = 101L, other = 2L))
  for (name in c("exclude", "replace", "rename", "columns", "rollup", "cube",
                 "sets", "grouping")) {
    DBI::dbExecute(con, paste(
      "CREATE MACRO", DBI::dbQuoteIdentifier(con, name), "(x) AS x"
    ))
  }
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  for (sql in c(
    "SELECT * EXCLUDE (other) FROM src",
    "SELECT src.* EXCLUDE (other) FROM src",
    "SELECT k FROM (SELECT * REPLACE (k+1 AS k) FROM src)",
    "SELECT renamed AS k FROM (SELECT * RENAME (other AS renamed) FROM src)",
    "SELECT k FROM (SELECT COLUMNS(*) FROM src)",
    "SELECT COLUMNS(* EXCLUDE (other)) FROM src",
    "SELECT COLUMNS('k') FROM src",
    "SELECT k FROM src GROUP BY ROLLUP(k)",
    "SELECT k FROM src GROUP BY CUBE(k)",
    "SELECT k FROM src GROUP BY GROUPING SETS ((k), ())",
    "SELECT k FROM src GROUP BY GROUPING SETS (ROLLUP(k), CUBE(other))",
    "SELECT grouping(k) AS k FROM src GROUP BY ROLLUP(k)"
  )) {
    DBI::dbExecute(con, paste('CREATE OR REPLACE VIEW "star.csv" AS', sql))
    source <- dplyr::tbl(con, dbplyr::sql('TABLE "star.csv"'), vars = "k")
    before <- serialize(source$lazy_query, NULL)
    expect_true(is.numeric(dplyr::collect(utils::head(source, 0L))$k))
    expect_identical(inspect_grouping(
      source, .grouping = grouping_set(where(is.numeric))
    )$included, "(k)")
    audit <- last_sent_queries(details = TRUE)
    expect_identical(audit$purpose, c("relation_catalog", "selection_proxy"))
    expect_identical(audit$status, c("returned", "returned"))
    expect_identical(serialize(source$lazy_query, NULL), before)
  }
  for (sql in c(
    "SELECT k * exclude(k) AS k FROM src",
    "SELECT k * replace(k) AS k FROM src",
    "SELECT k * rename(k) AS k FROM src",
    'SELECT "columns"(k) AS k FROM src',
    "SELECT main.columns(k) AS k FROM src",
    "SELECT rollup(k) AS k FROM src",
    "SELECT cube(k) AS k FROM src",
    'SELECT "sets"(k) AS k FROM src',
    "SELECT main.grouping(k) AS k FROM src",
    "SELECT k FROM src GROUP BY k + rollup(k), k"
  )) {
    DBI::dbExecute(con, paste('CREATE OR REPLACE VIEW "star.csv" AS', sql))
    source <- dplyr::tbl(con, dbplyr::sql('TABLE "star.csv"'), vars = "k")
    expect_true(is.numeric(dplyr::collect(utils::head(source, 0L))$k))
    expect_error(inspect_grouping(
      source, .grouping = grouping_set(where(is.numeric))
    ), "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
    expect_duckdb_catalog_only()
  }
})

test_that("stored explicit CSV reader metadata survives view normalization", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("k", "not_an_integer"), path)
  statement <- paste0(
    'CREATE OR REPLACE VIEW "safe.csv" AS SELECT * FROM read_csv(',
    DBI::dbQuoteString(con, path),
    ", columns={'k':'BIGINT'}, auto_detect=false, header=true)"
  )
  DBI::dbExecute(con, statement)
  source <- dplyr::tbl(con, dbplyr::sql('TABLE "safe.csv"'), vars = "k")
  before <- serialize(source$lazy_query, NULL)
  expect_true(is.numeric(dplyr::collect(utils::head(source, 0L))$k))
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  expect_identical(inspect_grouping(
    source, .grouping = grouping_set(where(is.numeric))
  )$included, "(k)")
  expect_identical(last_sent_queries(details = TRUE)$purpose,
                   c("relation_catalog", "selection_proxy"))
  expect_identical(last_sent_queries(details = TRUE)$status,
                   c("returned", "returned"))
  expect_error(dplyr::collect(source), "Conversion Error", fixed = TRUE)
  expect_identical(serialize(source$lazy_query, NULL), before)
  DBI::dbExecute(con, paste0(
    'CREATE OR REPLACE VIEW "safe.csv" AS SELECT * FROM read_csv(',
    DBI::dbQuoteString(con, path),
    ", columns={'k':'BIGINT'}, auto_detect=(false OR true), header=true)"
  ))
  expect_error(inspect_grouping(
    source, .grouping = grouping_set(where(is.numeric))
  ), "source rows", fixed = TRUE, class = "marginplyr_error")
  expect_duckdb_catalog_only()
  DBI::dbExecute(con, statement)
  DBI::dbExecute(con, "CREATE MACRO struct_pack(k) AS k")
  expect_error(inspect_grouping(
    source, .grouping = grouping_set(where(is.numeric))
  ), "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
  expect_duckdb_catalog_only()
})

test_that("reader-like catalog relations and CTEs retain typed inspection", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA metadata")
  for (name in c("ordinary.csv", "ordinary.tsv", "ordinary.json",
                 "ordinary.jsonl", "ordinary.ndjson")) {
    identifier <- DBI::dbQuoteIdentifier(con, name)
    DBI::dbWriteTable(con, name, data.frame(k = 101L, label = "east"))
    DBI::dbWriteTable(con, DBI::Id(schema = "metadata", table = name),
                      data.frame(k = 101L, label = "east"))
    for (sql in c(
      paste("SELECT * FROM", identifier),
      paste0("SELECT * FROM metadata.", identifier),
      paste0("SELECT * FROM memory.metadata.", identifier),
      paste0("SELECT * FROM memory.", identifier),
      paste0("WITH ", identifier,
             " AS (SELECT 101 AS k, 'east' AS label) SELECT * FROM ",
             identifier)
    )) {
      source <- dplyr::tbl(con, dbplyr::sql(sql), vars = c("k", "label"))
      before <- serialize(source$lazy_query, NULL)
      plan <- inspect_grouping(
        source, .by = where(is.character),
        .grouping = grouping_set(where(is.numeric))
      )
      expect_identical(plan$fixed, "(label)")
      expect_identical(plan$included, "(k)")
      audit <- last_sent_queries(details = TRUE)
      expect_identical(sum(audit$purpose == "selection_proxy"), 1L)
      expect_identical(audit$purpose, if (startsWith(sql, "WITH")) {
        "selection_proxy"
      } else {
        c("relation_catalog", "selection_proxy")
      })
      expect_true(all(audit$status == "returned"))
      expect_identical(serialize(source$lazy_query, NULL), before)
      joined <- dplyr::left_join(source, dplyr::select(source, k), by = "k")
      expect_identical(inspect_grouping(
        joined, .by = where(is.character),
        .grouping = grouping_set(where(is.numeric))
      )$included, "(k)")
      # The rendered join's call-shaped heads are looked up in the catalog.
      expect_identical(last_sent_queries(details = TRUE)$purpose,
                       c("relation_catalog", "selection_proxy"))
    }
  }
})

test_that("reader-like relations follow search paths and temporary catalogs", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA metadata")
  DBI::dbExecute(con, "SET search_path = 'metadata,main'")
  DBI::dbExecute(con, 'CREATE TABLE metadata."schema.csv" AS SELECT 101 AS k')
  DBI::dbExecute(con, 'CREATE TEMP TABLE "temporary.json" AS SELECT 101 AS k')
  DBI::dbExecute(con, 'CREATE VIEW metadata."view.json" AS SELECT 101 AS k')
  DBI::dbExecute(con, "CREATE MACRO current_database() AS 'unrelated'")
  DBI::dbExecute(con, "CREATE MACRO current_setting(setting) AS 'unrelated'")
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  for (name in c("schema.csv", "temporary.json", "view.json")) {
    source <- dplyr::tbl(con, dbplyr::sql(paste(
      "SELECT * FROM", DBI::dbQuoteIdentifier(con, name)
    )), vars = "k")
    expect_identical(inspect_grouping(
      source, .grouping = grouping_set(where(is.numeric))
    )$included, "(k)")
    audit <- last_sent_queries(details = TRUE)
    expect_identical(audit$purpose, c("relation_catalog", "selection_proxy"))
    expect_identical(audit$status, c("returned", "returned"))
  }
})

test_that("reader-like CTE names retain their SQL scope", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  for (name in c("local.csv", "local.json")) {
    identifier <- DBI::dbQuoteIdentifier(con, name)
    for (sql in c(
      paste0("WITH ", identifier, " AS (SELECT 101 AS k) TABLE ", identifier),
      paste0("WITH ", identifier, "(k) AS MATERIALIZED (SELECT 101) ",
             "SELECT * FROM ", identifier),
      paste0("SELECT * FROM (WITH ", identifier,
             " AS NOT MATERIALIZED (SELECT 101 AS k) TABLE ", identifier,
             ") AS nested"),
      paste0("WITH RECURSIVE ", identifier,
             "(k) AS (SELECT 101 UNION ALL SELECT k + 1 FROM ", identifier,
             " WHERE k < 102) SELECT * FROM ", identifier),
      paste0("WITH ", identifier, " AS (SELECT 101 AS k), derived AS (TABLE ",
             identifier, ") TABLE derived")
    )) {
      source <- dplyr::tbl(con, dbplyr::sql(sql), vars = "k")
      expect_identical(inspect_grouping(
        source, .grouping = grouping_set(where(is.numeric))
      )$included, "(k)")
      audit <- last_sent_queries(details = TRUE)
      expect_identical(audit$purpose, "selection_proxy")
      expect_identical(audit$status, "returned")
    }
  }
})

test_that("unresolved files stay refused outside CTE and catalog visibility", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  identifier <- DBI::dbQuoteIdentifier(con, path)
  DBI::dbExecute(con, "CREATE SCHEMA hidden")
  DBI::dbWriteTable(con, DBI::Id(schema = "hidden", table = path),
                    data.frame(k = 999L))
  DBI::dbExecute(con, "ATTACH ':memory:' AS other")
  DBI::dbWriteTable(
    con, DBI::Id(catalog = "other", schema = "main", table = path),
    data.frame(k = 999L)
  )
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  for (sql in c(
    paste("SELECT * FROM", identifier),
    paste0("WITH ", identifier, " AS (SELECT * FROM ", identifier,
           ") SELECT * FROM ", identifier),
    paste0("SELECT file.k FROM (WITH ", identifier,
           " AS (SELECT 1 AS k) TABLE ", identifier,
           ") AS bounded CROSS JOIN ", identifier, " AS file")
  )) {
    source <- dplyr::tbl(con, dbplyr::sql(sql), vars = "k")
    before <- serialize(source$lazy_query, NULL)
    expect_identical(inspect_grouping(
      source, .grouping = grouping_set(k)
    )$included, "(k)")
    expect_equal(nrow(last_sent_queries()), 0L)
    for (value in c("101", "source_tail")) {
      writeLines(c("k", value), path)
      native <- dplyr::collect(utils::head(source, 0L))
      expect_identical(is.numeric(native$k), identical(value, "101"))
      expect_error(inspect_grouping(
        source, .grouping = grouping_set(where(is.numeric))
      ), "source rows", fixed = TRUE, class = "marginplyr_error")
      expect_duckdb_catalog_only()
    }
    expect_identical(serialize(source$lazy_query, NULL), before)
    unlink(path)
  }
})

test_that("CSV replacement inspection refuses source-value inference", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  source <- dplyr::tbl(con, dbplyr::sql(paste0(
    "SELECT * FROM ", DBI::dbQuoteString(con, path)
  )), vars = "k")
  before <- serialize(source$lazy_query, NULL)
  established <- dplyr::tbl(con, dbplyr::sql("SELECT 101 AS k"), vars = "k")
  inputs <- list(
    source, dplyr::filter(source, k == k),
    dplyr::left_join(established, source, by = "k"),
    dplyr::left_join(source, established, by = "k")
  )
  expect_identical(
    inspect_grouping(source, .grouping = grouping_set(k))$included, "(k)"
  )
  expect_equal(nrow(last_sent_queries(details = TRUE)), 0L)

  for (value in c("101", "source_tail")) {
    writeLines(c("k", value), path)
    # Only the data row changes, exposing inference hidden by LIMIT 0.
    native <- dplyr::collect(utils::head(source, 0L))
    expect_identical(is.numeric(native$k), identical(value, "101"))
    for (input in inputs) {
      input_before <- serialize(input$lazy_query, NULL)
      expect_error(
        inspect_grouping(input, .by = where(is.numeric)),
        "source rows", fixed = TRUE, class = "marginplyr_error"
      )
      expect_duckdb_catalog_only()
      expect_error(
        inspect_grouping(input, .grouping = grouping_set(where(is.numeric))),
        "dplyr::collect()", fixed = TRUE, class = "marginplyr_error"
      )
      expect_duckdb_catalog_only()
      expect_identical(serialize(input$lazy_query, NULL), input_before)
    }
    expect_identical(as.character(dplyr::collect(source)$k), value)
  }
  unlink(path)
  expect_error(
    inspect_grouping(source, .by = where(is.numeric)),
    "source rows", fixed = TRUE, class = "marginplyr_error"
  )
  expect_identical(serialize(source$lazy_query, NULL), before)
})

test_that("CSV replacement guards follow SQL relation positions", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("k", "101"), path)
  quoted <- DBI::dbQuoteString(con, path)
  identifier <- DBI::dbQuoteIdentifier(con, path)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  for (sql in c(
    paste0("TABLE ", identifier),
    paste0("SELECT * FROM (TABLE ", identifier, ") AS t"),
    paste0("TABLE ", identifier, " UNION ALL TABLE ", identifier),
    paste0("FROM ", quoted),
    paste0("SELECT t.k FROM (SELECT 1 AS v) AS t0 JOIN ", quoted,
           " AS t ON true"),
    paste0("SELECT t.k FROM (SELECT 1 AS v) AS t0, ", quoted, " AS t"),
    paste0("SELECT k FROM (", identifier,
           " CROSS JOIN (SELECT 1 AS v) AS u)"),
    paste0("PIVOT ", quoted, " ON k IN (101) USING count(*)")
  )) {
    vars <- if (startsWith(sql, "PIVOT")) "101" else "k"
    source <- dplyr::tbl(con, dbplyr::sql(sql), vars = vars)
    expect_true(is.numeric(dplyr::collect(utils::head(source, 0L))[[1L]]))
    expect_identical(
      inspect_grouping(source, .grouping = grouping_set(everything()))$included,
      paste0("(", vars, ")")
    )
    expect_error(
      inspect_grouping(source, .grouping = grouping_set(where(is.numeric))),
      "source rows", fixed = TRUE, class = "marginplyr_error"
    )
    expect_duckdb_catalog_only()
  }
})

test_that("neighbouring relation and CTE spellings refuse CSV before binding", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  quoted <- DBI::dbQuoteString(con, path)
  identifier <- DBI::dbQuoteIdentifier(con, path)
  split <- DBI::dbQuoteString(con, sub("sv$", "", path))
  DBI::dbWriteTable(con, "by_name_source", data.frame(k = 101L))
  writeLines(c("k", "101"), path)
  # The stored definition keeps RECURSIVE and the anchor-term file reference.
  DBI::dbExecute(con, paste0(
    "CREATE VIEW anchored AS WITH RECURSIVE ", identifier, "(k) AS ",
    "(SELECT * FROM ", identifier, " UNION SELECT k FROM ", identifier,
    " WHERE false) SELECT * FROM ", identifier
  ))
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  refused <- list(
    list("SELECT * FROM anchored", "k"),
    list(paste0("SELECT 'x' AS g WHERE false UNION ALL BY NAME TABLE ",
                identifier), c("g", "k")),
    list(paste0("SELECT 'x' AS g WHERE false UNION BY NAME TABLE ",
                identifier), c("g", "k")),
    list(paste0("SELECT * FROM (FROM by_name_source UNION ALL BY NAME FROM ",
                quoted, ")"), "k"),
    list(paste0("SELECT * FROM (TABLE by_name_source UNION ALL BY NAME FROM ",
                quoted, ")"), "k"),
    # DuckDB concatenates string literals separated by a line break.
    list(paste0("SELECT * FROM ", split, "\n'sv'"), "k"),
    list(paste0("SELECT * FROM ", split, " -- continued\n  'sv'"), "k"),
    # A recursive CTE's own name is visible only in its recursive term.
    list(paste0("WITH RECURSIVE ", identifier, " AS (SELECT * FROM ",
                identifier, ") SELECT * FROM ", identifier), "k"),
    list(paste0("WITH RECURSIVE ", identifier, "(k) AS (SELECT * FROM ",
                identifier, " UNION SELECT k FROM ", identifier,
                " WHERE false) SELECT * FROM ", identifier), "k"),
    # USING KEY precedes AS without ending the CTE header.
    list(paste0("WITH RECURSIVE r(z) USING KEY (z) AS (SELECT 1 AS z UNION ",
                "SELECT z + 1 FROM r WHERE z < 1) TABLE ", identifier), "k")
  )
  for (value in c("101", "source_tail")) {
    writeLines(c("k", value), path)
    for (case in refused) {
      source <- dplyr::tbl(con, dbplyr::sql(case[[1L]]), vars = case[[2L]])
      expect_error(
        inspect_grouping(source, .grouping = grouping_set(where(is.numeric))),
        "source rows", fixed = TRUE, class = "marginplyr_error"
      )
      expect_duckdb_catalog_only()
      expect_error(
        summarize_with_margins(source, n = dplyr::n(), .grouping = rollup(k)),
        "DuckDB CSV input", fixed = TRUE, class = "marginplyr_error"
      )
      expect_duckdb_catalog_only()
    }
  }

  # Tables and explicit-schema readers keep one typed plan by name.
  explicit <- paste0(
    "read_csv(", quoted, ", columns = {'k': 'VARCHAR'}, auto_detect = false,",
    " header = true)"
  )
  for (value in c("101", "source_tail")) {
    writeLines(c("k", value), path)
    for (operand in c("TABLE by_name_source", paste("FROM", explicit))) {
      source <- dplyr::tbl(con, dbplyr::sql(paste0(
        "SELECT * FROM (FROM by_name_source UNION ALL BY NAME ", operand, ")"
      )), vars = "k")
      expect_identical(inspect_grouping(
        source, .grouping = grouping_set(where(is.character))
      )$included, if (startsWith(operand, "FROM")) "(k)" else "()")
      expect_identical(last_sent_queries(details = TRUE)$purpose,
                       c("relation_catalog", "selection_proxy"))
    }
  }
  # A continued literal outside a relation position stays a scalar string,
  # and a keyed recursion's self-reference in its recursive term is the CTE.
  for (case in list(
    list(paste0("SELECT ", split, "\n'sv' AS s, k FROM by_name_source"),
         c("s", "k")),
    list(paste0("WITH RECURSIVE ", identifier, "(k) USING KEY (k) AS ",
                "(SELECT 101 UNION SELECT k + 1 FROM ", identifier,
                " WHERE k < 102) TABLE ", identifier), "k")
  )) {
    source <- dplyr::tbl(con, dbplyr::sql(case[[1L]]), vars = case[[2L]])
    expect_identical(inspect_grouping(
      source, .grouping = grouping_set(where(is.numeric))
    )$included, "(k)")
    expect_identical(last_sent_queries(details = TRUE)$purpose,
                     c("relation_catalog", "selection_proxy"))
  }
})

test_that("PIVOT synonyms and file statements refuse CSV before binding", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  wide <- function(keyword, source) {
    dplyr::tbl(con, dbplyr::sql(paste(
      keyword, source, "ON g IN ('a', 'b') USING first(v) GROUP BY k"
    )), vars = c("k", "a", "b"))
  }
  long <- function(keyword, source) {
    dplyr::tbl(con, dbplyr::sql(paste(
      keyword, source, "ON v INTO NAME name VALUE value"
    )), vars = c("k", "g", "name", "value"))
  }
  stem <- sub("[.]csv$", "", path)
  spellings <- c(
    DBI::dbQuoteString(con, path), DBI::dbQuoteIdentifier(con, path),
    DBI::dbQuoteString(con, paste0(stem, ".tsv.gz")),
    DBI::dbQuoteString(con, paste0(stem, ".csv.zst")),
    DBI::dbQuoteString(con, file.path(dirname(path), "*.csv")),
    paste0("$path$", path, "$path$")
  )
  writeLines(c("k,g,v", "x,a,1", "x,b,2"), path)
  for (spelling in spellings) {
    for (input in list(wide("PIVOT_WIDER", spelling),
                       long("pivot_longer", spelling))) {
      expect_error(
        inspect_grouping(input, .grouping = grouping_set(where(is.numeric))),
        "DuckDB CSV input", fixed = TRUE, class = "marginplyr_error"
      )
      expect_duckdb_catalog_only()
      expect_error(
        inspect_grouping(input, .by = where(is.numeric)),
        "DuckDB CSV input", fixed = TRUE, class = "marginplyr_error"
      )
      expect_duckdb_catalog_only()
      expect_error(
        summarize_with_margins(input, n = dplyr::n(), .grouping = rollup(k)),
        "DuckDB CSV input", fixed = TRUE, class = "marginplyr_error"
      )
      expect_duckdb_catalog_only()
      expect_error(
        expand_with_margins(input, .grouping = rollup(k)),
        "DuckDB CSV input", fixed = TRUE, class = "marginplyr_error"
      )
      expect_duckdb_catalog_only()
    }
  }
  # The JSON family is decided by suffix, as it is for PIVOT.
  json <- DBI::dbQuoteString(con, paste0(stem, ".json"))
  for (keyword in c("PIVOT", "PIVOT_WIDER")) {
    expect_error(
      inspect_grouping(wide(keyword, json), .by = where(is.numeric)),
      "DuckDB JSON input", fixed = TRUE, class = "marginplyr_error"
    )
    expect_duckdb_catalog_only()
  }
  # SUMMARIZE, DESCRIBE and SHOW sniff a CSV file to report its columns.
  for (keyword in c("SUMMARIZE", "DESCRIBE", "SHOW")) {
    statement <- dplyr::tbl(con, dbplyr::sql(paste0(
      "SELECT column_name, column_type FROM (", keyword, " ",
      DBI::dbQuoteString(con, path), ")"
    )), vars = c("column_name", "column_type"))
    expect_error(
      inspect_grouping(statement, .by = where(is.character)),
      "DuckDB CSV input", fixed = TRUE, class = "marginplyr_error"
    )
    expect_duckdb_catalog_only()
  }

  # Established schemas keep one typed plan whatever the file holds.
  DBI::dbWriteTable(con, "pivot_source", data.frame(
    k = "x", g = c("a", "b"), v = c("1", "2")
  ))
  explicit <- paste0(
    "read_csv(", DBI::dbQuoteString(con, path),
    ", columns = {'k': 'VARCHAR', 'g': 'VARCHAR', 'v': 'VARCHAR'},",
    " auto_detect = false, header = true)"
  )
  for (value in c("2", "source_tail")) {
    writeLines(c("k,g,v", "x,a,1", paste0("x,b,", value)), path)
    for (source in c("pivot_source", explicit)) {
      for (keyword in c("PIVOT_WIDER", "PIVOT_LONGER")) {
        input <- if (keyword == "PIVOT_WIDER") wide(keyword, source) else
          long(keyword, source)
        expect_identical(inspect_grouping(
          input, .grouping = grouping_set(where(is.character))
        )$included, if (keyword == "PIVOT_WIDER") "(k, a, b)" else
          "(k, g, name, value)")
        expect_identical(last_sent_queries(details = TRUE)$purpose,
                         c("relation_catalog", "selection_proxy"))
      }
    }
  }
  for (keyword in c("SUMMARIZE", "DESCRIBE", "SHOW")) {
    statement <- dplyr::tbl(con, dbplyr::sql(paste0(
      "SELECT column_name, column_type FROM (", keyword, " pivot_source)"
    )), vars = c("column_name", "column_type"))
    expect_identical(inspect_grouping(
      statement, .grouping = grouping_set(where(is.character))
    )$included, "(column_name, column_type)")
  }
})

test_that("CSV replacement guards preserve ordinary values and non-CSV files", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- tempfile(fileext = ".parquet")
  on.exit(unlink(path), add = TRUE)
  DBI::dbExecute(con, paste0(
    "COPY (SELECT 101 AS k, 'east.csv' AS label) TO ",
    DBI::dbQuoteString(con, path), " (FORMAT PARQUET)"
  ))
  parquet <- dplyr::tbl(con, dbplyr::sql(paste0(
    "SELECT * FROM ", DBI::dbQuoteString(con, path)
  )), vars = c("k", "label"))
  literal <- dplyr::tbl(con, dbplyr::sql(paste(
    'SELECT substring(coalesce("table", \'absent.csv\') FROM 1) AS label,',
    "101 AS k FROM (VALUES ('east', $$absent.tsv$$, E'absent.csv.gz'))",
    'AS t("table", a, b)',
    "WHERE 'absent.csv' = 'absent.csv' /* FROM 'absent.csv' */"
  )), vars = c("label", "k"))
  ordinary <- lapply(c("report.csv.zstd", "report.csv'"), function(name) {
    dplyr::copy_to(con, tibble::tibble(k = 101L, label = "east.csv"), name)
    dplyr::tbl(con, dbplyr::sql(paste(
      "SELECT * FROM", DBI::dbQuoteIdentifier(con, name)
    )), vars = c("k", "label"))
  })
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  inputs <- c(list(literal = literal, parquet = parquet), ordinary)
  for (i in seq_along(inputs)) {
    input <- inputs[[i]]
    plan <- inspect_grouping(
      input, .by = where(is.character),
      .grouping = grouping_set(where(is.numeric))
    )
    expect_identical(plan$fixed, "(label)")
    expect_identical(plan$included, "(k)")
    audit <- last_sent_queries(details = TRUE)
    expect_identical(audit$purpose, c("relation_catalog", "selection_proxy"))
    expect_true(all(audit$status == "returned"))
    expect_match(audit$sql[audit$purpose == "selection_proxy"],
                 "LIMIT 0", fixed = TRUE)
  }
})

test_that("CSV relation parsing keeps quoted identifiers out of SQL grammar", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  for (name in c(
    "select", "from", "join", "pivot", "unpivot", "table", "where",
    "order", "union", "all", "(", ")", ","
  )) {
    identifier <- DBI::dbQuoteIdentifier(con, name)
    literal <- dplyr::tbl(con, dbplyr::sql(paste0(
      "SELECT coalesce(", identifier, ", 'absent.csv') AS label,101 AS k ",
      "FROM (VALUES ('east')) AS control(", identifier, ")"
    )), vars = c("label", "k"))
    expect_true(is.numeric(dplyr::collect(utils::head(literal, 0L))$k))
    plan <- inspect_grouping(
      literal, .by = where(is.character),
      .grouping = grouping_set(where(is.numeric))
    )
    expect_identical(plan$fixed, "(label)")
    expect_identical(plan$included, "(k)")
    expect_identical(last_sent_queries(details = TRUE)$purpose,
                     c("relation_catalog", "selection_proxy"))
  }
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  for (name in c("select", "where", "(", ")", ",")) {
    DBI::dbWriteTable(con, name, data.frame(z = 1L))
    source <- dplyr::tbl(con, dbplyr::sql(paste0(
      "SELECT file.k FROM ", DBI::dbQuoteIdentifier(con, name),
      " AS ordinary JOIN ", DBI::dbQuoteString(con, path),
      " AS file ON true"
    )), vars = "k")
    before <- serialize(source$lazy_query, NULL)
    for (value in c("101", "source_tail")) {
      writeLines(c("k", value), path)
      native <- dplyr::collect(utils::head(source, 0L))
      expect_identical(is.numeric(native$k), identical(value, "101"))
      expect_error(
        inspect_grouping(source, .by = where(is.numeric)),
        "source rows", fixed = TRUE, class = "marginplyr_error"
      )
      expect_duckdb_catalog_only()
      expect_error(
        inspect_grouping(source, .grouping = grouping_set(where(is.numeric))),
        "source rows", fixed = TRUE, class = "marginplyr_error"
      )
      expect_duckdb_catalog_only()
    }
    expect_identical(serialize(source$lazy_query, NULL), before)
  }
})

test_that("qualified keywords preserve column and joined-relation scopes", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA metadata")
  DBI::dbWriteTable(con, DBI::Id(schema = "metadata", table = "select"),
                    data.frame(z = 1L))
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  source <- dplyr::tbl(con, dbplyr::sql(paste0(
    "SELECT file.k FROM metadata.select AS ordinary JOIN ",
    DBI::dbQuoteString(con, path), " AS file ON true"
  )), vars = "k")
  for (value in c("101", "source_tail")) {
    writeLines(c("k", value), path)
    native <- dplyr::collect(utils::head(source, 0L))
    expect_identical(is.numeric(native$k), identical(value, "101"))
    expect_error(
      inspect_grouping(source, .grouping = grouping_set(where(is.numeric))),
      "source rows", fixed = TRUE, class = "marginplyr_error"
    )
  }
  literal <- dplyr::tbl(con, dbplyr::sql(paste(
    "SELECT row_value.from AS label, 'absent.csv' AS text,101 AS k",
    "FROM (SELECT {'from':'east'} AS row_value) AS control"
  )), vars = c("label", "text", "k"))
  expect_true(is.numeric(dplyr::collect(utils::head(literal, 0L))$k))
  plan <- inspect_grouping(
    literal, .grouping = grouping_set(where(is.numeric))
  )
  expect_identical(plan$included, "(k)")
})

test_that("qualified CSV names cannot hide replacement inference", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  directory <- tempfile()
  dir.create(directory)
  previous <- setwd(directory)
  on.exit({
    setwd(previous)
    unlink(directory, recursive = TRUE)
  }, add = TRUE)
  forms <- c(
    "replacement.csv", '"replacement".csv', 'replacement."csv"',
    '"replacement"."csv"', "ONLY replacement.csv",
    "ONLY (replacement.csv)", "replacement.more.csv",
    '"replacement.more".csv', '"replacement"."more"."csv"'
  )
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  for (form in forms) {
    source <- dplyr::tbl(con, dbplyr::sql(paste(
      "SELECT * FROM", form
    )), vars = "k")
    before <- serialize(source$lazy_query, NULL)
    expect_identical(
      inspect_grouping(source, .grouping = grouping_set(k))$included, "(k)"
    )
    expect_equal(nrow(last_sent_queries(details = TRUE)), 0L)
    expect_error(
      inspect_grouping(source, .by = where(is.numeric)),
      "source rows", fixed = TRUE, class = "marginplyr_error"
    )
    expect_duckdb_catalog_only()
    expect_error(
      inspect_grouping(source, .grouping = grouping_set(where(is.numeric))),
      "source rows", fixed = TRUE, class = "marginplyr_error"
    )
    expect_duckdb_catalog_only()
    expect_identical(serialize(source$lazy_query, NULL), before)
  }
  # ONLY is guarded syntactically; DuckDB 2 rejects it as a filename relation.
  native_forms <- forms[!startsWith(forms, "ONLY")]
  paths <- c(rep("replacement.csv", 4L), rep("replacement.more.csv", 3L))
  for (i in seq_along(native_forms)) {
    for (value in c("101", "source_tail")) {
      writeLines(c("k", value), paths[[i]])
      native <- DBI::dbGetQuery(con, paste(
        "SELECT * FROM", native_forms[[i]], "LIMIT 0"
      ))
      expect_identical(is.numeric(native$k), identical(value, "101"))
    }
    unlink(paths[[i]])
  }
  DBI::dbExecute(con, "CREATE SCHEMA metadata")
  DBI::dbExecute(con, "CREATE MACRO metadata.csv() AS TABLE SELECT 101 AS k")
  DBI::dbExecute(con, 'CREATE MACRO "reader.csv"() AS TABLE SELECT 101 AS k')
  for (form in c("metadata.csv()", '"reader.csv"()')) {
    source <- dplyr::tbl(con, dbplyr::sql(paste(
      "SELECT * FROM", form
    )), vars = "k")
    # A macro's body is opaque even when its name resembles a filename.
    expect_error(inspect_grouping(
      source, .grouping = grouping_set(where(is.numeric))
    ), "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
    expect_duckdb_catalog_only()
  }
})

test_that("equivalent CSV replacement spellings keep inference disabled", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  directory <- tempfile()
  dir.create(directory)
  directory <- normalizePath(directory, winslash = "/", mustWork = TRUE)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  stem <- if (.Platform$OS.type == "windows") "source'" else "source'\""
  for (extension in c(".csv", ".tsv", ".csv.gz", ".csv.zst", ".csv?part")) {
    path <- file.path(directory, paste0(stem, extension))
    actual <- sub("?", "1", path, fixed = TRUE)
    spellings <- c(
      DBI::dbQuoteString(con, path), DBI::dbQuoteIdentifier(con, path),
      paste0("E", DBI::dbQuoteString(con, path)),
      paste0("E", gsub("''", "\\'", DBI::dbQuoteString(con, path),
                       fixed = TRUE)),
      paste0("$path$", path, "$path$")
    )
    if (extension == ".csv") {
      spellings <- c(spellings, vapply(c("\\x2e", "\\056"), function(dot) {
        paste0("E", sub(".csv", paste0(dot, "csv"),
                        DBI::dbQuoteString(con, path), fixed = TRUE))
      }, character(1)))
    }
    for (spelling in spellings) {
      source <- dplyr::tbl(con, dbplyr::sql(paste0(
        "SELECT * FROM ", spelling
      )), vars = "k")
      before <- serialize(source$lazy_query, NULL)
      expect_identical(
        inspect_grouping(source, .grouping = grouping_set(k))$included, "(k)"
      )
      expect_equal(nrow(last_sent_queries(details = TRUE)), 0L)
      expect_error(
        inspect_grouping(source, .by = where(is.numeric)),
        "source rows", fixed = TRUE, class = "marginplyr_error"
      )
      expect_duckdb_catalog_only()
      expect_identical(serialize(source$lazy_query, NULL), before)
    }
    # E and dollar strings remain reader arguments on both DuckDB 1.5 and 2;
    # DuckDB 2 rejects them as filename relations.
    controls <- ifelse(
      startsWith(spellings, "E") | startsWith(spellings, "$"),
      paste0("read_csv_auto(", spellings, ")"), spellings
    )
    # COPY writes valid compressed controls without an extra dependency.
    DBI::dbExecute(con, paste0(
      "COPY (SELECT 101 AS k) TO ", DBI::dbQuoteString(con, actual),
      " (FORMAT CSV, HEADER, COMPRESSION ",
      if (extension == ".csv.gz") "GZIP" else
        if (extension == ".csv.zst") "ZSTD" else "NONE", ")"
    ))
    for (control in controls) {
      native <- DBI::dbGetQuery(con, paste(
        "SELECT * FROM", control, "LIMIT 0"
      ))
      expect_true(is.numeric(native$k))
    }
    unlink(actual)
  }
})

test_that("escaped uppercase CSV names retain their native filename", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- file.path(
    normalizePath(tempdir(), winslash = "/", mustWork = TRUE),
    basename(tempfile(fileext = ".CSV"))
  )
  on.exit(unlink(path), add = TRUE)
  spelling <- paste0("E", sub(
    ".CSV", "\\x2e\\x43\\x53\\x56", DBI::dbQuoteString(con, path),
    fixed = TRUE
  ))
  source <- dplyr::tbl(con, dbplyr::sql(paste(
    "SELECT * FROM", spelling
  )), vars = "k")
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  before <- serialize(source$lazy_query, NULL)
  expect_identical(
    inspect_grouping(source, .grouping = grouping_set(k))$included, "(k)"
  )
  expect_equal(nrow(last_sent_queries(details = TRUE)), 0L)
  expect_error(
    inspect_grouping(source, .grouping = grouping_set(where(is.numeric))),
    "source rows", fixed = TRUE, class = "marginplyr_error"
  )
  expect_duckdb_catalog_only()
  expect_identical(serialize(source$lazy_query, NULL), before)
  writeLines(c("k", "101"), path)
  native <- DBI::dbGetQuery(con, paste0(
    "SELECT k, filename FROM read_csv_auto(", spelling, ", filename=true)"
  ))
  expect_true(is.numeric(native$k))
  expect_identical(native$filename, path)
})

test_that("typed CSV inspection refuses before DuckDB can infer source types", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)

  for (reader in c("read_csv", "read_csv_auto")) {
    for (sampling in c("", ", sample_size = -1")) {
      source <- dplyr::tbl(con, dbplyr::sql(paste0(
        "SELECT * FROM ", reader, "(", DBI::dbQuoteString(con, path),
        ", header = true", sampling, ")"
      )), vars = "k")
      before <- serialize(source$lazy_query, NULL)
      expect_identical(
        inspect_grouping(source, .grouping = grouping_set(k))$included, "(k)"
      )
      expect_equal(nrow(last_sent_queries(details = TRUE)), 0L)

      for (value in c("101", "source_tail")) {
        writeLines(c("k", value), path)
        # Only the value changes: native zero-row metadata infers different
        # types, so returning zero rows cannot establish absence of source work.
        native <- dplyr::collect(utils::head(source, 0L))
        expect_identical(is.numeric(native$k), identical(value, "101"))
        for (input in list(source, dplyr::filter(source, k == k))) {
          expect_error(
            inspect_grouping(input, .by = where(is.numeric)),
            "dplyr::collect()", fixed = TRUE, class = "marginplyr_error"
          )
          expect_equal(nrow(last_sent_queries(details = TRUE)), 0L)
          expect_error(
            inspect_grouping(input, .grouping = grouping_set(
              where(is.character)
            )),
            "source rows", fixed = TRUE, class = "marginplyr_error"
          )
          expect_equal(nrow(last_sent_queries(details = TRUE)), 0L)
        }
        # Explicit collection remains the caller's choice.
        expect_identical(as.character(dplyr::collect(source)$k), value)
      }
      unlink(path)
      # A file error would prove the binder was reached before the refusal.
      expect_error(
        inspect_grouping(source, .by = where(is.numeric)),
        "source rows", fixed = TRUE, class = "marginplyr_error"
      )
      expect_identical(serialize(source$lazy_query, NULL), before)
    }
  }
})

test_that("CSV refusal precedes even the reader's source-path evaluation", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("k", "101"), path)
  DBI::dbExecute(con, "CREATE SEQUENCE csv_source_work START 1")
  first <- DBI::dbGetQuery(con, "SELECT nextval('csv_source_work')")[[1L]]
  # DuckDB's binder evaluates nextval() while preparing the reader. Give that
  # write an explicit transaction rather than its read-only prepare transaction.
  DBI::dbBegin(con)
  source <- dplyr::tbl(con, dbplyr::sql(paste0(
    "SELECT * FROM read_csv(CASE WHEN nextval('csv_source_work') > 0 THEN ",
    DBI::dbQuoteString(con, path), " END, header = true, sample_size = -1)"
  )), vars = "k")
  for (audit in c(FALSE, TRUE)) {
    old <- options(marginplyr.audit_sql = audit)
    on.exit(options(old), add = TRUE)
    expect_error(
      inspect_grouping(source, .grouping = grouping_set(where(is.numeric))),
      "source rows", fixed = TRUE, class = "marginplyr_error"
    )
    expect_identical(
      DBI::dbGetQuery(con, "SELECT currval('csv_source_work')")[[1L]], first
    )
  }
  # The same source expression runs when its caller explicitly collects it.
  expect_identical(as.character(dplyr::collect(source)$k), "101")
  expect_gt(
    DBI::dbGetQuery(con, "SELECT currval('csv_source_work')")[[1L]], first
  )
  DBI::dbCommit(con)
})

test_that("explicit CSV schemas retain typed inspection without source work", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("k,label", "101,east", "source_tail,west"), path)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  ordinary <- dplyr::copy_to(
    con, tibble::tibble(k = 101L, label = "east"), "csv_control"
  )
  for (reader in c("read_csv", "read_csv_auto", '"read_csv"')) {
    source <- dplyr::tbl(con, dbplyr::sql(paste0(
      "SELECT * FROM ", reader, " /* outer /* nested */ comment */ (",
      DBI::dbQuoteString(con, path), ", header = true, auto_detect = false, ",
      "columns = {'k': 'INTEGER', 'label': 'VARCHAR'})"
    )), vars = c("k", "label"))
    before <- serialize(source$lazy_query, NULL)
    for (kind in c("reader", "table")) {
      input <- switch(kind, reader = source, table = ordinary)
      plan <- inspect_grouping(
        input, .by = where(is.character),
        .grouping = grouping_set(where(is.numeric))
      )
      expect_identical(plan$fixed, "(label)")
      expect_identical(plan$included, "(k)")
      audit <- last_sent_queries(details = TRUE)
      expected <- c("relation_catalog", "selection_proxy")
      expect_identical(audit$purpose, expected)
      expect_true(all(audit$status == "returned"))
      snapshot <- audit[audit$purpose == "selection_proxy", , drop = FALSE]
      expect_match(snapshot$sql, "system.main.query(", fixed = TRUE)
      expect_match(snapshot$sql, "LIMIT 0", fixed = TRUE)
    }
    # A zero-row result would still fail here if metadata converted source rows.
    expect_error(dplyr::collect(source), "Conversion Error", fixed = TRUE)
    expect_identical(serialize(source$lazy_query, NULL), before)
  }
})

test_that("SQL comments and values cannot opt a CSV reader out of the guard", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  missing_path <- DBI::dbQuoteString(con, tempfile(fileext = ".csv"))
  for (arguments in c(
    ", header = true /* auto_detect = false */",
    ", header = true -- auto_detect = false\n",
    ", nullstr = 'auto_detect = false'",
    ", columns = {'auto_detect': 'INTEGER'}, auto_detect = true",
    ", auto_detect = true, nullstr = $$auto_detect = false$$"
  )) {
    source <- dplyr::tbl(con, dbplyr::sql(paste0(
      'SELECT * FROM "read_csv"(', missing_path, arguments, ")"
    )), vars = "k")
    expect_error(
      inspect_grouping(source, .by = where(is.numeric)),
      "source rows", fixed = TRUE, class = "marginplyr_error"
    )
  }
  established <- dplyr::tbl(con, dbplyr::sql("SELECT 1 AS k"), vars = "k")
  unsafe <- dplyr::tbl(con, dbplyr::sql(paste0(
    "SELECT * FROM read_csv(", missing_path, ")"
  )), vars = "k")
  for (input in list(
    dplyr::left_join(established, unsafe, by = "k"),
    dplyr::left_join(unsafe, established, by = "k")
  )) {
    expect_error(
      inspect_grouping(input, .grouping = grouping_set(where(is.numeric))),
      "source rows", fixed = TRUE, class = "marginplyr_error"
    )
  }
  literal <- dplyr::tbl(con, dbplyr::sql(paste(
    "SELECT 'read_csv(' AS k, $$read_csv_auto($$ AS label, 1 AS value",
    "/* read_csv('absent.csv') */"
  )), vars = c("k", "label", "value"))
  expect_identical(
    inspect_grouping(literal, .grouping = grouping_set(
      where(is.character)
    ))$included,
    "(k, label)"
  )
})

test_that("CSV preflight preserves a metadata translation failure's record", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  source <- dplyr::tbl(con, dbplyr::sql("SELECT 1 AS k"), vars = "k")
  source <- dplyr::mutate(source, value = mean(k, trim = 0.1))
  error <- expect_error(
    inspect_grouping(source, .by = where(is.numeric)), "trim"
  )
  expect_false(inherits(error, "marginplyr_error"))
  audit <- last_sent_queries(details = TRUE)
  expect_identical(audit$purpose, "selection_proxy")
  expect_identical(audit$sql, NA_character_)
  expect_identical(audit$status, "errored")
})
