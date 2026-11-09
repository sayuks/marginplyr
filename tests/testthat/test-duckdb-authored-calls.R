test_that("authored table macros cannot hide source-value inference", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("k", "101"), path)
  reader <- paste0("read_csv_auto(", DBI::dbQuoteString(con, path), ")")
  DBI::dbExecute(con, paste(
    "CREATE MACRO hidden_reader() AS TABLE SELECT * FROM", reader
  ))
  inputs <- lapply(c("hidden_reader()", reader), function(call) {
    dplyr::tbl(con, dbplyr::sql(paste("SELECT * FROM", call)), vars = "k")
  })
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  for (value in c("101", "source_tail")) {
    writeLines(c("k", value), path)
    for (input in inputs) {
      before <- serialize(input$lazy_query, NULL)
      expect_identical(is.numeric(dplyr::collect(utils::head(input, 0L))$k),
                       identical(value, "101"))
      for (audit in c(TRUE, FALSE)) {
        options(marginplyr.audit_sql = audit)
        expect_error(inspect_grouping(
          input, .grouping = grouping_set(where(is.numeric))
        ), "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
        expect_error(inspect_grouping(
          input, .by = where(is.numeric)
        ), class = "marginplyr_error")
        expect_error(expand_with_margins(
          input, .grouping = grouping_set(where(is.numeric))
        ), class = "marginplyr_error")
        if (audit) {
          sent <- last_sent_queries(details = TRUE)
          expect_false(any(sent$purpose == "selection_proxy"))
          expect_true(all(sent$status == "returned"))
        } else {
          expect_error(last_sent_queries(), "marginplyr.audit_sql",
                       fixed = TRUE)
        }
      }
      expect_identical(serialize(input$lazy_query, NULL), before)
      expect_identical(as.character(dplyr::collect(input)$k), value)
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

test_that("ordinary functions and established readers retain typed metadata", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("k", "not_an_integer"), path)
  DBI::dbExecute(con, "CREATE MACRO hidden_reader() AS TABLE SELECT 99 AS k")
  DBI::dbExecute(con, 'CREATE MACRO "select"() AS TABLE SELECT 99 AS k')
  DBI::dbWriteTable(con, "src", data.frame(k = 1L))
  DBI::dbExecute(con, "CREATE MACRO exclude(x) AS x")
  DBI::dbExecute(con, "CREATE MACRO materialized(x) AS x")
  reader <- paste0(
    "SELECT * FROM read_csv(", DBI::dbQuoteString(con, path),
    ", columns={'k':'INTEGER'}, auto_detect=false, header=true)"
  )
  statements <- c(
    "SELECT * FROM range(2) AS t(k)",
    "SELECT * FROM generate_series(1, 2) t(k)",
    "SELECT * FROM range(2) query_table(k)",
    "WITH query_table(k) AS (SELECT 1) TABLE query_table",
    "SELECT unnest([1, 2]) AS k",
    "SELECT * FROM range(2) AS hidden_reader(k)",
    "SELECT * FROM range(2) hidden_reader(k)",
    "SELECT * FROM src hidden_reader(k)",
    "WITH t AS (TABLE src) SELECT * FROM t hidden_reader(k)",
    "WITH t AS NOT MATERIALIZED (TABLE src) SELECT * FROM t",
    "SELECT * EXCLUDE (k), 1 AS k FROM src",
    "WITH hidden_reader(k) AS (SELECT 1) TABLE hidden_reader",
    "SELECT (1) AS k",
    "SELECT count(*) FILTER (WHERE TRUE) OVER () AS k FROM range(2)",
    reader
  )
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  for (sql in statements) {
    input <- dplyr::tbl(con, dbplyr::sql(sql), vars = "k")
    before <- serialize(input$lazy_query, NULL)
    expect_identical(inspect_grouping(
      input, .by = where(is.numeric)
    )$fixed, "(k)")
    expect_identical(inspect_grouping(
      input, .grouping = grouping_set(where(is.numeric))
    )$included, "(k)")
    sent <- last_sent_queries(details = TRUE)
    expect_identical(sum(sent$purpose == "selection_proxy"), 1L)
    expect_lte(sum(sent$purpose == "relation_catalog"), 1L)
    expect_true(all(sent$status == "returned"))
    result <- expand_with_margins(
      input, .grouping = grouping_set(where(is.numeric)), .margin_label = NULL
    )
    sent <- last_sent_queries(details = TRUE)
    expect_identical(sum(sent$purpose == "selection_proxy"), 1L)
    expect_identical(utils::tail(sent$purpose, 1L), "result")
    expect_identical(utils::tail(sent$status, 1L), "prepared")
    expect_identical(serialize(input$lazy_query, NULL), before)
    if (identical(sql, reader)) {
      expect_error(dplyr::collect(result), "Conversion Error", fixed = TRUE)
    } else {
      expect_true(is.numeric(dplyr::collect(result)$k))
    }
  }
})

test_that("nested scalar and query calls are checked in authored spellings", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("k", "101"), path)
  reader_sql <- paste0(
    "SELECT * FROM read_csv_auto(", DBI::dbQuoteString(con, path), ")"
  )
  for (name in c("hidden_reader", "over", "filter")) {
    DBI::dbExecute(con, paste(
      "CREATE MACRO", DBI::dbQuoteIdentifier(con, name),
      "() AS TABLE", reader_sql
    ))
  }
  DBI::dbExecute(con, paste(
    "CREATE MACRO nested_reader() AS TABLE SELECT * FROM hidden_reader()"
  ))
  DBI::dbExecute(con, paste(
    "CREATE MACRO hidden_scalar() AS (SELECT k FROM (", reader_sql, "))"
  ))
  DBI::dbExecute(con, paste(
    "CREATE MACRO read_csv(filename, auto_detect := false) AS TABLE",
    reader_sql
  ))
  statements <- c(
    "SELECT * FROM nested_reader()",
    "SELECT * FROM LATERAL hidden_reader()",
    "SELECT * FROM HiDdEn_ReAdEr /* call */ ()",
    'SELECT * FROM main."hidden_reader"()',
    'SELECT * FROM memory.main."hidden_reader"()',
    "FROM hidden_reader() SELECT *",
    "SELECT hidden_scalar() AS k",
    'SELECT main."hidden_scalar"() AS k',
    "SELECT * FROM over()", "SELECT * FROM filter()",
    paste0("SELECT * FROM read_csv(", DBI::dbQuoteString(con, path),
           ", auto_detect=false)"),
    paste0("SELECT * FROM query(", DBI::dbQuoteString(con, reader_sql), ")"),
    paste0('SELECT * FROM system.main."query"(',
           DBI::dbQuoteString(con, reader_sql), ")")
  )
  inputs <- lapply(statements, function(sql) {
    dplyr::tbl(con, dbplyr::sql(sql), vars = "k")
  })
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  for (value in c("101", "source_tail")) {
    writeLines(c("k", value), path)
    for (input in inputs) {
      before <- serialize(input$lazy_query, NULL)
      expect_identical(is.numeric(dplyr::collect(utils::head(input, 0L))$k),
                       identical(value, "101"))
      expect_error(inspect_grouping(
        input, .grouping = grouping_set(where(is.numeric))
      ), "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
      expect_duckdb_catalog_only()
      expect_error(inspect_grouping(input, .by = where(is.numeric)),
                   class = "marginplyr_error")
      expect_duckdb_catalog_only()
      expect_error(expand_with_margins(
        input, .grouping = grouping_set(where(is.numeric))
      ), class = "marginplyr_error")
      expect_duckdb_catalog_only()
      expect_identical(serialize(input$lazy_query, NULL), before)
    }
  }
  unlink(path)
  for (input in inputs) {
    expect_identical(inspect_grouping(
      input, .grouping = grouping_set(k)
    )$included, "(k)")
    expect_equal(nrow(last_sent_queries()), 0L)
  }
})

test_that("relation indirection cannot hide source-value inference", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("k", "101"), path)
  reader <- paste0("read_csv_auto(", DBI::dbQuoteString(con, path), ")")
  DBI::dbExecute(con, paste("CREATE VIEW ordinary AS SELECT * FROM", reader))
  statements <- c(
    "SELECT * FROM query_table('ordinary')",
    "SELECT * FROM QuErY_TaBlE /* call */ ('ordinary')",
    'SELECT * FROM main."query_table"(\'ordinary\')',
    'SELECT * FROM system.main."query_table"(\'ordinary\')',
    "FROM query_table('ordinary') SELECT *",
    "SELECT * FROM LATERAL query_table('ordinary')",
    "SELECT * FROM query_table('memory.main.ordinary')",
    'SELECT * FROM query_table(\'"ordinary"\')',
    "SELECT * FROM query_table(['ordinary'])",
    "SELECT * FROM query_table(['ordinary', 'ordinary'])",
    "SELECT * FROM query_table(['ordinary'], true)",
    paste("SELECT * FROM", reader)
  )
  rows <- c(rep(1L, 9L), 2L, 1L, 1L)
  inputs <- lapply(statements, function(sql) {
    dplyr::tbl(con, dbplyr::sql(sql), vars = "k")
  })
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  for (value in c("101", "source_tail")) {
    writeLines(c("k", value), path)
    for (i in seq_along(inputs)) {
      input <- inputs[[i]]
      before <- serialize(input$lazy_query, NULL)
      expect_identical(is.numeric(dplyr::collect(utils::head(input, 0L))$k),
                       identical(value, "101"))
      for (audit in c(TRUE, FALSE)) {
        options(marginplyr.audit_sql = audit)
        expect_error(inspect_grouping(
          input, .grouping = grouping_set(where(is.numeric))
        ), "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
        if (audit && i < length(inputs)) expect_duckdb_catalog_only()
        expect_error(inspect_grouping(input, .by = where(is.numeric)),
                     class = "marginplyr_error")
        if (audit && i < length(inputs)) expect_duckdb_catalog_only()
        expect_error(expand_with_margins(
          input, .grouping = grouping_set(where(is.numeric))
        ), class = "marginplyr_error")
        if (audit && i < length(inputs)) expect_duckdb_catalog_only()
        if (audit) {
          sent <- last_sent_queries(details = TRUE)
          expect_identical(as.character(dplyr::collect(input)$k),
                           rep(value, rows[[i]]))
          expect_identical(last_sent_queries(details = TRUE), sent)
        }
        if (!audit) {
          expect_error(last_sent_queries(), "marginplyr.audit_sql",
                       fixed = TRUE)
        }
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
    expect_error(inspect_grouping(
      input, .grouping = grouping_set(where(is.numeric))
    ), class = "marginplyr_error")
  }
})
