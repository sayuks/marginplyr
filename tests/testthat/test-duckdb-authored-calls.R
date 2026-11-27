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
  # The literal schema admits the reader spelling, so only the catalog read
  # can show that read_csv is this macro.
  DBI::dbExecute(con, paste(
    "CREATE MACRO read_csv(filename, auto_detect := false, columns := NULL)",
    "AS TABLE", reader_sql
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
           ", auto_detect=false, columns={'k': 'INTEGER'})"),
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

test_that("DuckDB's own scalar macros are admitted without a caller macro", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(
    g = c("a", "b"), v = c(1L, 2L), d = as.Date(c("2026-01-01", "2026-01-02"))
  ))
  calls <- c(
    "nullif(v, 0)", "date_add(d, INTERVAL 1 DAY)", "list_sum([v, v])",
    "array_to_string([g, g], '-')", "current_database()"
  )
  # The typed selection follows each macro's native result type.
  character_columns <- c("(g)", "(g)", "(g)", "(g, y)", "(g, y)")
  t <- dplyr::tbl(con, "t")
  for (i in seq_along(calls)) {
    view <- paste0("v", i)
    sql <- paste0("SELECT g, v, ", calls[[i]], " AS y FROM t")
    DBI::dbExecute(con, paste("CREATE VIEW", view, "AS", sql))
    inputs <- list(
      dplyr::tbl(con, dbplyr::sql(sql)),
      dplyr::mutate(t, y = dbplyr::sql(calls[[i]])),
      dplyr::tbl(con, view)
    )
    for (input in inputs) {
      expect_identical(inspect_grouping(
        input, .grouping = grouping_set(where(is.character))
      )$included, character_columns[[i]])
      summary <- dplyr::collect(summarize_with_margins(
        input, n = dplyr::n(), .grouping = grouping_set(g)
      ))
      expect_identical(as.integer(summary$n), c(1L, 1L))
      expanded <- dplyr::collect(expand_with_margins(
        input, .grouping = grouping_set(g)
      ))
      expect_identical(nrow(expanded), 2L)
    }
  }
})

test_that("DuckDB's own macros stay refused where they can reach source work", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("k", "101"), path)
  quoted <- DBI::dbQuoteString(con, path)
  DBI::dbWriteTable(con, "t", data.frame(v = c(1L, 2L)))
  refused <- function(input) {
    expect_error(inspect_grouping(
      input, .grouping = grouping_set(where(is.numeric))
    ), "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
  }
  authored <- function(sql, vars) {
    dplyr::tbl(con, dbplyr::sql(sql), vars = vars)
  }
  refused(authored(
    paste0("SELECT * FROM histogram_values(", quoted, ", k)"),
    c("bin", "count")
  ))
  refused(authored(
    paste0("SELECT * FROM histogram(", quoted, ", k)"),
    c("bin", "count", "bar")
  ))
  refused(authored("SELECT * FROM query('SELECT * FROM t')", "v"))
  refused(authored(paste0(
    "SELECT nullif((SELECT k FROM read_csv_auto(", quoted, ")), 0) AS k"
  ), "k"))
  # Once a caller macro exists, a system macro's body can resolve to it.
  DBI::dbExecute(con, paste0(
    "CREATE MACRO list_aggr(l, f) AS (SELECT k FROM read_csv_auto(",
    quoted, ") LIMIT 1)"
  ))
  sql <- "SELECT list_sum([v]) AS s FROM t"
  for (value in c("101", "source_tail")) {
    writeLines(c("k", value), path)
    expect_identical(
      is.numeric(DBI::dbGetQuery(con, paste(sql, "LIMIT 0"))$s),
      identical(value, "101")
    )
    refused(authored(sql, "s"))
  }
  refused(authored("SELECT nullif(v, 0) AS v FROM t", "v"))
  DBI::dbExecute(con, "CREATE VIEW vn AS SELECT nullif(v, 0) AS v FROM t")
  expect_error(inspect_grouping(
    dplyr::tbl(con, "vn"), .grouping = grouping_set(where(is.numeric))
  ), "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
})

test_that("a caller macro is refused in every dbplyr spelling", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("k", "101"), path)
  reader <- paste0("read_csv_auto(", DBI::dbQuoteString(con, path), ")")
  DBI::dbWriteTable(con, "t", data.frame(g = c("a", "b"), v = c(1L, 2L)))
  for (name in c("hidden_first", "lower")) {
    DBI::dbExecute(con, paste0(
      "CREATE MACRO ", name, "(x) AS (SELECT first(k) FROM ", reader, ")"
    ))
  }
  t <- dplyr::tbl(con, "t")
  inputs <- list(
    dplyr::mutate(t, y = hidden_first(v)),
    dplyr::summarise(t, y = max(hidden_first(v), na.rm = TRUE), .by = g),
    dplyr::filter(t, hidden_first(v) > 0),
    dplyr::arrange(t, hidden_first(v)),
    dplyr::left_join(t, dplyr::mutate(t, y = hidden_first(v)), by = "g"),
    dplyr::union_all(
      dplyr::mutate(t, y = v), dplyr::mutate(t, y = hidden_first(v))
    ),
    # dbplyr renders tolower() as LOWER(), which resolves to the caller macro.
    dplyr::mutate(t, z = tolower(g))
  )
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  for (value in c("101", "source_tail")) {
    writeLines(c("k", value), path)
    for (input in inputs) {
      before <- serialize(input$lazy_query, NULL)
      expect_error(inspect_grouping(
        input, .grouping = grouping_set(where(is.numeric))
      ), "DuckDB call", fixed = TRUE, class = "marginplyr_error")
      expect_duckdb_catalog_only()
      expect_error(inspect_grouping(input, .by = where(is.numeric)),
                   "DuckDB call", fixed = TRUE, class = "marginplyr_error")
      expect_duckdb_catalog_only()
      expect_error(
        summarize_with_margins(input, n = dplyr::n(), .grouping = rollup(g)),
        "DuckDB call", fixed = TRUE, class = "marginplyr_error"
      )
      expect_duckdb_catalog_only()
      expect_error(expand_with_margins(input, .grouping = rollup(g)),
                   "DuckDB call", fixed = TRUE, class = "marginplyr_error")
      expect_duckdb_catalog_only()
      expect_identical(serialize(input$lazy_query, NULL), before)
    }
  }
  options(marginplyr.audit_sql = FALSE)
  expect_error(inspect_grouping(
    inputs[[1L]], .grouping = grouping_set(where(is.numeric))
  ), "DuckDB call", fixed = TRUE, class = "marginplyr_error")
  # Name-only inspection opens nothing.
  options(marginplyr.audit_sql = TRUE)
  expect_identical(inspect_grouping(
    inputs[[1L]], .grouping = grouping_set(v)
  )$included, "(v)")
  expect_equal(nrow(last_sent_queries()), 0L)
})

test_that("unquoted non-ASCII macro heads are read as DuckDB does (#1834)", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("k", "101"), path)
  reader <- paste0("read_csv_auto(", DBI::dbQuoteString(con, path), ")")
  DBI::dbWriteTable(con, "t", data.frame(g = c("a", "b"), v = 1:2))
  macro <- enc2utf8("mé")
  DBI::dbExecute(con, enc2utf8(paste0(
    'CREATE MACRO "mé"(a) AS (SELECT first(k) FROM ', reader, ")"
  )))
  DBI::dbExecute(con, paste0(
    "CREATE MACRO hidden_first(a) AS (SELECT first(k) FROM ", reader, ")"
  ))
  DBI::dbExecute(con, enc2utf8(paste0(
    'CREATE MACRO "tmé"() AS TABLE SELECT \'a\' AS g, k AS y FROM ',
    reader
  )))
  authored <- function(sql) {
    dplyr::tbl(con, dbplyr::sql(enc2utf8(sql)), vars = c("g", "y"))
  }
  t <- dplyr::tbl(con, "t")
  inputs <- list(
    authored("SELECT g, mé(v) AS y FROM t"),
    authored("SELECT g, main.mé(v) AS y FROM t"),
    authored("SELECT g, y FROM tmé()"),
    # A code point DuckDB strips as whitespace still leaves a call head.
    authored("SELECT g, hidden_first\u3000(v) AS y FROM t"),
    dplyr::mutate(t, y = !!rlang::call2(macro, quote(v))),
    dplyr::mutate(t, y = dplyr::sql(enc2utf8("mé(v)")))
  )
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  refused <- function(input) {
    expect_error(inspect_grouping(
      input, .grouping = grouping_set(where(is.numeric))
    ), "DuckDB call", fixed = TRUE, class = "marginplyr_error")
    expect_duckdb_catalog_only()
  }
  for (value in c("101", "source_tail")) {
    writeLines(c("k", value), path)
    for (input in inputs) {
      expect_identical(is.numeric(dplyr::collect(utils::head(input, 0L))$y),
                       identical(value, "101"))
      refused(input)
    }
  }
  unlink(path)
  for (input in inputs) refused(input)
})


test_that("a by() call head reaches a caller macro named by (#1835)", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("k", "101"), path)
  DBI::dbWriteTable(con, "t", data.frame(g = c("a", "b"), v = 1:2))
  authored <- function(sql) {
    dplyr::tbl(con, dbplyr::sql(sql), vars = c("g", "v", "y"))
  }
  ordered <- list(
    authored("SELECT g, v, 1 AS y FROM t GROUP BY (g), (v)"),
    authored("SELECT g, v, 1 AS y FROM t ORDER BY (g)"),
    authored(paste("SELECT g, v, row_number() OVER",
                   "(PARTITION BY (g) ORDER BY (v)) AS y FROM t"))
  )
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  # With no macro named `by`, BY before a parenthesis stays syntax.
  for (input in ordered) {
    expect_identical(inspect_grouping(
      input, .grouping = grouping_set(where(is.numeric))
    )$included, "(v, y)")
  }
  DBI::dbExecute(con, paste0(
    'CREATE MACRO "by"(a) AS (SELECT first(k) FROM read_csv_auto(',
    DBI::dbQuoteString(con, path), "))"
  ))
  inputs <- list(
    authored("SELECT g, v, by(v) AS y FROM t"),
    authored("SELECT g, v, by (v) AS y FROM t"),
    dplyr::mutate(dplyr::tbl(con, "t"), y = by(v)),
    dplyr::mutate(dplyr::tbl(con, "t"), y = dplyr::sql("by(v)"))
  )
  for (value in c("101", "source_tail", "absent")) {
    if (value == "absent") unlink(path) else writeLines(c("k", value), path)
    for (input in inputs) {
      expect_error(inspect_grouping(
        input, .grouping = grouping_set(where(is.numeric))
      ), "DuckDB call", fixed = TRUE, class = "marginplyr_error")
      expect_duckdb_catalog_only()
    }
  }
})

test_that("no word the call scan reads as syntax reaches a caller macro", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  reached <- function(sql) {
    tryCatch(identical(DBI::dbGetQuery(con, sql)[[1L]], "reached"),
             error = function(cnd) FALSE)
  }
  for (word in duckdb_call_syntax) {
    name <- DBI::dbQuoteIdentifier(con, word)
    DBI::dbExecute(con, paste0("CREATE MACRO ", name, "(a) AS 'reached'"))
    expect_false(reached(paste0("SELECT ", word, "(1)")), label = word)
    DBI::dbExecute(con, paste("DROP MACRO", name))
    DBI::dbExecute(con, paste0(
      "CREATE MACRO ", name, "(a) AS TABLE SELECT 'reached' AS r"
    ))
    expect_false(reached(paste0("SELECT * FROM ", word, "(1)")), label = word)
    DBI::dbExecute(con, paste("DROP MACRO TABLE", name))
  }
})


test_that("a star modifier is told from a multiplication (#1836)", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("k", "101"), path)
  reader <- paste0("read_csv_auto(", DBI::dbQuoteString(con, path), ")")
  DBI::dbWriteTable(con, "t", data.frame(g = c("a", "b"), v = 1:2))
  modifiers <- c("exclude", "replace", "rename")
  for (name in modifiers) {
    DBI::dbExecute(con, paste0(
      "CREATE MACRO ", name, "(a) AS (SELECT first(k) FROM ", reader, ")"
    ))
  }
  authored <- function(sql) {
    dplyr::tbl(con, dbplyr::sql(sql), vars = c("g", "v", "y"))
  }
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  for (sql in c(
    "SELECT * EXCLUDE (v), 1 AS v, 2 AS y FROM t",
    "SELECT DISTINCT * EXCLUDE (v), 1 AS v, 2 AS y FROM t",
    "SELECT ALL * EXCLUDE (v), 1 AS v, 2 AS y FROM t",
    "SELECT g, * EXCLUDE (g), 2 AS y FROM t",
    "SELECT t.* REPLACE (v + 1 AS v), 2 AS y FROM t",
    "SELECT \"t\".* REPLACE (v + 1 AS v), 2 AS y FROM t",
    "SELECT g, COLUMNS(t.* EXCLUDE (g)), 2 AS y FROM t",
    "SELECT g, v, y FROM (SELECT * RENAME (v AS y), v FROM t)",
    "SELECT g, COLUMNS(* EXCLUDE (g)), 2 AS y FROM t"
  )) {
    expect_identical(inspect_grouping(
      authored(sql), .grouping = grouping_set(where(is.numeric))
    )$included, "(v, y)", label = sql)
  }
  t <- dplyr::tbl(con, "t")
  inputs <- c(
    lapply(modifiers, function(name) {
      authored(paste0("SELECT g, v, v * ", name, "(v) AS y FROM t"))
    }),
    list(
      authored("SELECT g, v, (v) * exclude(v) AS y FROM t"),
      # A numeric literal's trailing dot qualifies no star.
      authored("SELECT g, v, 2. * exclude(v) AS y FROM t"),
      # Nor does a piece of one: these lex as `1`, `_0`, `.` and `1`, `e3`,
      # `.`, and a qualified star starts a select item.
      authored("SELECT g, v, 1_0. * exclude(v) AS y FROM t"),
      authored("SELECT g, v, 1e3. * replace(v) AS y FROM t"),
      dplyr::mutate(t, y = dplyr::sql("1_0. * exclude(v)")),
      authored("SELECT g, v, v ** exclude(v) AS y FROM t"),
      authored("SELECT g, v, v AS y FROM t WHERE v * exclude(v) > 0"),
      dplyr::mutate(t, y = v * exclude(v)),
      dplyr::mutate(t, y = dplyr::sql("v * rename(v)"))
    )
  )
  for (value in c("101", "source_tail", "absent")) {
    if (value == "absent") unlink(path) else writeLines(c("k", value), path)
    for (input in inputs) {
      expect_error(inspect_grouping(
        input, .grouping = grouping_set(where(is.numeric))
      ), "DuckDB call", fixed = TRUE, class = "marginplyr_error")
      expect_duckdb_catalog_only()
    }
  }
})


test_that("chained and parenthesis-less macro calls are checked (#1647)", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("k", "101"), path)
  reader <- paste0("read_csv_auto(", DBI::dbQuoteString(con, path), ")")
  DBI::dbWriteTable(con, "t", data.frame(g = c("a", "b"), v = c(1L, 2L)))
  # DuckDB binds a bare current_timestamp as get_current_timestamp(). Its own
  # `user` and `current_catalog` macros call current_user and
  # current_database() by unqualified or schema-qualified name.
  for (name in c("hidden_first", "get_current_timestamp", "current_user",
                 "current_database")) {
    DBI::dbExecute(con, paste0(
      "CREATE MACRO ", name, "(x := 0) AS (SELECT first(k) FROM ", reader, ")"
    ))
  }
  calls <- c("v.hidden_first()", "t.v.hidden_first()", "(v).hidden_first()",
             'v."hidden_first"()', "current_user", "CURRENT_USER",
             '"current_user"', "current_timestamp", "user",
             "current_catalog")
  t <- dplyr::tbl(con, "t")
  inputs <- list()
  for (i in seq_along(calls)) {
    sql <- paste0("SELECT g, v, ", calls[[i]], " AS y FROM t")
    view <- paste0("chained_view", i)
    DBI::dbExecute(con, paste("CREATE VIEW", view, "AS", sql))
    inputs <- c(inputs, list(
      list(dplyr::tbl(con, dbplyr::sql(sql), vars = c("g", "v", "y")),
           "DuckDB call"),
      list(dplyr::mutate(t, y = dbplyr::sql(calls[[i]])), "DuckDB call"),
      list(dplyr::tbl(con, view), "DuckDB view")
    ))
  }
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  for (value in c("101", "source_tail")) {
    writeLines(c("k", value), path)
    for (case in inputs) {
      expect_error(inspect_grouping(
        case[[1L]], .grouping = grouping_set(where(is.numeric))
      ), case[[2L]], fixed = TRUE, class = "marginplyr_error")
      expect_duckdb_catalog_only()
      expect_error(
        summarize_with_margins(
          case[[1L]], n = dplyr::n(), .grouping = rollup(g)
        ),
        case[[2L]], fixed = TRUE, class = "marginplyr_error"
      )
      expect_duckdb_catalog_only()
    }
  }
  # Qualified and chained calls to ordinary functions stay admitted.
  admitted <- c("main.lower(g)", "system.main.lower(g)", "g.lower()",
                "(g).upper()", "t.g.lower()")
  for (call in admitted) {
    sql <- paste0("SELECT g, v, ", call, " AS y FROM t")
    for (input in list(dplyr::tbl(con, dbplyr::sql(sql)),
                       dplyr::mutate(t, y = dbplyr::sql(call)))) {
      expect_identical(inspect_grouping(
        input, .grouping = grouping_set(where(is.numeric))
      )$included, "(v)")
      sent <- last_sent_queries(details = TRUE)
      expect_identical(sum(sent$purpose == "selection_proxy"), 1L)
    }
  }
})

test_that("a system macro's body resolves in a stored view's schema", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("k", "101"), path)
  DBI::dbWriteTable(con, "t", data.frame(g = c("a", "b"), v = c(1L, 2L)))
  DBI::dbExecute(con, "CREATE SCHEMA s")
  # DuckDB's own `user` macro calls current_user, which the view's schema
  # supplies when the view binds.
  DBI::dbExecute(con, paste0(
    "CREATE MACRO s.current_user() AS (SELECT first(k) FROM read_csv_auto(",
    DBI::dbQuoteString(con, path), "))"
  ))
  DBI::dbExecute(con, "CREATE VIEW s.vw2 AS SELECT user AS u, v FROM main.t")
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  expect_error(inspect_grouping(
    dplyr::tbl(con, DBI::Id(schema = "s", table = "vw2")),
    .grouping = grouping_set(where(is.numeric))
  ), "DuckDB view", fixed = TRUE, class = "marginplyr_error")
  expect_duckdb_catalog_only()
})

test_that("a view's functions also resolve in its catalog's main schema", {
  skip_if_suggest_absent("duckdb", "DBI")
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("k", "101"), path)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  # Each case names where its caller macro lives and the view body reaching
  # it; `NA` defines none, and `db3.main` is outside the view's search path.
  cases <- list(
    list(macro = "db2.main.current_user()", body = "user AS u, v",
         refused = TRUE),
    list(macro = "db2.main.hf(x)", body = "hf(v) AS u, v", refused = TRUE),
    list(macro = "db3.main.current_user()", body = "user AS u, v",
         refused = FALSE),
    list(macro = NA, body = "user AS u, v", refused = FALSE)
  )
  for (case in cases) {
    con <- duckdb_test_connection()
    DBI::dbExecute(con, "ATTACH ':memory:' AS db2")
    DBI::dbExecute(con, "ATTACH ':memory:' AS db3")
    DBI::dbExecute(con, "CREATE SCHEMA db2.s2")
    DBI::dbExecute(con, "CREATE TABLE db2.main.t AS SELECT 'a' AS g, 1 AS v")
    if (!is.na(case$macro)) {
      DBI::dbExecute(con, paste0(
        "CREATE MACRO ", case$macro, " AS ",
        "(SELECT first(k) FROM read_csv_auto(",
        DBI::dbQuoteString(con, path), "))"
      ))
    }
    DBI::dbExecute(con, paste(
      "CREATE VIEW db2.s2.vw AS SELECT", case$body, "FROM db2.main.t"
    ))
    input <- dplyr::tbl(con, DBI::Id(catalog = "db2", schema = "s2",
                                     table = "vw"))
    if (case$refused) {
      expect_error(inspect_grouping(
        input, .grouping = grouping_set(where(is.numeric))
      ), "DuckDB view", fixed = TRUE, class = "marginplyr_error")
      expect_duckdb_catalog_only()
    } else {
      expect_identical(inspect_grouping(
        input, .grouping = grouping_set(where(is.numeric))
      )$included, "(v)")
      expect_identical(last_sent_queries(details = TRUE)$purpose,
                       c("relation_catalog", "selection_proxy"))
    }
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
})

test_that("DuckDB's own parenthesis-less functions keep typed metadata", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(g = c("a", "b"), v = c(1L, 2L)))
  for (call in c("current_user", "user", "current_timestamp")) {
    input <- dplyr::tbl(con, dbplyr::sql(
      paste0("SELECT g, v, ", call, " AS y FROM t")
    ))
    expect_identical(inspect_grouping(
      input, .grouping = grouping_set(where(is.numeric))
    )$included, "(v)")
  }
})

test_that("value-function names reaching no caller macro stay admitted", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  columns <- c("user", "current_user", "session_user", "current_role",
               "current_catalog", "current_schema")
  frame <- data.frame(v = c(1L, 2L))
  frame[columns] <- "a"
  DBI::dbWriteTable(con, "t", frame)
  DBI::dbExecute(con, "CREATE MACRO add_one(x) AS x + 1")
  t <- dplyr::tbl(con, "t")
  for (name in columns) {
    quoted <- DBI::dbQuoteIdentifier(con, name)
    sql <- paste0("SELECT ", quoted, ", v FROM t")
    view <- paste0("view_", name)
    DBI::dbExecute(con, paste("CREATE VIEW", view, "AS", sql))
    inputs <- list(
      dplyr::select(t, dplyr::all_of(c(name, "v"))),
      dplyr::tbl(con, dbplyr::sql(sql)),
      dplyr::tbl(con, view),
      dplyr::tbl(con, dbplyr::sql(paste0("SELECT v, ", name, " AS y FROM t")))
    )
    for (input in inputs) {
      expect_identical(inspect_grouping(
        input, .grouping = grouping_set(where(is.numeric))
      )$included, "(v)")
    }
  }
})

test_that("a system macro reaches a caller macro through any overload", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE MACRO hidden_first() AS 1")
  catalog <- duckdb_metadata_catalog(con)
  # No overloaded system macro is reachable from a value-function name, so a
  # second overload of current_user's macro is appended after the real one,
  # whose stored definition calls nothing.
  first <- duckdb_metadata_catalog_match(
    "current_user", catalog, kinds = c("macro", "table_macro")
  )
  expect_false(duckdb_macro_reaches_caller(first, catalog))
  overload <- function(definition) {
    row <- catalog[first, ]
    row$definition <- definition
    rbind(catalog, row)
  }
  expect_true(duckdb_macro_reaches_caller(first, overload("hidden_first()")))
  expect_false(duckdb_macro_reaches_caller(first, overload("lower('a')")))
})

test_that("verbatim SQL carriers cannot hide a CSV source", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(g = c("a", "b"), v = c(1L, 2L)))
  fragment <- paste0("(SELECT max(k) FROM ", DBI::dbQuoteString(con, path), ")")
  t <- dplyr::tbl(con, "t")
  inputs <- list(
    dplyr::mutate(t, y = !!DBI::SQL(fragment)),
    dplyr::mutate(t, y = !!dbplyr::ident_q(fragment))
  )
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  for (value in c("101", "source_tail")) {
    writeLines(c("k", value), path)
    for (input in inputs) {
      expect_error(inspect_grouping(
        input, .grouping = grouping_set(where(is.numeric))
      ), "DuckDB CSV input", fixed = TRUE, class = "marginplyr_error")
      expect_duckdb_catalog_only()
    }
  }
})

test_that("translated pipelines without a caller macro keep typed metadata", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(g = c("a", "b"), v = c(1L, 2L)))
  t <- dplyr::tbl(con, "t")
  joined <- dplyr::left_join(
    t, dplyr::mutate(t, w = v * 2L), by = c("g", "v")
  )
  cases <- list(
    list(dplyr::mutate(t, y = v + 1L, z = tolower(g)), "(v, y)"),
    list(dplyr::summarise(t, y = mean(v, na.rm = TRUE), .by = g), "(y)"),
    list(dplyr::mutate(t, y = dplyr::na_if(v, 0L)), "(v, y)"),
    list(dplyr::filter(
      dplyr::mutate(joined, r = dplyr::row_number(), .by = g), v > 0L
    ), "(v, w, r)")
  )
  for (case in cases) {
    expect_identical(inspect_grouping(
      case[[1L]], .grouping = grouping_set(where(is.numeric))
    )$included, case[[2L]])
  }
})

# The remedy a caller can run comes first: `compute()` stores a table whose
# columns are read from the catalog, which the refusal could not do (#1703).
test_that("a refused DuckDB call or view names compute() as its remedy", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(g = c("a", "b"), v = c(1L, 2L)))
  DBI::dbExecute(con, "CREATE MACRO doubled(x) AS (SELECT 2 * x)")
  DBI::dbExecute(
    con, "CREATE VIEW macro_view AS SELECT g, doubled(v) AS y FROM t"
  )
  t <- dplyr::tbl(con, "t")
  inputs <- list(
    call = dplyr::mutate(t, y = doubled(v)),
    view = dplyr::tbl(con, "macro_view")
  )
  remedy <- paste0(
    "Call `dplyr::compute()` on the input first, so the Margin verb reads ",
    "the stored table, or call `dplyr::collect()` and pass the resulting ",
    "data frame."
  )
  for (kind in names(inputs)) {
    error <- expect_error(
      inspect_grouping(
        inputs[[kind]], .grouping = grouping_set(where(is.numeric))
      ),
      paste0("this DuckDB ", kind),
      class = "marginplyr_error",
      info = kind
    )
    expect_match(conditionMessage(error), remedy, fixed = TRUE, info = kind)
    expect_no_match(conditionMessage(error), "reader metadata", info = kind)

    stored <- dplyr::compute(inputs[[kind]])
    included <- inspect_grouping(
      stored, .grouping = grouping_set(where(is.numeric)), .format = "list"
    )$included
    expect_true("y" %in% included[[1L]], info = kind)
  }
})

test_that("backtick-quoted macro and query heads are refused (#1779)", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  if (utils::packageVersion("duckdb") < "2.0.0") {
    # DuckDB 1.5 has no backtick identifier; the operator reading below is
    # what its preflight follows.
    expect_error(DBI::dbGetQuery(con, "SELECT 1 AS `k`"), "syntax error",
                 fixed = TRUE)
  } else {
    path <- tempfile(fileext = ".csv")
    on.exit(unlink(path), add = TRUE)
    writeLines(c("k", "101"), path)
    reader <- paste0("read_csv_auto(", DBI::dbQuoteString(con, path), ")")
    DBI::dbWriteTable(con, "t", data.frame(g = c("a", "b"), v = c(1L, 2L)))
    DBI::dbExecute(con, paste0(
      "CREATE MACRO hidden_first(x) AS (SELECT first(k) FROM ", reader, ")"
    ))
    DBI::dbExecute(con, paste(
      "CREATE MACRO hidden_reader() AS TABLE SELECT * FROM", reader
    ))
    authored <- function(sql, vars) {
      dplyr::tbl(con, dbplyr::sql(sql), vars = vars)
    }
    inputs <- list(
      authored("SELECT g, v, `hidden_first`(v) AS y FROM t", c("g", "v", "y")),
      authored("SELECT g, v, main.`hidden_first`(v) AS y FROM t",
               c("g", "v", "y")),
      authored("SELECT 'a' AS g, * FROM `hidden_reader`()", c("g", "k")),
      authored(paste0(
        "SELECT 'a' AS g, * FROM `query`(",
        DBI::dbQuoteString(con, paste("SELECT * FROM", reader)), ")"
      ), c("g", "k")),
      dplyr::mutate(dplyr::tbl(con, "t"), y = dplyr::sql("`hidden_first`(v)"))
    )
    old <- options(marginplyr.audit_sql = TRUE)
    on.exit(options(old), add = TRUE)
    for (value in c("101", "source_tail")) {
      writeLines(c("k", value), path)
      for (input in inputs) {
        expect_error(inspect_grouping(
          input, .grouping = grouping_set(where(is.numeric))
        ), "DuckDB call", fixed = TRUE, class = "marginplyr_error")
        expect_duckdb_catalog_only()
        expect_error(
          summarize_with_margins(input, n = dplyr::n(), .grouping = rollup(g)),
          "DuckDB call", fixed = TRUE, class = "marginplyr_error"
        )
        expect_duckdb_catalog_only()
      }
    }
  }
})

test_that("a caller macro DuckDB binds without a call head refuses (#1780)", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("k", "101"), path)
  reader <- paste0("read_csv_auto(", DBI::dbQuoteString(con, path), ")")
  DBI::dbWriteTable(con, "t", data.frame(g = c("a", "b"), v = c(1L, 2L)))
  operator_sql <- "SELECT g, v, v + 1 AS y FROM t"
  like_sql <- "SELECT g, v, g LIKE 'a' AS y FROM t"
  # DuckDB stores both forms as operators, not as named calls.
  DBI::dbExecute(con, paste("CREATE VIEW operator_view AS", operator_sql))
  DBI::dbExecute(con, paste("CREATE VIEW like_view AS", like_sql))
  t <- dplyr::tbl(con, "t")
  authored <- function(sql, vars = c("g", "v", "y")) {
    dplyr::tbl(con, dbplyr::sql(sql), vars = vars)
  }
  inputs <- list(
    dplyr::mutate(t, y = v + 1L),
    dplyr::count(t, g),
    authored(operator_sql),
    authored("SELECT g, COUNT(*) AS n FROM t GROUP BY g", c("g", "n")),
    authored("SELECT g, v, {'a': v} AS y FROM t"),
    authored("SELECT g, v, [v] AS y FROM t"),
    authored("SELECT g, v, EXTRACT(year FROM DATE '2020-01-01') AS y FROM t"),
    authored(like_sql),
    dplyr::tbl(con, "operator_view"),
    dplyr::tbl(con, "like_view")
  )
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  typed_plans <- function() {
    lapply(inputs, function(input) {
      plan <- inspect_grouping(input, .grouping = grouping_set(
        where(is.numeric)
      ))$included
      sent <- last_sent_queries(details = TRUE)
      expect_identical(sum(sent$purpose == "selection_proxy"), 1L)
      plan
    })
  }
  # Without such a macro, or with one the statement does not reach, the
  # plans do not follow the file.
  plans <- typed_plans()
  DBI::dbExecute(con, paste0(
    "CREATE MACRO hidden_first(x) AS (SELECT first(k) FROM ", reader, ")"
  ))
  writeLines(c("k", "source_tail"), path)
  expect_identical(typed_plans(), plans)
  for (name in c("+", "count_star", "struct_pack", "list_value", "date_part",
                 "~~", "factorial", "regexp_matches", "trunc")) {
    macro <- DBI::dbQuoteIdentifier(con, name)
    DBI::dbExecute(con, paste0(
      "CREATE MACRO ", macro, "(a := NULL, b := NULL) AS ",
      "(SELECT first(k) FROM ", reader, ")"
    ))
    for (value in c("101", "source_tail")) {
      writeLines(c("k", value), path)
      for (input in inputs) {
        expect_error(inspect_grouping(
          input, .grouping = grouping_set(where(is.numeric))
        ), "DuckDB call", fixed = TRUE, class = "marginplyr_error")
        expect_duckdb_catalog_only()
        expect_error(
          summarize_with_margins(input, n = dplyr::n(), .grouping = rollup(g)),
          "DuckDB call", fixed = TRUE, class = "marginplyr_error"
        )
        expect_duckdb_catalog_only()
        expect_error(expand_with_margins(input, .grouping = rollup(g)),
                     "DuckDB call", fixed = TRUE, class = "marginplyr_error")
        expect_duckdb_catalog_only()
      }
    }
    DBI::dbExecute(con, paste("DROP MACRO", macro))
  }
})

test_that("a caller macro named with operator characters refuses (#1813)", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("k", "101"), path)
  reader <- paste0("read_csv_auto(", DBI::dbQuoteString(con, path), ")")
  DBI::dbWriteTable(con, "t", data.frame(g = "a", v = 1L, j = "{\"a\":1}"))
  t <- dplyr::tbl(con, "t")
  unrelated <- dplyr::select(t, g, v)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  plan <- inspect_grouping(
    unrelated, .grouping = grouping_set(where(is.numeric))
  )$included
  # With no operator macro, each spelling of an operator DuckDB binds keeps
  # its plan.
  shift <- "SELECT g, v, v << 1 AS y FROM t"
  DBI::dbExecute(con, paste("CREATE VIEW shift_view AS", shift))
  for (input in list(
    dplyr::tbl(con, dbplyr::sql(shift), vars = c("g", "v", "y")),
    dplyr::mutate(t, y = dplyr::sql("v << 1")),
    dplyr::mutate(t, y = !!rlang::call2("%<<%", quote(v), 1L)),
    dplyr::tbl(con, "shift_view")
  )) {
    expect_identical(inspect_grouping(
      input, .grouping = grouping_set(where(is.numeric))
    )$included, "(v, y)")
  }
  DBI::dbExecute(con, "DROP VIEW shift_view")
  # DuckDB 2.0 lexes a backtick as a quoted identifier, 1.5 as an operator.
  operators <- c("@@", "->>", "<<=", "!!", "|/",
                 if (utils::packageVersion("duckdb") < "2.0.0") "`")
  for (operator in operators) {
    macro <- DBI::dbQuoteIdentifier(con, operator)
    DBI::dbExecute(con, paste0(
      "CREATE MACRO ", macro, "(a := NULL, b := NULL) AS ",
      "(SELECT first(k) FROM ", reader, ")"
    ))
    form <- paste0("j ", operator, " 'a'")
    sql <- paste0("SELECT g, v, ", form, " AS y FROM t")
    DBI::dbExecute(con, paste("CREATE VIEW operator_view AS", sql))
    infix <- rlang::call2(paste0("%", operator, "%"), quote(j), "a")
    inputs <- list(
      dplyr::tbl(con, dbplyr::sql(sql), vars = c("g", "v", "y")),
      dplyr::mutate(t, y = dplyr::sql(form)),
      dplyr::mutate(t, y = !!infix),
      dplyr::tbl(con, "operator_view"),
      unrelated
    )
    for (value in c("101", "source_tail")) {
      writeLines(c("k", value), path)
      for (input in inputs) {
        expect_error(inspect_grouping(
          input, .grouping = grouping_set(where(is.numeric))
        ), "DuckDB call", fixed = TRUE, class = "marginplyr_error",
        info = operator)
        expect_duckdb_catalog_only()
        expect_error(
          summarize_with_margins(input, n = dplyr::n(), .grouping = rollup(g)),
          "DuckDB call", fixed = TRUE, class = "marginplyr_error",
          info = operator
        )
        expect_duckdb_catalog_only()
      }
    }
    # Name-only inspection opens nothing.
    expect_identical(inspect_grouping(
      unrelated, .grouping = grouping_set(v)
    )$included, "(v)")
    expect_equal(nrow(last_sent_queries()), 0L)
    DBI::dbExecute(con, "DROP VIEW operator_view")
    DBI::dbExecute(con, paste("DROP MACRO", macro))
  }
  expect_identical(inspect_grouping(
    unrelated, .grouping = grouping_set(where(is.numeric))
  )$included, plan)
})

test_that("each implicitly bound function name is reached without a head", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  # Operators DuckDB binds as functions, the class duckdb_operator_name()
  # decides by its characters.
  operators <- c(
    "+" = "2 + 1", "-" = "2 - 1", "*" = "2 * 1", "/" = "2 / 1", "%" = "2 % 1",
    "//" = "2 // 1", "^" = "2 ^ 1", "**" = "2 ** 1", "||" = "'a' || 'b'",
    "<<" = "2 << 1", ">>" = "2 >> 1", "&" = "2 & 1", "|" = "2 | 1",
    "~" = "~ 2", "@" = "@ 2", "^@" = "'ab' ^@ 'a'", "&&" = "[1] && [1]",
    "@>" = "[1] @> [1]", "<@" = "[1] <@ [1]", "<->" = "[1.0] <-> [1.0]",
    "<=>" = "[1.0] <=> [1.0]", "~~" = "'a' LIKE 'a'",
    "!~~" = "'a' NOT LIKE 'a'", "~~*" = "'a' ILIKE 'a'",
    "!~~*" = "'a' NOT ILIKE 'a'", "~~~" = "'a' GLOB 'a'",
    "@@" = "'a' @@ 'a'", "->>" = "'a' ->> 'a'", "<<=" = "2 <<= 1",
    "!!" = "!! 2", "|/" = "|/ 4"
  )
  expect_true(all(duckdb_operator_name(names(operators))))
  # The syntax that binds each name in duckdb_implicit_functions.
  forms <- c(
    count_star = "COUNT(*)", list_value = "[1]", struct_pack = "{'a': 1}",
    row = "(1, 2)", map = "MAP {'a': 1}",
    date_part = "EXTRACT(year FROM DATE '2020-01-01')",
    timezone = "TIMESTAMPTZ '2020-01-01' AT TIME ZONE 'UTC'",
    ltrim = "TRIM(LEADING 'x' FROM 'xa')",
    rtrim = "TRIM(TRAILING 'x' FROM 'ax')",
    regexp_full_match = "'a' SIMILAR TO 'a'",
    like_escape = "'a' LIKE 'a' ESCAPE '$'",
    ilike_escape = "'a' ILIKE 'a' ESCAPE '$'",
    list_apply = "[x FOR x IN [1]]", contains = "1 IN [1]",
    to_millennia = "INTERVAL (1) MILLENNIUM",
    to_centuries = "INTERVAL (1) CENTURY", to_decades = "INTERVAL (1) DECADE",
    to_years = "INTERVAL (1) YEAR", to_quarters = "INTERVAL (1) QUARTER",
    to_months = "INTERVAL (1) MONTH", to_weeks = "INTERVAL (1) WEEK",
    to_days = "INTERVAL (1) DAY", to_hours = "INTERVAL (1) HOUR",
    to_minutes = "INTERVAL (1) MINUTE", to_seconds = "INTERVAL (1) SECOND",
    to_milliseconds = "INTERVAL (1) MILLISECOND",
    to_microseconds = "INTERVAL (1) MICROSECOND",
    # The INTERVAL constructor truncates a fractional count to an integer,
    # so this macro returns one in place of the text the others return.
    trunc = "CAST(INTERVAL (1.5) DAY AS VARCHAR)"
  )
  # DuckDB 1.5 binds postfix `!` and `~` to its own functions however the
  # catalog names a macro, so those names have no form to assert there.
  if (utils::packageVersion("duckdb") >= "2.0.0") {
    forms <- c(forms, factorial = "5!", regexp_matches = "'a' ~ 'a'")
  }
  expect_setequal(names(forms), setdiff(
    duckdb_implicit_functions,
    if (utils::packageVersion("duckdb") < "2.0.0") {
      c("factorial", "regexp_matches")
    }
  ))
  forms <- c(operators, forms)
  for (name in names(forms)) {
    macro <- DBI::dbQuoteIdentifier(con, name)
    body <- if (name == "trunc") "7" else "'reached'"
    DBI::dbExecute(con, paste0(
      "CREATE MACRO ", macro, "(a := NULL, b := NULL, c := NULL) AS ", body
    ))
    expect_identical(
      DBI::dbGetQuery(con, paste("SELECT", forms[[name]], "AS r"))$r,
      if (name == "trunc") "7 days" else "reached", label = forms[[name]]
    )
    DBI::dbExecute(con, paste("DROP MACRO", macro))
  }
})

test_that("the histogram() aggregate is refused like its table macro (#1781)", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(g = c("a", "a", "b"),
                                         v = c(1L, 1L, 2L)))
  # The aggregate shares its name with a system table macro, and the call
  # head is matched against macros wherever it stands.
  x <- dplyr::summarise(dplyr::group_by(dplyr::tbl(con, "t"), g),
                        h = histogram(v), v = sum(v, na.rm = TRUE))
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  expect_error(
    inspect_grouping(x, .grouping = grouping_set(where(is.numeric))),
    "DuckDB call", fixed = TRUE, class = "marginplyr_error"
  )
  expect_duckdb_catalog_only()
  expect_error(
    summarize_with_margins(x, v = sum(v), .grouping = rollup(g)),
    "DuckDB call", fixed = TRUE, class = "marginplyr_error"
  )
  # compute() first is the documented remedy.
  totals <- dplyr::collect(summarize_with_margins(
    dplyr::compute(x), v = sum(v, na.rm = TRUE), .grouping = rollup(g)
  ))
  expect_identical(as.numeric(totals$v[match(c("a", "b", "Total"), totals$g)]),
                   c(2, 2, 4))
})
