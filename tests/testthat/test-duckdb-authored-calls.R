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

test_that("chained and parenthesis-less macro calls are checked (#1647)", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("k", "101"), path)
  reader <- paste0("read_csv_auto(", DBI::dbQuoteString(con, path), ")")
  DBI::dbWriteTable(con, "t", data.frame(g = c("a", "b"), v = c(1L, 2L)))
  # DuckDB binds a bare current_timestamp as get_current_timestamp().
  for (name in c("hidden_first", "current_user", "get_current_timestamp")) {
    DBI::dbExecute(con, paste0(
      "CREATE MACRO ", name, "(x := 0) AS (SELECT first(k) FROM ", reader, ")"
    ))
  }
  calls <- c("v.hidden_first()", "t.v.hidden_first()", "(v).hidden_first()",
             'v."hidden_first"()', "current_user", "CURRENT_USER",
             '"current_user"', "current_timestamp")
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
