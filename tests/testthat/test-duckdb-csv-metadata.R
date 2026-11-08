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
      expect_equal(nrow(last_sent_queries(details = TRUE)), 0L)
      expect_error(
        inspect_grouping(input, .grouping = grouping_set(where(is.numeric))),
        "dplyr::collect()", fixed = TRUE, class = "marginplyr_error"
      )
      expect_equal(nrow(last_sent_queries(details = TRUE)), 0L)
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
    expect_equal(nrow(last_sent_queries(details = TRUE)), 0L)
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
  for (input in c(list(literal, parquet), ordinary)) {
    plan <- inspect_grouping(
      input, .by = where(is.character),
      .grouping = grouping_set(where(is.numeric))
    )
    expect_identical(plan$fixed, "(label)")
    expect_identical(plan$included, "(k)")
    audit <- last_sent_queries(details = TRUE)
    expect_identical(audit$purpose, "selection_proxy")
    expect_identical(audit$status, "returned")
    expect_match(audit$sql, "LIMIT 0", fixed = TRUE)
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
                     "selection_proxy")
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
      expect_equal(nrow(last_sent_queries(details = TRUE)), 0L)
      expect_error(
        inspect_grouping(source, .grouping = grouping_set(where(is.numeric))),
        "source rows", fixed = TRUE, class = "marginplyr_error"
      )
      expect_equal(nrow(last_sent_queries(details = TRUE)), 0L)
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
  paths <- c(rep("replacement.csv", 6L), rep("replacement.more.csv", 3L))
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  for (i in seq_along(forms)) {
    source <- dplyr::tbl(con, dbplyr::sql(paste(
      "SELECT * FROM", forms[[i]]
    )), vars = "k")
    expect_identical(
      inspect_grouping(source, .grouping = grouping_set(k))$included, "(k)"
    )
    for (value in c("101", "source_tail")) {
      writeLines(c("k", value), paths[[i]])
      native <- dplyr::collect(utils::head(source, 0L))
      expect_identical(is.numeric(native$k), identical(value, "101"))
      expect_error(
        inspect_grouping(source, .by = where(is.numeric)),
        "source rows", fixed = TRUE, class = "marginplyr_error"
      )
      expect_equal(nrow(last_sent_queries(details = TRUE)), 0L)
      expect_error(
        inspect_grouping(source, .grouping = grouping_set(where(is.numeric))),
        "source rows", fixed = TRUE, class = "marginplyr_error"
      )
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
    plan <- inspect_grouping(
      source, .grouping = grouping_set(where(is.numeric))
    )
    expect_identical(plan$included, "(k)")
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
      expect_identical(
        inspect_grouping(source, .grouping = grouping_set(k))$included, "(k)"
      )
      expect_equal(nrow(last_sent_queries(details = TRUE)), 0L)
      # COPY writes valid compressed controls without an extra dependency.
      DBI::dbExecute(con, paste0(
        "COPY (SELECT 101 AS k) TO ", DBI::dbQuoteString(con, actual),
        " (FORMAT CSV, HEADER, COMPRESSION ",
        if (extension == ".csv.gz") "GZIP" else
          if (extension == ".csv.zst") "ZSTD" else "NONE", ")"
      ))
      expect_true(is.numeric(dplyr::collect(utils::head(source, 0L))$k))
      expect_error(
        inspect_grouping(source, .by = where(is.numeric)),
        "source rows", fixed = TRUE, class = "marginplyr_error"
      )
      expect_equal(nrow(last_sent_queries(details = TRUE)), 0L)
      unlink(actual)
    }
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
  writeLines(c("k", "101"), path)
  spelling <- paste0("E", sub(
    ".CSV", "\\x2e\\x43\\x53\\x56", DBI::dbQuoteString(con, path),
    fixed = TRUE
  ))
  source <- dplyr::tbl(con, dbplyr::sql(paste(
    "SELECT * FROM", spelling
  )), vars = "k")
  expect_true(is.numeric(dplyr::collect(utils::head(source, 0L))$k))
  expect_error(
    inspect_grouping(source, .grouping = grouping_set(where(is.numeric))),
    "source rows", fixed = TRUE, class = "marginplyr_error"
  )
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
    for (input in list(source, ordinary)) {
      plan <- inspect_grouping(
        input, .by = where(is.character),
        .grouping = grouping_set(where(is.numeric))
      )
      expect_identical(plan$fixed, "(label)")
      expect_identical(plan$included, "(k)")
      audit <- last_sent_queries(details = TRUE)
      expect_identical(audit$purpose, "selection_proxy")
      expect_identical(audit$status, "returned")
      expect_match(audit$sql, "system.main.query(", fixed = TRUE)
      expect_match(audit$sql, "LIMIT 0", fixed = TRUE)
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
