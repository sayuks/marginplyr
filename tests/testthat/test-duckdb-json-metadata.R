prepare_duckdb_json_tests()

test_that("non-ASCII catalog and CTE names cannot admit a distinct JSON file", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  duckdb_test_json_extension(con)
  directory <- tempfile()
  dir.create(directory)
  previous <- setwd(directory)
  on.exit({
    setwd(previous)
    unlink(directory, recursive = TRUE)
  }, add = TRUE)
  path <- "\u00e4.json"
  table <- "\u00c4.json"
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
    for (value in c('{"k":101}', '{"k":"source_tail"}')) {
      writeLines(value, path)
      expect_identical(is.numeric(dplyr::collect(utils::head(source, 0L))$k),
                       identical(value, '{"k":101}'))
      expect_error(inspect_grouping(
        source, .by = where(is.numeric)
      ), "source rows", fixed = TRUE, class = "marginplyr_error")
      expect_duckdb_catalog_only()
    }
    expect_identical(serialize(source$lazy_query, NULL), before)
    unlink(path)
  }
})

test_that("JSON view admission checks nested reader definitions", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  duckdb_test_json_extension(con)
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  writeLines('{"k":101}', path)
  DBI::dbExecute(con, paste(
    "CREATE VIEW inner_source AS SELECT * FROM", DBI::dbQuoteString(con, path)
  ))
  DBI::dbExecute(con, 'CREATE VIEW "outer.json" AS TABLE inner_source')
  source <- dplyr::tbl(con, dbplyr::sql('TABLE "outer.json"'), vars = "k")
  before <- serialize(source$lazy_query, NULL)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  unlink(path)
  expect_identical(inspect_grouping(
    source, .grouping = grouping_set(k)
  )$included, "(k)")
  expect_equal(nrow(last_sent_queries()), 0L)
  for (value in c('{"k":101}', '{"k":"source_tail"}')) {
    writeLines(value, path)
    expect_identical(is.numeric(dplyr::collect(utils::head(source, 0L))$k),
                     identical(value, '{"k":101}'))
    expect_error(inspect_grouping(
      source, .grouping = grouping_set(where(is.numeric))
    ), "source rows", fixed = TRUE, class = "marginplyr_error")
    expect_duckdb_catalog_only()
  }
  expect_identical(serialize(source$lazy_query, NULL), before)
})

test_that("stored JSON schema and record metadata survive view normalization", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  duckdb_test_json_extension(con)
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  writeLines("INVALID JSON", path)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  for (schema in c(
    "columns={'k':'BIGINT'}, auto_detect=false, records=true",
    "columns={'k':'BIGINT'}, records=false, auto_detect=false",
    "columns={'k':'BIGINT', 'label':'VARCHAR'}"
  )) {
    DBI::dbExecute(con, paste0(
      'CREATE OR REPLACE VIEW "safe.json" AS SELECT * FROM read_json(',
      DBI::dbQuoteString(con, path), ", ", schema, ")"
    ))
    vars <- if (grepl("label", schema, fixed = TRUE)) c("k", "label") else "k"
    source <- dplyr::tbl(con, dbplyr::sql('TABLE "safe.json"'), vars = vars)
    before <- serialize(source$lazy_query, NULL)
    expect_true(is.numeric(dplyr::collect(utils::head(source, 0L))$k))
    expect_identical(inspect_grouping(
      source, .grouping = grouping_set(where(is.numeric))
    )$included, "(k)")
    expect_identical(last_sent_queries(details = TRUE)$purpose,
                     c("relation_catalog", "selection_proxy"))
    expect_identical(last_sent_queries(details = TRUE)$status,
                     c("returned", "returned"))
    expect_error(dplyr::collect(source), "Malformed JSON")
    expect_identical(serialize(source$lazy_query, NULL), before)
  }
})

test_that("typed JSON inspection refuses before source type inference", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  duckdb_test_json_extension(con)
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)

  for (reader in c(
    "read_json", "read_json_auto", "read_ndjson", "read_ndjson_auto",
    '"read_json"'
  )) {
    for (sampling in c("", ", sample_size = -1")) {
      source <- dplyr::tbl(con, dbplyr::sql(paste0(
        "SELECT * FROM ", reader, "(", DBI::dbQuoteString(con, path),
        sampling, ")"
      )), vars = "k")
      before <- serialize(source$lazy_query, NULL)
      expect_identical(
        inspect_grouping(source, .grouping = grouping_set(k))$included, "(k)"
      )
      expect_equal(nrow(last_sent_queries(details = TRUE)), 0L)

      for (value in c("101", "source_tail")) {
        writeLines(paste0(
          '{"k":', if (value == "101") value else '"source_tail"', "}"
        ), path)
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

test_that("JSON replacement scans cannot infer types during inspection", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  duckdb_test_json_extension(con)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  for (extension in c(".json", ".jsonl", ".ndjson")) {
    path <- tempfile(fileext = extension)
    on.exit(unlink(path), add = TRUE)
    for (quote in list(DBI::dbQuoteString, DBI::dbQuoteIdentifier)) {
      source <- dplyr::tbl(con, dbplyr::sql(paste0(
        "SELECT * FROM ", quote(con, path)
      )), vars = "k")
      expect_identical(
        inspect_grouping(source, .grouping = grouping_set(k))$included, "(k)"
      )
      expect_equal(nrow(last_sent_queries(details = TRUE)), 0L)
      for (value in c('{"k":101}', '{"k":"source_tail"}')) {
        writeLines(value, path)
        native <- dplyr::collect(utils::head(source, 0L))
        expect_identical(is.numeric(native$k), identical(value, '{"k":101}'))
        expect_error(
          inspect_grouping(source, .by = where(is.numeric)),
          "source rows", fixed = TRUE, class = "marginplyr_error"
        )
        expect_duckdb_catalog_only()
      }
      unlink(path)
      expect_error(
        inspect_grouping(source, .by = where(is.numeric)),
        "source rows", fixed = TRUE, class = "marginplyr_error"
      )
    }
  }
})

test_that("JSON relation escapes and globs cannot hide type inference", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  duckdb_test_json_extension(con)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  path <- file.path(
    normalizePath(tempdir(), winslash = "/", mustWork = TRUE),
    basename(tempfile(fileext = ".json"))
  )
  glob_path <- paste0(path, "_part")
  on.exit(unlink(c(path, glob_path)), add = TRUE)
  prefix <- substr(path, 1L, nchar(path) - 5L)
  spellings <- c(
    paste0("E'", prefix, "\\x2ejson'"),
    paste0("E'", prefix, "\\056json'"),
    paste0("$$", path, "$$"),
    as.character(DBI::dbQuoteString(con, paste0(path, "?part")))
  )
  for (spelling in spellings) {
    source <- dplyr::tbl(con, dbplyr::sql(paste0(
      "SELECT * FROM ", spelling
    )), vars = "k")
    before <- serialize(source$lazy_query, NULL)
    expect_identical(
      inspect_grouping(source, .grouping = grouping_set(k))$included, "(k)"
    )
    expect_equal(nrow(last_sent_queries(details = TRUE)), 0L)
    for (input in list(source, dplyr::filter(source, k == k))) {
      expect_error(
        inspect_grouping(input, .by = where(is.numeric)),
        "source rows", fixed = TRUE, class = "marginplyr_error"
      )
      expect_duckdb_catalog_only()
      expect_error(
        inspect_grouping(input, .grouping = grouping_set(where(is.numeric))),
        "source rows", fixed = TRUE, class = "marginplyr_error"
      )
      expect_duckdb_catalog_only()
    }
    expect_identical(serialize(source$lazy_query, NULL), before)
  }
  # E and dollar strings remain reader arguments on both DuckDB 1.5 and 2;
  # DuckDB 2 rejects them as filename relations.
  controls <- ifelse(
    startsWith(spellings, "E") | startsWith(spellings, "$"),
    paste0("read_json_auto(", spellings, ")"), spellings
  )
  for (value in c('{"k":101}', '{"k":"source_tail"}')) {
    writeLines(value, path)
    writeLines(value, glob_path)
    for (control in controls) {
      native <- DBI::dbGetQuery(con, paste(
        "SELECT * FROM", control, "LIMIT 0"
      ))
      expect_identical(is.numeric(native$k), identical(value, '{"k":101}'))
    }
  }
})

test_that("JSON-like ordinary table names retain faithful metadata", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  for (name in c("ordinary.json.zstd", "ordinary.json'")) {
    DBI::dbWriteTable(con, name, data.frame(k = 1L))
    source <- dplyr::tbl(con, dbplyr::sql(paste0(
      "SELECT * FROM ", DBI::dbQuoteIdentifier(con, name)
    )), vars = "k")
    plan <- inspect_grouping(
      source, .grouping = grouping_set(where(is.numeric))
    )
    expect_identical(plan$included, "(k)")
  }
})

test_that("qualified JSON names cannot hide replacement inference", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  duckdb_test_json_extension(con)
  directory <- tempfile()
  dir.create(directory)
  previous <- setwd(directory)
  on.exit({
    setwd(previous)
    unlink(directory, recursive = TRUE)
  }, add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  forms <- c(
    "replacement.json", '"replacement".json', 'replacement."json"',
    '"replacement"."json"', "ONLY replacement.json"
  )
  for (form in forms) {
    source <- dplyr::tbl(con, dbplyr::sql(paste(
      "SELECT * FROM", form
    )), vars = "k")
    before <- serialize(source$lazy_query, NULL)
    expect_identical(
      inspect_grouping(source, .grouping = grouping_set(k))$included, "(k)"
    )
    expect_equal(nrow(last_sent_queries(details = TRUE)), 0L)
    for (input in list(source, dplyr::filter(source, k == k))) {
      expect_error(
        inspect_grouping(input, .by = where(is.numeric)),
        "source rows", fixed = TRUE, class = "marginplyr_error"
      )
      expect_duckdb_catalog_only()
      expect_error(
        inspect_grouping(input, .grouping = grouping_set(where(is.numeric))),
        "source rows", fixed = TRUE, class = "marginplyr_error"
      )
      expect_duckdb_catalog_only()
    }
    expect_identical(serialize(source$lazy_query, NULL), before)
  }
  # ONLY is guarded syntactically; DuckDB 2 rejects it as a filename relation.
  native_forms <- forms[!startsWith(forms, "ONLY")]
  for (form in native_forms) {
    for (value in c('{"k":101}', '{"k":"source_tail"}')) {
      writeLines(value, "replacement.json")
      native <- DBI::dbGetQuery(con, paste(
        "SELECT * FROM", form, "LIMIT 0"
      ))
      expect_identical(is.numeric(native$k), identical(value, '{"k":101}'))
    }
    unlink("replacement.json")
  }
})

test_that("TABLE queries cannot hide JSON replacement inference", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  duckdb_test_json_extension(con)
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  writeLines("INVALID JSON", path)
  identifier <- DBI::dbQuoteIdentifier(con, path)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  for (sql in c(
    paste0("TABLE ", identifier),
    paste0("SELECT * FROM (TABLE ", identifier, ") AS t"),
    paste0("TABLE ", identifier, " UNION ALL TABLE ", identifier)
  )) {
    source <- dplyr::tbl(con, dbplyr::sql(sql), vars = "k")
    expect_error(dplyr::collect(utils::head(source, 0L)), "Malformed JSON")
    expect_error(
      inspect_grouping(source, .grouping = grouping_set(where(is.numeric))),
      "source rows", fixed = TRUE, class = "marginplyr_error"
    )
    expect_duckdb_catalog_only()
  }
})

test_that("established JSON schemas retain metadata without sampling", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  duckdb_test_json_extension(con)
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  # Any parse attempt fails, independently of whether conversion is deferred.
  writeLines("INVALID JSON", path)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  ordinary <- dplyr::copy_to(
    con, tibble::tibble(k = 101L, label = "east"), "json_control"
  )
  for (reader in c(
    "read_json", "read_json_auto", "read_ndjson", "read_ndjson_auto"
  )) {
    for (schema in c(
      "columns = {'k': 'INTEGER'}, records = true, auto_detect = false",
      "columns = {'k': 'INTEGER'}, records := 'true'",
      "columns = {'k': 'INTEGER'}, records = false, auto_detect = false",
      "columns = {'k': 'INTEGER', 'label': 'VARCHAR'}"
    )) {
      vars <- if (grepl("label", schema, fixed = TRUE)) c("k", "label") else "k"
      source <- dplyr::tbl(con, dbplyr::sql(paste0(
        "SELECT * FROM ", reader, "(", DBI::dbQuoteString(con, path),
        ", ", schema, ")"
      )), vars = vars)
      before <- serialize(source$lazy_query, NULL)
      inputs <- list(reader = source, filtered = dplyr::filter(source, k == k),
                     table = ordinary)
      for (kind in names(inputs)) {
        input <- inputs[[kind]]
        plan <- inspect_grouping(
          input, .by = where(is.character),
          .grouping = grouping_set(where(is.numeric))
        )
        expect_identical(plan$included, "(k)")
        audit <- last_sent_queries(details = TRUE)
        expected <- c("relation_catalog", "selection_proxy")
        expect_identical(audit$purpose, expected)
        expect_true(all(audit$status == "returned"))
        expect_match(audit$sql[audit$purpose == "selection_proxy"],
                     "LIMIT 0", fixed = TRUE)
      }
      expect_error(dplyr::collect(source), "Malformed JSON")
      expect_identical(serialize(source$lazy_query, NULL), before)
    }
  }
})

test_that("JSON preflight follows joins and ignores SQL values and comments", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  duckdb_test_json_extension(con)
  path <- DBI::dbQuoteString(con, tempfile(fileext = ".json"))
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  for (arguments in c(
    " /* auto_detect=false, records=true */",
    ", auto_detect=false, columns={'k':'INTEGER'} /* records=true */",
    ", auto_detect=false, columns={'k':'INTEGER'}, records='auto'",
    ", columns={'k':'INTEGER'}, dateformat='records=true'",
    # DuckDB applies the last occurrence of a repeated option.
    ", columns={'k':'INTEGER'}, records=true, records='auto'",
    ", auto_detect=false, records=true, auto_detect=true",
    ", auto_detect=false, records=true, union_by_name=true"
  )) {
    source <- dplyr::tbl(con, dbplyr::sql(paste0(
      'SELECT * FROM "read_json" /* outer /* nested */ comment */ (',
      path, arguments, ")"
    )), vars = "k")
    expect_error(
      inspect_grouping(source, .by = where(is.numeric)),
      "source rows", fixed = TRUE, class = "marginplyr_error"
    )
    expect_equal(nrow(last_sent_queries(details = TRUE)), 0L)
  }
  established <- dplyr::tbl(con, dbplyr::sql("SELECT 1 AS k"), vars = "k")
  unsafe <- dplyr::tbl(con, dbplyr::sql(paste0(
    "SELECT * FROM ", path
  )), vars = "k")
  for (input in list(
    dplyr::left_join(established, unsafe, by = "k"),
    dplyr::left_join(unsafe, established, by = "k")
  )) {
    expect_error(
      inspect_grouping(input, .grouping = grouping_set(where(is.numeric))),
      "source rows", fixed = TRUE, class = "marginplyr_error"
    )
    expect_duckdb_catalog_only()
  }
  for (sql in c(
    paste0("SELECT file.k FROM (SELECT 1 AS k) AS control, ", path, " AS file"),
    paste0("SELECT * FROM (", path, ")")
  )) {
    input <- dplyr::tbl(con, dbplyr::sql(sql), vars = "k")
    expect_error(
      inspect_grouping(input, .by = where(is.numeric)),
      "source rows", fixed = TRUE, class = "marginplyr_error"
    )
  }
  literal <- dplyr::tbl(con, dbplyr::sql(paste(
    "SELECT substring('a.json' FROM 1) AS k, $$read_json($$ AS label,",
    "1 AS value /* FROM 'absent.json', read_ndjson('absent.json') */"
  )), vars = c("k", "label", "value"))
  expect_identical(
    inspect_grouping(literal, .grouping = grouping_set(
      where(is.character)
    ))$included,
    "(k, label)"
  )
})

test_that("JSON readers disabling inference pass the guard without a schema", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- DBI::dbQuoteString(con, tempfile(fileext = ".json"))
  for (arguments in c(
    ", auto_detect = false, records = true",
    ", auto_detect := false, records := false",
    ", auto_detect = true, records = true, auto_detect = false",
    ", auto_detect = false, records = true, union_by_name = false"
  )) {
    source <- dplyr::tbl(con, dbplyr::sql(paste0(
      "SELECT * FROM read_json(", path, arguments, ")"
    )), vars = "k")
    # Whatever binding then reports, the guard itself raises nothing.
    refusal <- tryCatch(
      inspect_grouping(source, .grouping = grouping_set(where(is.numeric))),
      error = conditionMessage
    )
    expect_false(any(grepl("DuckDB JSON input", refusal, fixed = TRUE)))
  }
})

test_that("static pivots cannot hide JSON replacement inference", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  duckdb_test_json_extension(con)
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  writeLines("INVALID JSON", path)
  source <- dplyr::tbl(con, dbplyr::sql(paste0(
    "PIVOT ", DBI::dbQuoteString(con, path),
    " ON k IN ('east') USING count(*)"
  )), vars = "east")
  expect_error(dplyr::collect(utils::head(source, 0L)), "Malformed JSON")
  expect_error(
    inspect_grouping(source, .by = where(is.numeric)),
    "source rows", fixed = TRUE, class = "marginplyr_error"
  )
})

test_that("JSON record inference is refused even with explicit column types", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  duckdb_test_json_extension(con)
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  writeLines("INVALID JSON", path)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  source <- dplyr::tbl(con, dbplyr::sql(paste0(
    "SELECT * FROM read_json(", DBI::dbQuoteString(con, path),
    ", auto_detect = false, columns = {'k': 'INTEGER'})"
  )), vars = "k")
  # Malformed contents distinguish record inference from schema-only binding.
  expect_error(dplyr::collect(utils::head(source, 0L)), "Malformed JSON")
  expect_error(
    inspect_grouping(source, .by = where(is.numeric)),
    "source rows", fixed = TRUE, class = "marginplyr_error"
  )
  expect_equal(nrow(last_sent_queries(details = TRUE)), 0L)
})
