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
