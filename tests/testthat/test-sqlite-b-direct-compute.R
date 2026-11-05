test_that("qualified SQLite compute writes and reads one destination", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "ATTACH DATABASE ':memory:' AS other")
  source <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), v = c(2, 5)),
    "b_direct_source", temporary = TRUE
  )
  DBI::dbExecute(con, "CREATE TABLE main.report (g TEXT, sid INT, z REAL)")
  DBI::dbExecute(con, "INSERT INTO main.report VALUES ('keep', 99, -1)")
  DBI::dbExecute(con, "CREATE INDEX main.report_g ON report(g)")
  DBI::dbExecute(con, "ANALYZE main.report")
  main_before <- DBI::dbGetQuery(con, "SELECT * FROM main.report")
  main_schema_before <- DBI::dbGetQuery(
    con, "SELECT * FROM main.sqlite_master ORDER BY type, name"
  )
  main_stats_before <- DBI::dbGetQuery(con, "SELECT * FROM main.sqlite_stat1")
  expected <- data.frame(g = c("a", "b", "Total"),
                         sid = c(1L, 1L, 2L), z = c(2, 5, 7))

  for (name in list(dbplyr::in_schema("other", "report"),
                    DBI::Id(schema = "other", table = "report"))) {
    query <- summarize_with_margins(
      source, z = sum(v, na.rm = TRUE), .grouping = rollup(g),
      .id = "sid", .sort = "last"
    )
    out <- dplyr::compute(query, name = name, temporary = FALSE,
                          overwrite = TRUE, analyze = TRUE)
    expect_equal(as.data.frame(dplyr::collect(out)), expected)
    expect_equal(DBI::dbGetQuery(con, "SELECT g, sid, z FROM other.report"),
                 expected)
    expect_identical(DBI::dbListFields(
      con, DBI::Id(schema = "other", table = "report")
    ), names(expected))
    expect_identical(DBI::dbGetQuery(con, "SELECT * FROM main.report"),
                     main_before)
    expect_identical(DBI::dbGetQuery(
      con, "SELECT * FROM main.sqlite_master ORDER BY type, name"
    ), main_schema_before)
    expect_identical(DBI::dbGetQuery(con, "SELECT * FROM main.sqlite_stat1"),
                     main_stats_before)
    expect_identical(DBI::dbGetQuery(
      con, "SELECT tbl, stat FROM other.sqlite_stat1 WHERE tbl = 'report'"
    )$stat, "3")
  }

  unsorted <- summarize_with_margins(
    source, z = sum(v, na.rm = TRUE), .grouping = rollup(g),
    .id = "sid", .sort = "none"
  )
  out <- dplyr::compute(unsorted,
                        name = DBI::Id(schema = "other", table = "unsorted"),
                        temporary = FALSE)
  unsorted_rows <- DBI::dbGetQuery(
    con, "SELECT g, sid, z FROM other.unsorted ORDER BY sid, g"
  )
  expect_equal(unsorted_rows, expected)
  expect_equal(as.data.frame(dplyr::collect(dplyr::arrange(out, sid, g))),
               expected)
  expect_identical(vapply(unsorted_rows, typeof, character(1)),
                   c(g = "character", sid = "integer", z = "double"))
  expect_identical(DBI::dbListFields(
    con, DBI::Id(schema = "other", table = "unsorted")
  ), names(expected))
  expect_identical(DBI::dbGetQuery(
    con, "SELECT tbl, stat FROM other.sqlite_stat1 WHERE tbl = 'unsorted'"
  )$stat, "3")
  expect_identical(DBI::dbGetQuery(con, "SELECT * FROM main.report"),
                   main_before)
  expect_identical(DBI::dbGetQuery(
    con, "SELECT * FROM main.sqlite_master ORDER BY type, name"
  ), main_schema_before)
  expect_identical(DBI::dbGetQuery(con, "SELECT * FROM main.sqlite_stat1"),
                   main_stats_before)
  DBI::dbBegin(con)
  dplyr::compute(unsorted, name = dbplyr::in_schema("other", "tx_report"),
                 temporary = FALSE, in_transaction = TRUE)
  expect_true(DBI::dbExistsTable(
    con, DBI::Id(schema = "other", table = "tx_report")
  ))
  DBI::dbRollback(con)
  expect_false(DBI::dbExistsTable(
    con, DBI::Id(schema = "other", table = "tx_report")
  ))
})

test_that("unsafe bare destinations are refused before mutation", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), v = c(2, 5)),
    "b_direct_collision_source", temporary = TRUE
  )
  query <- summarize_with_margins(
    source, z = sum(v, na.rm = TRUE), .grouping = rollup(g),
    .id = "sid", .sort = "last"
  )
  DBI::dbExecute(con, "CREATE TABLE temp.report (value TEXT)")
  DBI::dbExecute(con, "INSERT INTO temp.report VALUES ('temp')")
  DBI::dbExecute(con, "CREATE INDEX temp.report_value ON report(value)")
  DBI::dbExecute(con, "ANALYZE temp.report")
  DBI::dbExecute(con, "CREATE TABLE main.persistent (value TEXT)")
  DBI::dbExecute(con, "INSERT INTO main.persistent VALUES ('main')")
  DBI::dbExecute(con, "CREATE INDEX main.persistent_value ON persistent(value)")
  DBI::dbExecute(con, "ANALYZE main.persistent")
  before <- DBI::dbListTables(con)
  temp_schema_before <- DBI::dbGetQuery(
    con, "SELECT * FROM temp.sqlite_master ORDER BY type, name"
  )
  main_schema_before <- DBI::dbGetQuery(
    con, "SELECT * FROM main.sqlite_master ORDER BY type, name"
  )
  temp_stats_before <- DBI::dbGetQuery(con, "SELECT * FROM temp.sqlite_stat1")
  main_stats_before <- DBI::dbGetQuery(con, "SELECT * FROM main.sqlite_stat1")
  for (name in list("report", "REPORT", DBI::Id(table = "report"),
                    dbplyr::ident("report"), dbplyr::ident_q("`report`"),
                    I("`report`"))) {
    expect_error(
      dplyr::compute(query, name = name, temporary = FALSE, overwrite = TRUE),
      "temporary table shadows", class = "marginplyr_error"
    )
  }
  expect_identical(DBI::dbListTables(con), before)
  expect_identical(DBI::dbGetQuery(con, "SELECT value FROM temp.report")$value,
                   "temp")
  expect_identical(DBI::dbGetQuery(
    con, "SELECT * FROM temp.sqlite_master ORDER BY type, name"
  ), temp_schema_before)
  expect_identical(DBI::dbGetQuery(
    con, "SELECT * FROM main.sqlite_master ORDER BY type, name"
  ), main_schema_before)
  expect_identical(DBI::dbGetQuery(con, "SELECT * FROM temp.sqlite_stat1"),
                   temp_stats_before)
  expect_identical(DBI::dbGetQuery(con, "SELECT * FROM main.sqlite_stat1"),
                   main_stats_before)
  expect_identical(DBI::dbGetQuery(
    con, "SELECT value FROM main.persistent"
  )$value, "main")
  explicit_main <- dplyr::compute(
    query, name = DBI::Id(schema = "main", table = "report"),
    temporary = FALSE, overwrite = TRUE
  )
  expect_equal(nrow(dplyr::collect(explicit_main)), 3L)
  expect_identical(DBI::dbGetQuery(con, "SELECT value FROM temp.report")$value,
                   "temp")

  main_schema_before <- DBI::dbGetQuery(
    con, "SELECT * FROM main.sqlite_master ORDER BY type, name"
  )
  main_stats_before <- DBI::dbGetQuery(con, "SELECT * FROM main.sqlite_stat1")
  for (name in list("persistent", "PERSISTENT",
                    DBI::Id(table = "persistent"),
                    dbplyr::ident("persistent"),
                    dbplyr::ident_q("`persistent`"), I("`persistent`"))) {
    expect_error(
      dplyr::compute(query, name = name, temporary = TRUE, overwrite = TRUE),
      "temporary overwrite", class = "marginplyr_error"
    )
  }
  expect_identical(DBI::dbGetQuery(con,
                                   "SELECT value FROM main.persistent")$value,
                   "main")
  expect_false(DBI::dbExistsTable(
    con, DBI::Id(schema = "temp", table = "persistent")
  ))
  expect_identical(DBI::dbGetQuery(con, "SELECT * FROM temp.sqlite_stat1"),
                   temp_stats_before)
  expect_identical(DBI::dbGetQuery(con, "SELECT * FROM main.sqlite_stat1"),
                   main_stats_before)
  expect_identical(DBI::dbGetQuery(
    con, "SELECT * FROM temp.sqlite_master ORDER BY type, name"
  ), temp_schema_before)
  expect_identical(DBI::dbGetQuery(
    con, "SELECT * FROM main.sqlite_master ORDER BY type, name"
  ), main_schema_before)
  out <- dplyr::compute(query, name = "safe_report", temporary = TRUE)
  expect_identical(dplyr::collect(out)$sid, c(1L, 1L, 2L))

  DBI::dbExecute(con, "ATTACH DATABASE ':memory:' AS other")
  DBI::dbExecute(con, "CREATE TABLE other.attached_only (value TEXT)")
  DBI::dbExecute(con,
                 "INSERT INTO other.attached_only VALUES ('other')")
  main <- dplyr::compute(query, name = "attached_only", temporary = FALSE)
  expect_equal(nrow(dplyr::collect(main)), 3L)
  expect_identical(DBI::dbGetQuery(
    con, "SELECT value FROM other.attached_only"
  )$value, "other")
})

test_that("SQLite compute savepoint preserves caller transaction ownership", {
  skip_if_suggest_absent("RSQLite", "DBI")
  for (sort in c("none", "last")) {
    for (flag in c(FALSE, TRUE)) {
      con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
      tryCatch({
        source <- dplyr::copy_to(
          con, data.frame(g = c("a", "b"), v = c(2, 5)),
          "b_direct_tx_source", temporary = TRUE
        )
        DBI::dbExecute(con, "CREATE TABLE main.marker (value TEXT)")
        DBI::dbBegin(con)
        DBI::dbExecute(con, "INSERT INTO marker VALUES ('caller')")
        query <- summarize_with_margins(
          source, z = sum(v, na.rm = TRUE), .grouping = rollup(g),
          .id = "sid", .sort = sort
        )
        out <- dplyr::compute(
          query, name = "tx_report", temporary = FALSE,
          in_transaction = flag
        )
        expect_equal(nrow(dplyr::collect(out)), 3L)
        expect_identical(DBI::dbGetQuery(con,
                                         "SELECT value FROM marker")$value,
                         "caller")
        DBI::dbRollback(con)
        expect_false(DBI::dbExistsTable(con, "tx_report"))
        expect_identical(nrow(DBI::dbGetQuery(con,
                                              "SELECT * FROM marker")), 0L)
        DBI::dbBegin(con)
        DBI::dbExecute(con, "INSERT INTO marker VALUES ('committed')")
        dplyr::compute(query, name = "tx_report", temporary = FALSE,
                       in_transaction = flag)
        DBI::dbCommit(con)
        expect_identical(DBI::dbGetQuery(con,
                                         "SELECT value FROM marker")$value,
                         "committed")
        expect_equal(DBI::dbGetQuery(con,
                                     "SELECT COUNT(*) AS n FROM tx_report")$n,
                     3L)
        dplyr::compute(query, name = "outside_report", temporary = FALSE,
                       in_transaction = flag)
        expect_equal(DBI::dbGetQuery(
          con, "SELECT COUNT(*) AS n FROM outside_report"
        )$n, 3L)
      }, finally = DBI::dbDisconnect(con))
    }
  }
})

test_that("caller commit of SQLite materialization survives reconnect", {
  skip_if_suggest_absent("RSQLite", "DBI")
  path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(path), add = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  tryCatch({
    source <- dplyr::copy_to(
      con, data.frame(g = c("a", "b"), v = c(2, 5)),
      "b_direct_disk_source", temporary = FALSE
    )
    DBI::dbExecute(con, "CREATE TABLE marker (value TEXT)")
    query <- summarize_with_margins(
      source, z = sum(v, na.rm = TRUE), .grouping = rollup(g),
      .id = "sid", .sort = "last"
    )
    DBI::dbBegin(con)
    DBI::dbExecute(con, "INSERT INTO marker VALUES ('caller')")
    dplyr::compute(query, name = "disk_report", temporary = FALSE,
                   in_transaction = TRUE)
    DBI::dbCommit(con)
  }, finally = DBI::dbDisconnect(con))
  reopened <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(reopened), add = TRUE)
  expect_identical(DBI::dbGetQuery(
    reopened, "SELECT value FROM marker"
  )$value, "caller")
  expect_equal(DBI::dbGetQuery(
    reopened, "SELECT COUNT(*) AS n FROM disk_report"
  )$n, 3L)
})

test_that("failed SQLite compute restores overwrite and permits retry", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), v = c(1, 1)),
    "b_direct_failure_source", temporary = TRUE
  )
  DBI::dbExecute(con, "CREATE TABLE main.report (old INT)")
  DBI::dbExecute(con, "INSERT INTO main.report VALUES (42)")
  DBI::dbExecute(con, "CREATE TABLE main.marker (value TEXT)")
  query <- summarize_with_margins(
    source, z = sum(v, na.rm = TRUE), .grouping = rollup(g),
    .id = "sid", .sort = "last"
  )
  before <- DBI::dbListTables(con)
  DBI::dbBegin(con)
  DBI::dbExecute(con, "INSERT INTO marker VALUES ('caller')")
  expect_error(dplyr::compute(
    query, name = "report", temporary = FALSE, overwrite = TRUE,
    unique_indexes = list("z")
  ), "UNIQUE constraint failed")
  expect_identical(DBI::dbListTables(con), before)
  expect_identical(DBI::dbGetQuery(con, "SELECT old FROM main.report")$old,
                   42L)
  expect_identical(DBI::dbGetQuery(con, "SELECT value FROM marker")$value,
                   "caller")
  expect_identical(dplyr::collect(source)$g, c("a", "b"))
  out <- dplyr::compute(query, name = "report", temporary = FALSE,
                        overwrite = TRUE)
  expect_identical(dplyr::collect(out)$sid, c(1L, 1L, 2L))
  DBI::dbRollback(con)
  expect_identical(DBI::dbGetQuery(con, "SELECT old FROM main.report")$old,
                   42L)
})

test_that("rejected self-overwrite preserves the SQLite input", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), v = c(2, 5)),
    "self_overwrite_source", temporary = TRUE
  )
  query <- summarize_with_margins(
    source, z = sum(v, na.rm = TRUE), .grouping = rollup(g),
    .id = "sid", .sort = "last"
  )
  expect_error(dplyr::compute(
    query, name = "self_overwrite_source", temporary = TRUE,
    overwrite = TRUE
  ))
  expect_identical(dplyr::collect(source)$g, c("a", "b"))
  expect_equal(dplyr::collect(source)$v, c(2, 5))
  expect_identical(DBI::dbListFields(con, "self_overwrite_source"),
                   c("g", "v"))
})

test_that("SQLite refuses an existing temporary input as an overwrite target", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  main <- DBI::Id(schema = "main", table = "facts")
  temp <- DBI::Id(schema = "temp", table = "facts")
  DBI::dbExecute(con, "CREATE TABLE main.facts AS SELECT 'keep' AS g")
  DBI::dbExecute(
    con, "CREATE TEMP TABLE facts AS SELECT 'b' AS g UNION ALL SELECT 'a'"
  )
  DBI::dbExecute(con, "CREATE INDEX temp.facts_g ON facts(g)")
  DBI::dbExecute(con, paste(
    "CREATE TEMP VIEW facts_view AS SELECT g FROM facts INDEXED BY facts_g"
  ))
  DBI::dbExecute(con, "CREATE TEMP TABLE report AS SELECT 'old' AS g")
  query <- expand_with_margins(
    dplyr::tbl(con, "facts"), .grouping = rollup(g), .sort = "last"
  )
  expected <- c("a", "b", "Total", "Total")
  DBI::dbBegin(con)
  for (name in list("facts", "FACTS", temp)) {
    expect_error(
      dplyr::compute(query, name = name, overwrite = TRUE),
      "different destination name", class = "marginplyr_error"
    )
    expect_identical(DBI::dbGetQuery(
      con, "SELECT g FROM temp.facts ORDER BY rowid"
    )$g, c("b", "a"))
    expect_identical(DBI::dbGetQuery(con, "SELECT g FROM main.facts")$g, "keep")
    expect_identical(dplyr::collect(query)$g, expected)
  }
  through_view <- expand_with_margins(
    dplyr::tbl(con, "facts_view"), .grouping = rollup(g), .sort = "last"
  )
  expect_error(
    dplyr::compute(through_view, name = temp, overwrite = TRUE),
    "different destination name", class = "marginplyr_error"
  )
  expect_identical(dplyr::collect(through_view)$g, expected)
  out <- dplyr::compute(query, name = "report", overwrite = TRUE)
  expect_identical(dplyr::collect(out)$g, expected)
  DBI::dbRollback(con)
  expect_identical(DBI::dbGetQuery(con, "SELECT g FROM temp.report")$g, "old")

  qualified <- expand_with_margins(
    dplyr::tbl(con, main), .grouping = rollup(g), .sort = "last"
  )
  out <- dplyr::compute(qualified, name = temp, overwrite = TRUE)
  expect_identical(dplyr::collect(out)$g, c("keep", "Total"))
  expect_identical(DBI::dbGetQuery(con, "SELECT g FROM main.facts")$g, "keep")
})

test_that("SQLite temporary overwrite isolates virtual tables by schema", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  fts5 <- DBI::dbGetQuery(
    con, "SELECT sqlite_compileoption_used('ENABLE_FTS5') AS enabled"
  )
  skip_if(fts5$enabled == 0L, "SQLite was built without FTS5")
  DBI::dbExecute(con, "CREATE VIRTUAL TABLE main.facts USING fts5(g)")
  DBI::dbExecute(con, "INSERT INTO main.facts VALUES ('keep')")
  DBI::dbExecute(con, "CREATE TEMP TABLE facts AS SELECT 'old' AS g")
  DBI::dbExecute(con, paste(
    "CREATE VIRTUAL TABLE temp.lookup USING fts5(g, content = 'facts')"
  ))
  temp <- DBI::Id(schema = "temp", table = "facts")
  expand <- function(source) {
    expand_with_margins(
      dplyr::tbl(con, source), .grouping = rollup(g), .sort = "last"
    )
  }
  indirect <- expand(DBI::Id(schema = "temp", table = "lookup"))
  expect_identical(dplyr::collect(indirect)$g, c("old", "Total"))
  expect_error(
    dplyr::compute(indirect, name = temp, overwrite = TRUE),
    "different destination name", class = "marginplyr_error"
  )
  expect_identical(DBI::dbGetQuery(con, "SELECT g FROM temp.facts")$g, "old")

  qualified <- expand(DBI::Id(schema = "main", table = "facts"))
  out <- dplyr::compute(qualified, name = temp, overwrite = TRUE)
  expect_identical(dplyr::collect(out)$g, c("keep", "Total"))
  expect_identical(DBI::dbGetQuery(con, "SELECT g FROM main.facts")$g, "keep")

  DBI::dbExecute(con, "DROP TABLE temp.lookup")
  DBI::dbExecute(con, "DROP TABLE temp.facts")
  DBI::dbExecute(con, "CREATE VIRTUAL TABLE temp.facts USING fts5(g)")
  DBI::dbExecute(con, "CREATE TABLE main.plain AS SELECT 'plain' AS g")
  plain <- expand(DBI::Id(schema = "main", table = "plain"))
  out <- dplyr::compute(plain, name = temp, overwrite = TRUE)
  expect_identical(dplyr::collect(out)$g, c("plain", "Total"))
})

test_that("analysis failure rolls back the owned SQLite savepoint", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), v = c(2, 5)),
    "b_direct_analyze_source", temporary = TRUE
  )
  DBI::dbExecute(con, "CREATE TABLE main.report (old INT)")
  DBI::dbExecute(con, "INSERT INTO main.report VALUES (42)")
  query <- summarize_with_margins(
    source, z = sum(v, na.rm = TRUE), .grouping = rollup(g),
    .id = "sid", .sort = "last"
  )
  injection <- new.env(parent = emptyenv())
  injection$active <- TRUE
  suppressMessages(trace(
    "dbExecute", where = asNamespace("DBI"), print = FALSE,
    tracer = function() {
      statement <- as.character(get("statement", envir = parent.frame()))
      if (injection$active && grepl("^ANALYZE", statement)) {
        stop("injected analysis failure")
      }
    }
  ))
  on.exit(suppressMessages(untrace("dbExecute", where = asNamespace("DBI"))),
          add = TRUE)
  expect_error(dplyr::compute(
    query, name = "report", temporary = FALSE, overwrite = TRUE
  ), "injected analysis failure")
  expect_identical(DBI::dbGetQuery(con, "SELECT old FROM main.report")$old,
                   42L)
  expect_identical(dplyr::collect(source)$g, c("a", "b"))
  injection$active <- FALSE
  out <- dplyr::compute(query, name = "report", temporary = FALSE,
                        overwrite = TRUE)
  expect_equal(nrow(dplyr::collect(out)), 3L)
})

test_that("failed rollback retains the causal SQLite compute error", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), v = c(2, 5)),
    "b_direct_rollback_source", temporary = TRUE
  )
  query <- summarize_with_margins(
    source, z = sum(v, na.rm = TRUE), .grouping = rollup(g),
    .id = "sid", .sort = "last"
  )
  suppressMessages(trace(
    "dbExecute", where = asNamespace("DBI"), print = FALSE,
    tracer = function() {
      statement <- as.character(get("statement", envir = parent.frame()))
      if (grepl("^ANALYZE", statement)) {
        stop("injected analysis failure")
      }
      if (grepl("^ROLLBACK TO", statement)) {
        stop("injected rollback failure")
      }
    }
  ))
  on.exit(suppressMessages(untrace("dbExecute", where = asNamespace("DBI"))),
          add = TRUE)
  err <- tryCatch(
    dplyr::compute(query, name = "rollback_report"),
    error = identity
  )
  expect_match(conditionMessage(err), "savepoint rollback failed")
  expect_match(conditionMessage(err$parent), "injected analysis failure")
  # The injected failure prevented package cleanup; the test owns recovery.
  DBI::dbExecute(con, "ROLLBACK")
  expect_false(DBI::dbExistsTable(con, "rollback_report"))
})

test_that("qualified-index failure restores a SQLite destination", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "ATTACH DATABASE ':memory:' AS other")
  source <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), v = c(2, 5)),
    "b_direct_index_source", temporary = TRUE
  )
  DBI::dbExecute(con, "CREATE TABLE other.report (old INT)")
  DBI::dbExecute(con, "INSERT INTO other.report VALUES (42)")
  query <- summarize_with_margins(
    source, z = sum(v, na.rm = TRUE), .grouping = rollup(g),
    .id = "sid", .sort = "last"
  )
  control <- dplyr::select(source, g)
  expect_error(dplyr::compute(
    control, name = dbplyr::in_schema("other", "ordinary"),
    temporary = FALSE, indexes = list("g")
  ), "Can't create index")
  expect_error(dplyr::compute(
    query, name = dbplyr::in_schema("other", "report"),
    temporary = FALSE, overwrite = TRUE, indexes = list("g")
  ), "Can't create index")
  expect_identical(DBI::dbGetQuery(
    con, "SELECT old FROM other.report"
  )$old, 42L)
  expect_identical(DBI::dbListFields(
    con, DBI::Id(schema = "other", table = "report")
  ), "old")
  out <- dplyr::compute(
    query, name = dbplyr::in_schema("other", "report"),
    temporary = FALSE, overwrite = TRUE, analyze = TRUE
  )
  expect_equal(nrow(dplyr::collect(out)), 3L)
})

test_that("ordinary temporary SQLite compute retains indexes and analysis", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), v = c(2, 5)),
    "b_direct_temp_index_source", temporary = TRUE
  )
  query <- summarize_with_margins(
    source, z = sum(v, na.rm = TRUE), .grouping = rollup(g),
    .sort = "last"
  )
  out <- dplyr::compute(
    query, name = "temp_index_report", temporary = TRUE,
    indexes = list("g"), analyze = TRUE
  )
  expect_equal(as.data.frame(dplyr::collect(out)),
               data.frame(g = c("a", "b", "Total"), z = c(2, 5, 7)))
  index_list <- DBI::dbGetQuery(
    con, "PRAGMA temp.index_list('temp_index_report')"
  )
  expect_identical(nrow(index_list), 1L)
  stats <- DBI::dbGetQuery(con, "SELECT tbl FROM temp.sqlite_stat1")
  expect_true("temp_index_report" %in% stats$tbl)
  expect_false(DBI::dbExistsTable(
    con, DBI::Id(schema = "main", table = "temp_index_report")
  ))
})

test_that("SQLite direct materialization executes one destination insert", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), v = c(2, 5)),
    "b_direct_statement_source", temporary = TRUE
  )
  query <- summarize_with_margins(
    source, z = sum(v, na.rm = TRUE), .grouping = rollup(g),
    .id = "sid", .sort = "last"
  )
  capture <- new.env(parent = emptyenv())
  capture$sql <- character()
  suppressMessages(trace(
    "dbExecute", where = asNamespace("DBI"), print = FALSE,
    tracer = function() {
      # Trace the public DBI execution boundary, including dbplyr's CREATE.
      statement <- get("statement", envir = parent.frame())
      capture$sql <- c(capture$sql, as.character(statement))
    }
  ))
  on.exit(suppressMessages(untrace("dbExecute", where = asNamespace("DBI"))),
          add = TRUE)
  out <- dplyr::compute(query, name = "one_insert", temporary = FALSE)
  expect_equal(nrow(dplyr::collect(out)), 3L)
  expect_identical(sum(grepl("^CREATE TABLE", capture$sql)), 1L)
  expect_identical(sum(grepl("^INSERT INTO", capture$sql)), 1L)
  expect_false(any(grepl("^CREATE TEMP", capture$sql)))
})

test_that("materialized SQLite Margin results support ordinary dplyr verbs", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), v = c(2, 5)),
    "b_direct_downstream_source", temporary = TRUE
  )
  for (sort in c("first", "last")) {
    query <- summarize_with_margins(
      source, z = sum(v, na.rm = TRUE), .grouping = rollup(g),
      .margin_label = NULL, .sort = sort
    )
    out <- dplyr::compute(query, name = paste0("out_", sort))
    expected_g <- if (identical(sort, "first")) {
      c(NA_character_, "a", "b")
    } else {
      c("a", "b", NA_character_)
    }
    expect_identical(dplyr::collect(out)$g, expected_g)
    expect_identical(DBI::dbListFields(con, paste0("out_", sort)),
                     c("g", "z"))
    expected <- data.frame(g = expected_g, z = c(7, 2, 5))
    if (identical(sort, "last")) {
      expected$z <- c(2, 5, 7)
    }
    expect_equal(as.data.frame(dplyr::collect(out)), expected)
    expect_equal(as.data.frame(dplyr::collect(dplyr::select(out, g))),
                 expected["g"])
    expect_equal(as.data.frame(dplyr::collect(
      dplyr::rename(out, value = z)
    )), data.frame(g = expected_g, value = expected$z))
    expect_equal(nrow(dplyr::collect(dplyr::filter(out, z > 2))), 2L)
    expect_equal(nrow(dplyr::collect(dplyr::mutate(out, doubled = z * 2))), 3L)
    expect_equal(nrow(dplyr::collect(dplyr::arrange(out, z))), 3L)
    expect_equal(nrow(dplyr::collect(dplyr::compute(
      dplyr::select(query, g)
    ))), 3L)
  }
  ordinary <- dplyr::summarise(source, z = sum(v, na.rm = TRUE))
  expect_equal(nrow(dplyr::collect(dplyr::select(
    dplyr::compute(ordinary), z
  ))), 1L)
})

test_that("unsorted SQLite compute accepts shadowed rowid aliases", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), v = c(2, 5)),
    "b_direct_unsorted_alias_source", temporary = TRUE
  )
  query <- summarize_with_margins(
    source,
    rowid = sum(v, na.rm = TRUE),
    oid = sum(v, na.rm = TRUE) + 1,
    `_ROWID_` = sum(v, na.rm = TRUE) + 2,
    .grouping = rollup(g), .id = "sid", .sort = "none"
  )
  out <- dplyr::compute(query, name = "unsorted_aliases")
  expect_equal(nrow(dplyr::collect(out)), 3L)
  expect_identical(DBI::dbListFields(con, "unsorted_aliases"),
                   c("g", "sid", "rowid", "oid", "_ROWID_"))
})

test_that("one-set SQLite results retain public types on compute", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), v = c(2, 5)),
    "b_direct_one_set_source", temporary = TRUE
  )
  query <- summarize_with_margins(
    source, z = sum(v, na.rm = TRUE),
    .grouping = grouping_set(g), .id = "sid", .sort = "last"
  )
  direct <- dplyr::collect(query)
  out <- dplyr::compute(query, name = "one_set_report", temporary = TRUE)
  expect_equal(as.data.frame(dplyr::collect(out)), as.data.frame(direct))
  expect_identical(dplyr::collect(out)$sid, c(1L, 1L))
  expect_identical(DBI::dbListFields(con, "one_set_report"),
                   c("g", "sid", "z"))
})

test_that("SQLite compute handles explicit temp and quoted literal names", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "ATTACH DATABASE ':memory:' AS other")
  source <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), v = c(2, 5)),
    "b_direct_names_source", temporary = TRUE
  )
  query <- summarize_with_margins(
    source, z = sum(v, na.rm = TRUE), .grouping = rollup(g),
    .id = "sid", .sort = "last"
  )
  DBI::dbExecute(con, "CREATE TABLE other.report (g TEXT)")
  temp <- dplyr::compute(query, name = dbplyr::in_schema("temp", "report"),
                         temporary = TRUE)
  expect_equal(nrow(dplyr::collect(temp)), 3L)
  expect_identical(DBI::dbListFields(
    con, DBI::Id(schema = "other", table = "report")
  ), "g")
  DBI::dbExecute(con, "CREATE TABLE main.explicit_temp (value TEXT)")
  DBI::dbExecute(con,
                 "INSERT INTO main.explicit_temp VALUES ('main')")
  explicit <- dplyr::compute(
    query, name = dbplyr::in_schema("temp", "explicit_temp"),
    temporary = TRUE, overwrite = TRUE
  )
  expect_equal(nrow(dplyr::collect(explicit)), 3L)
  expect_identical(DBI::dbGetQuery(
    con, "SELECT value FROM main.explicit_temp"
  )$value, "main")
  for (name in list(DBI::Id(table = "literal.dot"),
                    dbplyr::ident("literal.dot"))) {
    out <- dplyr::compute(query, name = name, temporary = TRUE,
                          overwrite = TRUE)
    expect_equal(nrow(dplyr::collect(out)), 3L)
    expect_true(DBI::dbExistsTable(
      con, DBI::Id(schema = "temp", table = "literal.dot")
    ))
  }
  keyword <- dplyr::compute(query, name = DBI::Id(table = "SELECT"),
                            temporary = TRUE)
  expect_equal(nrow(dplyr::collect(keyword)), 3L)
})

test_that("SQLite compute invalid flags leave the destination untouched", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), v = c(2, 5)),
    "b_direct_options_source", temporary = TRUE
  )
  query <- summarize_with_margins(
    source, z = sum(v, na.rm = TRUE), .grouping = rollup(g),
    .id = "sid", .sort = "last"
  )
  for (value in list(NA, 1L, "yes", c(TRUE, FALSE))) {
    expect_error(dplyr::compute(query, name = "invalid_report",
                                in_transaction = value),
                 "in_transaction", class = "marginplyr_error")
  }
  expect_error(dplyr::compute(query, name = "invalid_report",
                              temporary = NA),
               "temporary", class = "marginplyr_error")
  expect_error(dplyr::compute(query, name = "invalid_report",
                              overwrite = NA),
               "overwrite", class = "marginplyr_error")
  expect_error(dplyr::compute(query, name = "invalid_report",
                              analyze = NA),
               "analyze", class = "marginplyr_error")
  expect_false(DBI::dbExistsTable(con, "invalid_report"))
  with_options <- dplyr::compute(
    query, name = "with_options",
    sql_options = dbplyr::sql_options(cte = TRUE)
  )
  expect_equal(nrow(dplyr::collect(with_options)), 3L)
  deprecated <- suppressWarnings(dplyr::compute(
    query, name = "deprecated_cte", cte = TRUE
  ))
  expect_equal(nrow(dplyr::collect(deprecated)), 3L)
  expect_error(dplyr::compute(
    query, name = "exclusive_options", cte = TRUE,
    sql_options = dbplyr::sql_options(cte = TRUE)
  ), "Exactly one")
  expect_false(DBI::dbExistsTable(con, "exclusive_options"))
})

test_that("SQLite direct compute does not change a newer audit record", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  old_options <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old_options), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), v = c(2, 5)),
    "b_direct_audit_source", temporary = TRUE
  )
  first <- summarize_with_margins(
    source, z = sum(v, na.rm = TRUE), .grouping = rollup(g),
    .id = "sid", .sort = "last"
  )
  expect_identical(last_sent_queries()$sql[last_sent_queries()$purpose ==
                                             "result"],
                   as.character(dbplyr::sql_render(first)))
  second <- summarize_with_margins(
    source, z = mean(v, na.rm = TRUE), .grouping = rollup(g),
    .id = "sid", .sort = "none"
  )
  expected <- last_sent_queries()
  expect_identical(expected$sql[expected$purpose == "result"],
                   as.character(dbplyr::sql_render(second)))
  dplyr::collect(first, n = 1L)
  dplyr::compute(first, name = "audit_report")
  expect_identical(last_sent_queries(), expected)
})

test_that("SQLite direct compute keeps the source of a shadowing expansion", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  main <- DBI::Id(schema = "main", table = "facts")
  input <- data.frame(g = c("b", "a"))
  DBI::dbWriteTable(con, main, input)

  for (variant in c("sorted", "sorted_id", "id")) {
    id <- if (variant == "sorted") NULL else "sid"
    sort <- if (variant == "id") "none" else "last"
    expected <- data.frame(g = c("a", "b", "Total", "Total"))
    if (!is.null(id)) expected$sid <- c(1L, 1L, 2L, 2L)
    for (name in list("facts", "FACTS", DBI::Id(schema = "temp",
                                                table = "facts"))) {
      query <- expand_with_margins(
        dplyr::tbl(con, "facts"), .grouping = rollup(g),
        .sort = sort, .id = id
      )
      out <- dplyr::compute(query, name = name)
      if (variant == "id") out <- dplyr::arrange(out, sid, g)
      expect_identical(as.data.frame(dplyr::collect(out)), expected)
      temp <- DBI::Id(schema = "temp", table = "facts")
      expect_identical(DBI::dbListFields(con, temp), names(expected))
      expect_identical(DBI::dbReadTable(con, main), input)
      DBI::dbRemoveTable(con, temp)
    }
  }
})

test_that("SQLite shadowing compute preserves opaque and qualified sources", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "ATTACH DATABASE ':memory:' AS aux")
  source_id <- DBI::Id(schema = "aux", table = "facts")
  input <- data.frame(g = c("b", "a"))
  DBI::dbWriteTable(con, source_id, input)

  expected <- data.frame(g = c("a", "b", "Total", "Total"),
                         sid = c(1L, 1L, 2L, 2L))
  opaque <- dplyr::tbl(con, dbplyr::sql("SELECT g FROM facts"), vars = "g")
  query <- expand_with_margins(
    opaque, .grouping = rollup(g), .sort = "last", .id = "sid"
  )
  for (name in c("expanded", "facts")) {
    out <- dplyr::compute(query, name = name, indexes = list("sid"))
    expect_identical(as.data.frame(dplyr::collect(out)), expected)
    expect_true(name %in% DBI::dbGetQuery(con, paste(
      "SELECT tbl_name FROM temp.sqlite_master WHERE type = 'index'",
      "AND name = ?"
    ), params = list(paste0(name, "_sid")))$tbl_name)
    DBI::dbRemoveTable(con, DBI::Id(schema = "temp", table = name))
  }

  qualified <- expand_with_margins(
    dplyr::tbl(con, source_id), .grouping = rollup(g),
    .sort = "last", .id = "sid"
  )
  out <- dplyr::compute(qualified, name = "facts")
  expect_identical(as.data.frame(dplyr::collect(out)), expected)
  expect_identical(DBI::dbReadTable(con, source_id), input)
})

test_that("failed SQLite shadowing compute restores the temp schema", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  main <- DBI::Id(schema = "main", table = "facts")
  input <- data.frame(g = c("b", "a"))
  DBI::dbWriteTable(con, main, input)
  query <- expand_with_margins(
    dplyr::tbl(con, "facts"), .grouping = rollup(g), .sort = "last"
  )
  temp_schema <- function() {
    DBI::dbGetQuery(con, "SELECT * FROM temp.sqlite_master ORDER BY name")
  }
  before <- temp_schema()

  expect_error(
    dplyr::compute(query, name = "facts", indexes = list("missing")),
    "All columns specified through", class = "rlang_error"
  )
  expect_identical(temp_schema(), before)

  expect_error(dplyr::compute(query, name = "FACTS", overwrite = TRUE),
               "explicit schema or another name", class = "marginplyr_error")
  expect_identical(temp_schema(), before)

  expect_error(
    dplyr::compute(query, name = "facts", unique_indexes = list("g")),
    "UNIQUE constraint failed"
  )
  expect_identical(temp_schema(), before)
  expect_identical(DBI::dbReadTable(con, main), input)
  expect_identical(dplyr::collect(dplyr::compute(query, name = "facts"))$g,
                   c("a", "b", "Total", "Total"))
})

test_that("SQLite shadowing compute ignores unrelated invalid temp views", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  main <- DBI::Id(schema = "main", table = "facts")
  DBI::dbWriteTable(con, main, data.frame(g = c("b", "a")))
  DBI::dbExecute(con, "CREATE TEMP VIEW broken AS SELECT * FROM missing")
  rename_mode <- DBI::dbGetQuery(con, "PRAGMA legacy_alter_table")

  for (source in list("facts", main)) {
    query <- expand_with_margins(
      dplyr::tbl(con, source), .grouping = rollup(g), .sort = "last"
    )
    out <- dplyr::compute(query, name = "facts")
    expect_identical(dplyr::collect(out)$g, c("a", "b", "Total", "Total"))
    DBI::dbRemoveTable(con, DBI::Id(schema = "temp", table = "facts"))
  }
  expect_identical(DBI::dbGetQuery(con, "PRAGMA legacy_alter_table"),
                   rename_mode)
  expect_true(DBI::dbExistsTable(
    con, DBI::Id(schema = "temp", table = "broken")
  ))

  DBI::dbExecute(con, "CREATE VIEW main.unavailable AS SELECT * FROM missing")
  out <- dplyr::compute(query, name = "unavailable")
  expect_identical(dplyr::collect(out)$g, c("a", "b", "Total", "Total"))
  expect_true(DBI::dbExistsTable(
    con, DBI::Id(schema = "main", table = "unavailable")
  ))
})

test_that("SQLite compute preserves a source using a table-valued function", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE payload (b INTEGER, a INTEGER)")
  source <- dplyr::tbl(
    con, dbplyr::sql("SELECT name AS g FROM pragma_table_info('payload')"),
    vars = "g"
  )
  query <- expand_with_margins(source, .grouping = rollup(g), .sort = "last")
  out <- dplyr::compute(query, name = "pragma_table_info")
  expect_identical(dplyr::collect(out)$g, c("a", "b", "Total", "Total"))
})

test_that("SQLite generated compute names avoid the input relation", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  seed_name <- ".Random.seed"
  seed <- get0(seed_name, envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (is.null(seed)) {
      rm(seed_name, envir = .GlobalEnv)
    } else {
      assign(seed_name, seed, envir = .GlobalEnv)
    }
  }, add = TRUE)
  seed_table <- dplyr::copy_to(con, data.frame(g = "seed"), "seed")
  set.seed(1282)
  destination <- dplyr::compute(seed_table)
  name <- dbplyr::remote_name(destination)
  DBI::dbRemoveTable(con, name)
  DBI::dbWriteTable(con, name, data.frame(g = c("b", "a")))
  query <- expand_with_margins(
    dplyr::tbl(con, name), .grouping = rollup(g), .sort = "last"
  )
  set.seed(1282)
  expect_identical(dplyr::collect(dplyr::compute(query))$g,
                   c("a", "b", "Total", "Total"))
  expect_identical(DBI::dbReadTable(con, name)$g, c("b", "a"))
})

test_that("SQLite refuses a shadowing name before an unavailable rename", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "facts", data.frame(g = c("b", "a")))
  query <- expand_with_margins(
    dplyr::tbl(con, "facts"), .grouping = rollup(g), .sort = "last"
  )
  get_query <- DBI::dbGetQuery
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, ...) {
      if (identical(statement, "PRAGMA legacy_alter_table")) {
        return(data.frame())
      }
      get_query(conn, statement, ...)
    },
    .package = "DBI"
  )
  execute <- DBI::dbExecute
  local_mocked_bindings(
    dbExecute = function(conn, statement, ...) {
      expect_false(grepl("^(CREATE|ALTER|INSERT)", statement))
      execute(conn, statement, ...)
    },
    .package = "DBI"
  )

  expect_error(
    dplyr::compute(query, name = "facts"),
    "Choose a different destination name", class = "marginplyr_error"
  )
  expect_false(DBI::dbExistsTable(
    con, DBI::Id(schema = "temp", table = "facts")
  ))
  expect_identical(DBI::dbReadTable(con, "facts")$g, c("b", "a"))
})

test_that("SQLite generated compute names propagate a failed name probe", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "facts", data.frame(g = c("b", "a")))
  query <- expand_with_margins(
    dplyr::tbl(con, "facts"), .grouping = rollup(g), .sort = "last"
  )
  get_query <- DBI::dbGetQuery
  failure <- simpleError("name lookup failed")
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, ...) {
      if (startsWith(statement, "EXPLAIN SELECT 1 FROM")) stop(failure)
      get_query(conn, statement, ...)
    },
    .package = "DBI"
  )

  outcome <- tryCatch(dplyr::compute(query), error = identity)
  expect_identical(outcome, failure)
  catalog <- DBI::dbGetQuery(con, "SELECT name FROM temp.sqlite_master")
  expect_identical(catalog$name, character())
  expect_identical(DBI::dbReadTable(con, "facts")$g, c("b", "a"))
})
