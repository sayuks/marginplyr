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
    expect_identical(DBI::dbGetQuery(con, "SELECT g FROM main.report")$g,
                     "keep")
  }

  unsorted <- summarize_with_margins(
    source, z = sum(v, na.rm = TRUE), .grouping = rollup(g),
    .id = "sid", .sort = "none"
  )
  out <- dplyr::compute(unsorted,
                        name = DBI::Id(schema = "other", table = "unsorted"),
                        temporary = FALSE)
  expect_identical(names(dplyr::collect(out)), names(expected))
  expect_equal(DBI::dbGetQuery(con,
                               "SELECT COUNT(*) AS n FROM other.unsorted")$n,
               3L)
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
  before <- DBI::dbListTables(con)
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
  explicit_main <- dplyr::compute(
    query, name = DBI::Id(schema = "main", table = "report"),
    temporary = FALSE, overwrite = TRUE
  )
  expect_equal(nrow(dplyr::collect(explicit_main)), 3L)
  expect_identical(DBI::dbGetQuery(con, "SELECT value FROM temp.report")$value,
                   "temp")

  DBI::dbExecute(con, "CREATE TABLE main.persistent (value TEXT)")
  DBI::dbExecute(con, "INSERT INTO main.persistent VALUES ('main')")
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
    expect_equal(nrow(dplyr::collect(dplyr::select(out, g))), 3L)
    expect_equal(nrow(dplyr::collect(dplyr::rename(out, value = z))), 3L)
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
