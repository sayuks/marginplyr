test_that("internal SQL can be selected without discarding the result", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  remote <- dplyr::copy_to(con, data.frame(g = c("a", "b"), v = 1:2),
                           "label_source")
  query <- summarize_with_margins(
    remote, total = sum(v), .grouping = rollup(g),
    .check_margin_label = TRUE
  )
  recorded <- last_sent_queries()
  internal <- last_sent_queries(include_result = FALSE)
  expect_identical(internal$purpose, "observed_label_collision")
  expect_identical(names(internal), c("purpose", "sql"))
  expect_identical(recorded$purpose, c("observed_label_collision", "result"))
  expect_identical(last_sent_queries(), recorded)
  expect_identical(last_sent_queries(TRUE), recorded)
  expect_identical(recorded$sql[[2L]], as.character(dbplyr::sql_render(query)))
  counts <- DBI::dbGetQuery(con, internal$sql)
  expect_identical(dim(counts), c(1L, 1L))
  expect_equal(counts[[1L]], 0)
  DBI::dbRemoveTable(con, "label_source")
  testthat::local_mocked_bindings(
    sql_render = function(...) stop("accessor rendered SQL"),
    .package = "dbplyr"
  )
  expect_identical(last_sent_queries(FALSE), internal)
  expect_identical(last_sent_queries(), recorded)
  inspect_grouping(data.frame(x = 1))
  expect_identical(last_sent_queries(), dplyr::tibble(
    purpose = character(), sql = character()
  ))
  expect_identical(internal, recorded[1L, ])
})

test_that("view arguments are logical scalars and leave the record intact", {
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  remote <- dbplyr::tbl_lazy(data.frame(g = "a", v = 1))
  summarize_with_margins(remote, total = sum(v), .grouping = rollup(g))
  recorded <- last_sent_queries()
  empty <- dplyr::tibble(purpose = character(), sql = character())
  expect_identical(last_sent_queries(FALSE), empty)
  invalid_values <- list(
    NULL, NA, logical(), c(TRUE, FALSE), 1, "FALSE", list(TRUE)
  )
  for (invalid in invalid_values) {
    expect_error(last_sent_queries(invalid), "include_result",
                 class = "marginplyr_error")
    expect_identical(last_sent_queries(), recorded)
  }
  expect_identical(last_sent_queries(FALSE), empty)
  expect_identical(last_sent_queries(), recorded)
  expect_error(inspect_grouping(data.frame(g = 1), .duplicates = "bogus"),
               class = "marginplyr_error")
  expect_identical(last_sent_queries(FALSE), empty)
  expect_identical(last_sent_queries(), empty)
})

test_that("internal views use the audit setting captured at entry", {
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  empty <- dplyr::tibble(purpose = character(), sql = character())
  for (value in list(NULL, FALSE, 1, NA, "TRUE", c(TRUE, TRUE))) {
    options(marginplyr.audit_sql = value)
    inspect_grouping(data.frame(g = 1))
    options(marginplyr.audit_sql = TRUE)
    expect_error(last_sent_queries(FALSE), "marginplyr.audit_sql",
                 class = "marginplyr_error")
  }
  inspect_grouping(data.frame(g = 1))
  options(marginplyr.audit_sql = FALSE)
  expect_identical(last_sent_queries(FALSE), empty)
  expect_identical(last_sent_queries(), empty)
})

test_that("the public view excludes additional private record fields", {
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  summarize_with_margins(dbplyr::tbl_lazy(data.frame(v = 1)), total = sum(v))
  recorded <- last_sent_queries()
  # A future storage field cannot be supplied through the public API. Extend
  # the fixture here to verify the public projection independently of storage.
  saved <- sent_queries$latest$record$rows
  on.exit({
    sent_queries$latest$record$rows <- saved
  }, add = TRUE)
  sent_queries$latest$record$rows$private <- "private diagnostic"
  expect_identical(last_sent_queries(), recorded)
  expect_identical(last_sent_queries(FALSE), recorded[FALSE, ])
  expect_identical(
    sent_queries$latest$record$rows$private, "private diagnostic"
  )
})

test_that("failed internal renders remain readable in either view", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  remote <- dplyr::copy_to(con, data.frame(g = "a", v = 1), "render_source")
  testthat::local_mocked_bindings(
    sql_render = function(...) stop("diagnostic render failed"),
    .package = "dbplyr"
  )
  expect_error(summarize_with_margins(
    remote, total = sum(v), .grouping = rollup(g), .check_margin_label = TRUE
  ), "diagnostic render failed")
  expected <- dplyr::tibble(
    purpose = "observed_label_collision", sql = NA_character_
  )
  expect_identical(last_sent_queries(FALSE), expected)
  expect_identical(last_sent_queries(), expected)
})

test_that("a refused label check keeps its internal SQL", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  remote <- dplyr::copy_to(con, data.frame(g = "Total", v = 1),
                           "collision_source")
  expect_error(summarize_with_margins(
    remote, total = sum(v), .grouping = rollup(g), .check_margin_label = TRUE
  ), class = "marginplyr_error")
  internal <- last_sent_queries(FALSE)
  expect_identical(internal$purpose, "observed_label_collision")
  expect_false(anyNA(internal$sql))
  expect_identical(last_sent_queries(), internal)
})

test_that("label checks with no columns create no internal SQL", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  remote <- dplyr::copy_to(con, data.frame(g = "a", v = 1), "missing_label")
  summarize_with_margins(
    remote, total = sum(v), .grouping = rollup(g),
    .margin_label = NULL, .check_margin_label = TRUE
  )
  expect_identical(last_sent_queries(FALSE), dplyr::tibble(
    purpose = character(), sql = character()
  ))
  expect_identical(last_sent_queries()$purpose, "result")
})

test_that("DuckDB metadata is an internal view with no result execution", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  remote <- dplyr::copy_to(con, data.frame(g = "a", v = 1), "metadata_source")
  summarize_with_margins(remote, total = sum(v), .grouping = rollup(g))
  recorded <- last_sent_queries()
  internal <- last_sent_queries(FALSE)
  expect_identical(internal$purpose, "selection_proxy")
  expect_identical(nrow(DBI::dbGetQuery(con, internal$sql)), 0L)
  expect_identical(recorded$purpose, c("selection_proxy", "result"))
  expect_identical(last_sent_queries(), recorded)
  inspect_grouping(remote, .grouping = rollup(g))
  expect_identical(last_sent_queries(FALSE), internal)
  expect_identical(last_sent_queries(), internal)
})

test_that("cached dialect checks create no artificial internal SQL", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  saved <- as.list(share_dialect_verdicts, all.names = TRUE)
  on.exit(restore_share_dialect_verdicts(saved), add = TRUE)
  empty_share_dialect_verdicts()
  remote <- dplyr::copy_to(con, data.frame(g = "a", v = 1), "cached_source")
  for (attempt in 1:2) {
    summarize_with_margins(
      remote, total = sum(v), share = share_of_total(total),
      .grouping = rollup(g)
    )
    internal <- last_sent_queries(FALSE)
    if (attempt == 1L) {
      expect_identical(
        internal$purpose,
        c("selection_proxy", "share_dialect", "share_dialect_control")
      )
    } else {
      expect_identical(internal$purpose, "selection_proxy")
    }
  }
})
