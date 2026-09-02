# The record is emptied at the top of every call, so no test here isolates
# `sent_queries` itself; only the option leaks between tests, and every test
# that sets it restores it on exit. ADR 0027 is the decision these assert.

sent_queries_data <- function() {
  data.frame(
    g = c("a", "a", "b"),
    h = c("x", "y", "x"),
    v = 1:3
  )
}

with_audit_option <- function(value, expr) {
  old <- options(marginplyr.audit_sql = value)
  on.exit(options(old), add = TRUE)
  force(expr)
}

sent_queries_table <- function(con) {
  dplyr::copy_to(con, sent_queries_data(), "sent_queries", temporary = TRUE)
  dplyr::tbl(con, "sent_queries")
}

expect_unaudited <- function() {
  condition <- rlang::catch_cnd(
    last_sent_queries(),
    classes = "marginplyr_error"
  )
  expect_s3_class(condition, "marginplyr_error")
  expect_match(
    conditionMessage(condition),
    "marginplyr.audit_sql",
    fixed = TRUE
  )
}

expect_sent_nothing <- function() {
  record <- last_sent_queries()
  expect_s3_class(record, "tbl_df")
  expect_identical(names(record), c("purpose", "sql"))
  expect_type(record$purpose, "character")
  expect_type(record$sql, "character")
  expect_identical(nrow(record), 0L)
}

# --- option off: the primary regression guard --------------------------------

test_that("a local call under the default option records nothing", {
  expect_null(getOption("marginplyr.audit_sql"))
  summarize_with_margins(
    sent_queries_data(),
    total = sum(v, na.rm = TRUE),
    .grouping = rollup(g, h)
  )
  expect_unaudited()
})

test_that("a dtplyr call under the default option records nothing", {
  skip_if_suggest_absent("dtplyr")

  summarize_with_margins(
    dtplyr::lazy_dt(sent_queries_data()),
    total = sum(v, na.rm = TRUE),
    .grouping = rollup(g, h)
  )
  expect_unaudited()
})

test_that("an arrow call under the default option records nothing", {
  skip_if_suggest_absent("arrow")

  summarize_with_margins(
    arrow::arrow_table(sent_queries_data()),
    total = sum(v, na.rm = TRUE),
    .grouping = rollup(g, h)
  )
  expect_unaudited()
})

test_that("an RSQLite call under the default option records nothing", {
  skip_if_suggest_absent("RSQLite", "DBI")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  summarize_with_margins(
    sent_queries_table(con),
    total = sum(v, na.rm = TRUE),
    .grouping = rollup(g, h)
  )
  expect_unaudited()
})

test_that("a DuckDB call under the default option records nothing", {
  skip_if_suggest_absent("duckdb", "DBI")

  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  summarize_with_margins(
    sent_queries_table(con),
    total = sum(v, na.rm = TRUE),
    .grouping = rollup(g, h)
  )
  expect_unaudited()
})

test_that("a value other than TRUE means not audited and raises nothing", {
  for (value in list("yes", NA, 1)) {
    with_audit_option(value, {
      expect_no_error(summarize_with_margins(
        sent_queries_data(),
        total = sum(v, na.rm = TRUE),
        .grouping = rollup(g, h)
      ))
      expect_unaudited()
    })
  }
})

test_that("the option is read at the reset, not when the record is read", {
  # A caller setting the option after the call must be refused, not shown
  # zero rows: the flag belongs to the call, and this call was not audited.
  with_audit_option(FALSE, summarize_with_margins(
    sent_queries_data(),
    total = sum(v, na.rm = TRUE),
    .grouping = rollup(g, h)
  ))
  with_audit_option(TRUE, expect_unaudited())
})

# --- the SQL-not-execution boundary ------------------------------------------

test_that("an audited local call sent nothing", {
  with_audit_option(TRUE, {
    summarize_with_margins(
      sent_queries_data(),
      total = sum(v, na.rm = TRUE),
      .grouping = rollup(g, h)
    )
    expect_sent_nothing()
  })
})

test_that("an audited dtplyr call sent nothing", {
  skip_if_suggest_absent("dtplyr")

  with_audit_option(TRUE, {
    summarize_with_margins(
      dtplyr::lazy_dt(sent_queries_data()),
      total = sum(v, na.rm = TRUE),
      .grouping = rollup(g, h)
    )
    expect_sent_nothing()
  })
})

# --- the result row ----------------------------------------------------------

test_that("an audited RSQLite call records its result query", {
  skip_if_suggest_absent("RSQLite", "DBI")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  remote <- sent_queries_table(con)

  with_audit_option(TRUE, {
    summarize_with_margins(
      remote,
      total = sum(v, na.rm = TRUE),
      .grouping = rollup(g, h)
    )
    record <- last_sent_queries()
  })

  expect_identical(record$purpose, "result")
  # dbplyr's generated SQL is not a contract this package controls, so what is
  # asserted is that the row holds the caller's query and not its exact text.
  expect_match(record$sql, "SELECT", fixed = TRUE)
  expect_match(record$sql, "sent_queries", fixed = TRUE)
  expect_match(record$sql, "total", fixed = TRUE)
})

test_that("the result row is the query the caller receives, unexecuted", {
  skip_if_suggest_absent("RSQLite", "DBI")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  remote <- sent_queries_table(con)

  unaudited <- dplyr::collect(summarize_with_margins(
    remote,
    total = sum(v, na.rm = TRUE),
    .grouping = rollup(g, h)
  ))
  audited <- with_audit_option(TRUE, summarize_with_margins(
    remote,
    total = sum(v, na.rm = TRUE),
    .grouping = rollup(g, h)
  ))

  expect_s3_class(audited, "tbl_lazy")
  expect_identical(dplyr::collect(audited), unaudited)
})

test_that("a multi-line statement is read back as one string per row", {
  skip_if_suggest_absent("RSQLite", "DBI")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  remote <- sent_queries_table(con)

  with_audit_option(TRUE, {
    summarize_with_margins(
      remote,
      total = sum(v, na.rm = TRUE),
      .grouping = rollup(g, h)
    )
    record <- last_sent_queries()
  })

  expect_length(record$sql, 1L)
  expect_gt(length(strsplit(record$sql, "\n", fixed = TRUE)[[1L]]), 1L)
})

# --- one call, and which call ------------------------------------------------

test_that("the second call replaces the first call's rows", {
  skip_if_suggest_absent("RSQLite", "DBI")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  remote <- sent_queries_table(con)

  with_audit_option(TRUE, {
    summarize_with_margins(remote, first = sum(v), .grouping = rollup(g))
    summarize_with_margins(remote, second = sum(v), .grouping = rollup(g))
    record <- last_sent_queries()
  })

  expect_identical(record$purpose, "result")
  expect_match(record$sql, "second", fixed = TRUE)
  expect_no_match(record$sql, "first", fixed = TRUE)
})

test_that("inspect_grouping() after a Margin verb holds only its own rows", {
  skip_if_suggest_absent("RSQLite", "DBI")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  remote <- sent_queries_table(con)

  # A reset in `prepare_margin_operation()` instead would leave the result row
  # below readable here.
  with_audit_option(TRUE, {
    summarize_with_margins(
      remote,
      total = sum(v, na.rm = TRUE),
      .grouping = rollup(g, h)
    )
    inspect_grouping(remote, .grouping = rollup(g, h))
    expect_sent_nothing()
  })
})

# --- a statement with no SQL form --------------------------------------------

test_that("a translation refused at render is recorded as NA", {
  remote <- dbplyr::tbl_lazy(
    sent_queries_data(),
    con = dbplyr::simulate_postgres()
  )

  # `cumsum()` builds as a summary and is refused only when the query is
  # rendered, which is what makes the row `NA` rather than the call an error.
  with_audit_option(TRUE, {
    query <- expect_no_error(
      summarize_with_margins(remote, running = cumsum(v), .grouping = rollup(g))
    )
    record <- last_sent_queries()
  })

  expect_s3_class(query, "tbl_lazy")
  expect_identical(record$purpose, "result")
  expect_identical(record$sql, NA_character_)
  expect_error(dbplyr::sql_render(query), "cumsum")
})

# --- nothing recorded in this session ----------------------------------------

test_that("reading before any call has run is refused", {
  # Every test file before this one has already run a Margin verb, so the
  # branch is asserted against an emptied environment rather than a fresh
  # session; the environment is restored on exit.
  saved <- as.list(sent_queries, all.names = TRUE)
  on.exit(
    {
      rm(list = ls(sent_queries, all.names = TRUE), envir = sent_queries)
      list2env(saved, envir = sent_queries)
    },
    add = TRUE
  )
  rm(list = ls(sent_queries, all.names = TRUE), envir = sent_queries)

  condition <- rlang::catch_cnd(
    last_sent_queries(),
    classes = "marginplyr_error"
  )
  expect_s3_class(condition, "marginplyr_error")
  expect_no_match(
    conditionMessage(condition),
    "marginplyr.audit_sql",
    fixed = TRUE
  )
})

# --- the backend field the record reads --------------------------------------

test_that("grouping_backend() answers is_sql as dialect being present", {
  local <- grouping_backend(sent_queries_data())
  expect_false(local$is_sql)
  expect_null(local$dialect)

  simulated <- grouping_backend(dbplyr::tbl_lazy(
    sent_queries_data(),
    con = dbplyr::simulate_postgres()
  ))
  expect_true(simulated$is_sql)
  expect_false(is.null(simulated$dialect))
})

test_that("grouping_backend() answers is_sql = FALSE for dtplyr", {
  skip_if_suggest_absent("dtplyr")

  backend <- grouping_backend(dtplyr::lazy_dt(sent_queries_data()))
  expect_false(backend$is_sql)
})

# --- the selection proxy row -------------------------------------------------

test_that("an audited DuckDB call records its selection proxy", {
  skip_if_suggest_absent("duckdb", "DBI")

  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  remote <- sent_queries_table(con)

  with_audit_option(TRUE, {
    summarize_with_margins(
      remote,
      total = sum(v, na.rm = TRUE),
      .grouping = rollup(g, h)
    )
    record <- last_sent_queries()
  })

  expect_identical(record$purpose, c("selection_proxy", "result"))
  expect_match(record$sql[[1L]], "sent_queries", fixed = TRUE)
})

test_that("an audited RSQLite call records no selection proxy", {
  skip_if_suggest_absent("RSQLite", "DBI")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  remote <- sent_queries_table(con)

  with_audit_option(TRUE, {
    summarize_with_margins(
      remote,
      total = sum(v, na.rm = TRUE),
      .grouping = rollup(g, h)
    )
    record <- last_sent_queries()
  })

  expect_identical(record$purpose, "result")
})

test_that("an audited arrow call sent nothing", {
  skip_if_suggest_absent("arrow")

  with_audit_option(TRUE, {
    summarize_with_margins(
      arrow::arrow_table(sent_queries_data()),
      total = sum(v, na.rm = TRUE),
      .grouping = rollup(g, h)
    )
    expect_sent_nothing()
  })
})

# --- the label scan row ------------------------------------------------------

test_that("an audited label check records its scan", {
  skip_if_suggest_absent("RSQLite", "DBI")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  remote <- sent_queries_table(con)

  with_audit_option(TRUE, {
    summarize_with_margins(
      remote,
      total = sum(v, na.rm = TRUE),
      .grouping = rollup(g, h),
      .check_margin_label = TRUE
    )
    record <- last_sent_queries()
  })

  expect_identical(record$purpose, c("observed_label_collision", "result"))
  expect_match(record$sql[[1L]], "Total", fixed = TRUE)
})

# --- the dialect probe's rows ------------------------------------------------

# The verdict is cached per dialect for the session, so a probe sends its
# queries only against an empty cache; both tests below empty it the way
# `test-share-backends.R` does and put back what the rest of the suite had.
empty_sent_queries_verdicts <- function() {
  rm(
    list = ls(share_dialect_verdicts, all.names = TRUE),
    envir = share_dialect_verdicts
  )
}

restore_sent_queries_verdicts <- function(saved) {
  empty_sent_queries_verdicts()
  list2env(saved, envir = share_dialect_verdicts)
  invisible(NULL)
}

test_that("an audited DuckDB share records the probe and its control", {
  skip_if_suggest_absent("duckdb", "DBI")

  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  remote <- sent_queries_table(con)
  saved <- as.list(share_dialect_verdicts, all.names = TRUE)
  on.exit(restore_sent_queries_verdicts(saved), add = TRUE)
  empty_sent_queries_verdicts()

  with_audit_option(TRUE, {
    summarize_with_margins(
      remote,
      total = sum(v, na.rm = TRUE),
      share = share_of_parent(total),
      .grouping = rollup(g, h)
    )
    record <- last_sent_queries()
  })

  # DuckDB refuses summing a string, which is the answer the control is sent
  # to tell from a question that could not be put here at all.
  probes <- grep("^share_dialect", record$purpose, value = TRUE)
  expect_identical(probes, c("share_dialect", "share_dialect_control"))
})

test_that("a refused share leaves the probe's row readable", {
  skip_if_suggest_absent("RSQLite", "DBI")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  remote <- sent_queries_table(con)
  saved <- as.list(share_dialect_verdicts, all.names = TRUE)
  on.exit(restore_sent_queries_verdicts(saved), add = TRUE)
  empty_sent_queries_verdicts()

  # SQLite converts a string to a number rather than refusing it, so the share
  # is refused here, after the probe's query has already been recorded.
  with_audit_option(TRUE, {
    condition <- rlang::catch_cnd(
      summarize_with_margins(
        remote,
        total = sum(v, na.rm = TRUE),
        share = share_of_parent(total),
        .grouping = rollup(g, h)
      ),
      classes = "marginplyr_error"
    )
    record <- last_sent_queries()
  })

  expect_s3_class(condition, "marginplyr_error")
  expect_identical(record$purpose, "share_dialect")
})
