# The record is emptied at the top of every call, so no test here isolates
# `sent_queries` itself, save the one asserting what a session with no call in
# it answers -- which is the one state a call cannot put back. Two other pieces
# of state leak between tests and are restored on exit by every test that
# writes one: the option, and the per-dialect verdict cache the share tests
# below empty so that the probe sends its queries at all. ADR 0027 is the
# decision these assert.

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

# Which of the accessor's two refusals it raised: `TRUE` for the unaudited one,
# `FALSE` for the session's first, and `NA` where it answered rather than
# refusing. Both refusals are marginplyr errors, so naming
# `marginplyr.audit_sql` is the whole of what tells them apart.
refusal_names_the_option <- function() {
  condition <- rlang::catch_cnd(
    last_sent_queries(),
    classes = "marginplyr_error"
  )
  if (is.null(condition)) {
    return(NA)
  }
  grepl("marginplyr.audit_sql", conditionMessage(condition), fixed = TRUE)
}

expect_unaudited <- function() {
  expect_identical(refusal_names_the_option(), TRUE)
}

expect_nothing_recorded <- function() {
  expect_identical(refusal_names_the_option(), FALSE)
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

# --- a call whose input is another call --------------------------------------

# `|>` expands to `g(f(x))`, so `f` runs while `g` forces `.data`, and which
# call the record belongs to turns on whether `g` empties it before or after
# that forcing. Emptying first leaves the record spanning both, which is the
# `dbplyr::last_sql()` defect ADR 0027 exists to remove, reached by the
# idiomatic way of writing the call (#455). RSQLite records no selection proxy,
# so each of these reads a record whose whole content is result rows.

test_that("a piped Margin verb records only the outer call's result", {
  skip_if_suggest_absent("RSQLite", "DBI")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  remote <- sent_queries_table(con)

  with_audit_option(TRUE, {
    query <- remote |>
      summarize_with_margins(
        inner = sum(v, na.rm = TRUE),
        .grouping = rollup(g)
      ) |>
      summarize_with_margins(
        outer = sum(inner, na.rm = TRUE),
        .grouping = rollup(g)
      )
    record <- last_sent_queries()
  })

  # One row, not two: `"result"` is the one promised `purpose`, and a reader
  # matching on it must find the query they were handed and no other. The
  # outer query nests the input's, so its text holds the input's too, and what
  # tells the two apart is which query the row is -- not a substring absent
  # from one of them, as it is where the two calls read the same table.
  expect_identical(record$purpose, "result")
  expect_identical(record$sql, as.character(dbplyr::sql_render(query)))
})

test_that("a piped expand_with_margins() records only its own result", {
  skip_if_suggest_absent("RSQLite", "DBI")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  remote <- sent_queries_table(con)

  with_audit_option(TRUE, {
    query <- remote |>
      summarize_with_margins(
        inner = sum(v, na.rm = TRUE),
        .grouping = rollup(g)
      ) |>
      expand_with_margins(.grouping = rollup(g))
    record <- last_sent_queries()
  })

  expect_identical(record$purpose, "result")
  expect_identical(record$sql, as.character(dbplyr::sql_render(query)))
})

test_that("a dplyr verb between two Margin verbs changes nothing", {
  skip_if_suggest_absent("RSQLite", "DBI")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  remote <- sent_queries_table(con)

  with_audit_option(TRUE, {
    remote |>
      summarize_with_margins(
        inner = sum(v, na.rm = TRUE),
        .grouping = rollup(g)
      ) |>
      dplyr::filter(inner > 0) |>
      expand_with_margins(.grouping = rollup(g))
    record <- last_sent_queries()
  })

  expect_identical(record$purpose, "result")
})

test_that("inspect_grouping() piped from a Margin verb sends nothing", {
  skip_if_suggest_absent("RSQLite", "DBI")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  remote <- sent_queries_table(con)

  # The zero-row answer is the third of the four, and reaching it here is what
  # says the record belongs to the call that sent nothing rather than to the
  # one that filled it.
  with_audit_option(TRUE, {
    remote |>
      summarize_with_margins(
        inner = sum(v, na.rm = TRUE),
        .grouping = rollup(g)
      ) |>
      inspect_grouping(.grouping = rollup(g))
    expect_sent_nothing()
  })
})

test_that("an outer verb refused after its input ran reports its own record", {
  skip_if_suggest_absent("RSQLite", "DBI")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  remote <- sent_queries_table(con)

  # A refusal inside the body takes the third answer (ADR 0027), and an input
  # that recorded rows of its own is where that is hardest to hold: those rows
  # are in the record when the outer call begins.
  with_audit_option(TRUE, {
    condition <- rlang::catch_cnd(
      remote |>
        summarize_with_margins(
          inner = sum(v, na.rm = TRUE),
          .grouping = rollup(g)
        ) |>
        summarize_with_margins(
          outer = sum(inner, na.rm = TRUE),
          .grouping = rollup(g),
          .duplicates = "bogus"
        ),
      classes = "marginplyr_error"
    )
    expect_s3_class(condition, "marginplyr_error")
    expect_sent_nothing()
  })
})

test_that("an input that refuses leaves its own record readable", {
  skip_if_suggest_absent("RSQLite", "DBI")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  remote <- sent_queries_table(con)
  saved <- as.list(share_dialect_verdicts, all.names = TRUE)
  on.exit(restore_share_dialect_verdicts(saved), add = TRUE)
  empty_share_dialect_verdicts()

  # The other direction of the same forcing: the input raises, so the outer
  # call never reaches its own reset and the record left readable is the
  # input's, holding every query it had already sent.
  with_audit_option(TRUE, {
    condition <- rlang::catch_cnd(
      remote |>
        summarize_with_margins(
          total = sum(v, na.rm = TRUE),
          share = share_of_parent(total),
          .grouping = rollup(g, h)
        ) |>
        summarize_with_margins(
          outer = sum(total, na.rm = TRUE),
          .grouping = rollup(g)
        ),
      classes = "marginplyr_error"
    )
    record <- last_sent_queries()
  })

  expect_s3_class(condition, "marginplyr_error")
  expect_identical(record$purpose, "share_dialect")
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
# queries only against an empty cache; both tests below empty it through
# `helper-share-dialect-verdicts.R`.

test_that("an audited DuckDB share records the probe and its control", {
  skip_if_suggest_absent("duckdb", "DBI")

  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  remote <- sent_queries_table(con)
  saved <- as.list(share_dialect_verdicts, all.names = TRUE)
  on.exit(restore_share_dialect_verdicts(saved), add = TRUE)
  empty_share_dialect_verdicts()

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
  on.exit(restore_share_dialect_verdicts(saved), add = TRUE)
  empty_share_dialect_verdicts()

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

# --- a call refused before its plan ------------------------------------------

# The entry points, each with an argument it refuses in the validation it opens
# with -- before a Grouping plan is compiled, and so before any query could
# have been sent. `.duplicates` is the one option every entry point takes, and
# a local input keeps what is under test the refusal rather than the backend.
refused_entry_point_calls <- function() {
  lapply(verbs_taking(".grouping"), function(verb) {
    args <- list(
      quote(sent_queries_data()),
      .grouping = quote(rollup(g)),
      .duplicates = "bogus"
    )
    if (verb %in% c("summarize_with_margins", "summarise_with_margins")) {
      args <- c(args, list(total = quote(sum(v, na.rm = TRUE))))
    }
    list(verb = verb, args = args)
  })
}

test_that("a call refused before its plan reports its own empty record", {
  skip_if_suggest_absent("RSQLite", "DBI")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  remote <- sent_queries_table(con)

  # The verbs whose refused call left the previous call's record readable,
  # rather than one expectation per case: which entry point is stale is what a
  # failure here has to say, and the case is not otherwise in the report.
  stale <- character()

  for (case in refused_entry_point_calls()) {
    with_audit_option(TRUE, {
      summarize_with_margins(
        remote,
        total = sum(v, na.rm = TRUE),
        .grouping = rollup(g, h)
      )
      recorded <- nrow(last_sent_queries())
      condition <- rlang::catch_cnd(
        eval(rlang::call2(case$verb, !!!case$args)),
        classes = "marginplyr_error"
      )
      record <- last_sent_queries()
    })
    # A prior call that recorded nothing would leave zero rows to empty, and
    # a case that raised nothing refused at no point at all: either passes the
    # count on a record this test never put anything into.
    if (recorded == 0L || is.null(condition) || nrow(record) != 0L) {
      stale <- c(stale, case$verb)
    }
  }

  expect_identical(stale, character())
  # The count above is all the loop reads, so the zero-row answer's shape is
  # asserted once, on the record the last case left.
  expect_sent_nothing()
})

test_that("a call refused before its plan reads the option for itself", {
  skip_if_suggest_absent("RSQLite", "DBI")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  remote <- sent_queries_table(con)

  with_audit_option(TRUE, summarize_with_margins(
    remote,
    total = sum(v, na.rm = TRUE),
    .grouping = rollup(g, h)
  ))
  with_audit_option(FALSE, {
    expect_error(
      summarize_with_margins(
        remote,
        total = sum(v, na.rm = TRUE),
        .grouping = rollup(g, h),
        .duplicates = "bogus"
      ),
      class = "marginplyr_error"
    )
  })

  # The refused call was the unaudited one, so the flag it left is what the
  # accessor reports -- not the audited flag of the call before it.
  expect_unaudited()

  # And the other direction, where a stale flag would refuse a call that was
  # audited rather than answer it with the zero rows it sent.
  summarize_with_margins(
    remote,
    total = sum(v, na.rm = TRUE),
    .grouping = rollup(g, h)
  )
  with_audit_option(TRUE, {
    expect_error(
      summarize_with_margins(
        remote,
        total = sum(v, na.rm = TRUE),
        .grouping = rollup(g, h),
        .duplicates = "bogus"
      ),
      class = "marginplyr_error"
    )
  })
  expect_sent_nothing()
})

# `expr` evaluated against a record holding nothing, which is put back
# afterwards. The state a later call cannot restore is the session's first, so
# reaching it more than once means saving the whole record and emptying it.
with_empty_record <- function(expr) {
  saved <- as.list(sent_queries, all.names = TRUE)
  empty_the_record <- function() {
    rm(list = names(saved), envir = sent_queries)
  }
  on.exit(
    {
      empty_the_record()
      list2env(saved, envir = sent_queries)
    },
    add = TRUE
  )

  empty_the_record()
  force(expr)
}

test_that("a call refused before its plan is a call the session recorded", {
  with_empty_record({
    # The control: with nothing recorded, the accessor refuses rather than
    # answering, which is what makes the read after the refusal an assertion.
    expect_nothing_recorded()

    with_audit_option(TRUE, {
      expect_error(
        summarize_with_margins(
          sent_queries_data(),
          total = sum(v, na.rm = TRUE),
          .grouping = rollup(g),
          .duplicates = "bogus"
        ),
        class = "marginplyr_error"
      )
    })

    # A verb that began and then refused the call has begun, so the answer is
    # its own empty record and not the session's first (ADR 0027).
    expect_sent_nothing()
  })
})

# The first of the four answers is reached when nothing has been recorded, and
# every entry point moves the session off it -- `inspect_grouping()` included,
# which compiles a Grouping plan without a Margin operation and empties the
# record like the rest (ADR 0027). The reference states that condition once,
# and this is what holds it: an entry point that stopped recording would leave
# the session answering the first where the page promises the second.
test_that("any entry point moves the session off the first answer", {
  # The wrappers are what this runs, so the two are held to each other here as
  # every other caller holds them (`helper-margin-verbs.R`): a seventh verb
  # missing from the list would otherwise leave the loop covering six of seven
  # under a name saying it covers every entry point.
  expect_setequal(names(forwarded_verbs), verbs_taking(".grouping"))

  data <- data.frame(g = c("a", "a", "b"), value = 1:3)

  # The entry point after which the accessor did not give the unaudited
  # refusal -- the session either still answering its first or answering with
  # rows -- rather than one expectation per verb: which one it is not otherwise
  # in the report.
  unmoved <- character()

  for (name in names(forwarded_verbs)) {
    with_empty_record({
      expect_nothing_recorded()
      # Unaudited, the option being unset, so the answer the call moves to is
      # the second and its message names `marginplyr.audit_sql`.
      expect_null(getOption("marginplyr.audit_sql"))
      forwarded_verbs[[name]](data, grouping = rollup(g))
      if (!isTRUE(refusal_names_the_option())) {
        unmoved <- c(unmoved, name)
      }
    })
  }

  expect_identical(unmoved, character())
})

# --- the reset site, structurally --------------------------------------------

# Which functions empty the record is a property of every entry point rather
# than of any one call, so it is read from the loaded namespace; the shared
# visitor and enumeration come from `helper-namespace-walk.R`, whose header
# says why a structural gate reads a namespace at all.

# Whether `fn` empties the record anywhere in its body.
empties_the_record <- function(fn) {
  found <- FALSE
  visit_calls(body(fn), function(node) {
    head <- node[[1]]
    if (is.name(head) && identical(as.character(head), "reset_sent_queries")) {
      found <<- TRUE
    }
  })
  found
}

# The head of the counter call covr's wrapper holds, assembled rather than
# written as a `:::` call. `R CMD check`'s *unstated dependencies in tests*
# reads a `:::` in a test source from the parse tree, so spelling it there
# makes covr a dependency this package would have to declare -- and covr is
# supplied by `test-coverage.yaml`'s `extra-packages`, belonging in no
# dependency field of its own (`AGENTS.md`, *Dependency metadata*). Assembling
# it names the same call without putting the token where that scan reads.
coverage_counter <- function() {
  call(":::", as.name("covr"), as.name("count"))
}

# `expr` with covr's instrumentation taken off, where it has any.
#
# covr measures a namespace by replacing each statement in it with
# `if (TRUE) { covr:::count(<key>); <statement> }`, so a reader that takes a
# statement by position takes covr's wrapper rather than the statement. Every
# other structural gate in this suite goes through `visit_calls()`, which finds
# a call wherever the wrapper puts it; this file holds the one that reads a
# position, and the coverage job is where it reported all six entry points at
# once.
#
# Read through the wrapper rather than skip under covr: a gate that stops
# asserting in one job reads exactly like a gate nothing violates, which is the
# failure a structural gate exists to prevent. The shape is covr's and not
# documented, so a covr that changed it fails this gate instead of quieting it
# -- the direction the reading has to fail in, and why no `skip()` is here.
strip_coverage_wrapper <- function(expr) {
  if (!is.call(expr) || !identical(expr[[1]], quote(`if`)) ||
        length(expr) != 3L || !identical(expr[[2]], TRUE)) {
    return(expr)
  }
  branch <- expr[[3]]
  if (!is.call(branch) || !identical(branch[[1]], quote(`{`)) ||
        length(branch) != 3L) {
    return(expr)
  }
  counter <- branch[[2]]
  if (!is.call(counter) || !identical(counter[[1]], coverage_counter())) {
    return(expr)
  }
  branch[[3]]
}

# The `n`th expression of `fn`'s body, or `NULL` where the body has fewer. An
# unbraced body is one expression, so it answers `n == 1` and nothing else.
# Both readings go through the unwrapping above, because covr wraps an unbraced
# body whole and wraps each statement of a braced one.
nth_statement <- function(fn, n) {
  fn_body <- strip_coverage_wrapper(body(fn))
  if (!is.call(fn_body) || !identical(as.character(fn_body[[1]]), "{")) {
    if (n == 1L) {
      return(fn_body)
    }
    return(NULL)
  }
  if (length(fn_body) < n + 1L) {
    return(NULL)
  }
  strip_coverage_wrapper(fn_body[[n + 1L]])
}

test_that("every entry point forces its input and then empties the record", {
  ns <- asNamespace("marginplyr")
  # An entry point is an exported function that compiles a Grouping plan of
  # its own, and `.grouping` is how a specification reaches one. One taking a
  # plan by some other route would go unread, which the equality below is what
  # reports: it is the function calling `reset_sent_queries()` from outside
  # this set.
  entry_points <- verbs_taking(".grouping")

  emptying <- Filter(
    function(name) empties_the_record(get(name, envir = ns)),
    namespace_functions(ns)
  )
  # Both directions at once: an entry point that stopped emptying the record
  # leaves the record spanning two calls, and a function that empties it
  # part-way through one truncates that call's own (ADR 0027).
  expect_setequal(emptying, entry_points)

  # `force(.data)` first and the reset second, in that order. Emptying the
  # record before the input's promise is forced attributes to this call
  # whatever the call that wrote `.data` recorded, and forcing the promise
  # after any other statement is a statement running under the previous call's
  # record (#455).
  misordered <- Filter(
    function(name) {
      fn <- get(name, envir = ns)
      !identical(nth_statement(fn, 1L), quote(force(.data))) ||
        !identical(nth_statement(fn, 2L), quote(reset_sent_queries()))
    },
    entry_points
  )
  # Named rather than counted: a validation moved above either statement is
  # what this fires on, and which entry point took it is not otherwise in the
  # report.
  expect_identical(misordered, character())
})

test_that("the reset scan tells an opening statement from a later one", {
  # Both readings run over synthetic functions rather than over a member of the
  # namespace, since a member that failed either is what the gate above reports.
  opens_correctly <- function() {
    force(.data)
    reset_sent_queries()
    stop("unreachable")
  }
  resets_later <- function() {
    stop("unreachable")
    reset_sent_queries()
  }
  swapped <- function() {
    reset_sent_queries()
    force(.data)
  }
  validates_between <- function() {
    force(.data)
    stop("unreachable")
    reset_sent_queries()
  }
  bare <- function() reset_sent_queries()
  reset <- quote(reset_sent_queries())
  forced <- quote(force(.data))

  expect_true(empties_the_record(opens_correctly))
  expect_true(empties_the_record(resets_later))
  expect_false(empties_the_record(function() NULL))
  expect_identical(nth_statement(opens_correctly, 1L), forced)
  expect_identical(nth_statement(opens_correctly, 2L), reset)
  # Each way the two statements can be wrong is a distinct reading, and the
  # gate above is an `||` over both, so neither position may pass on its own:
  # a body missing the forcing, a body holding both in the other order, and a
  # body whose forcing is right with a statement pushed between the two. The
  # last is the only one the second position alone rejects, and is what a
  # validation moved above the reset would look like.
  expect_false(identical(nth_statement(resets_later, 1L), forced))
  expect_identical(nth_statement(swapped, 1L), reset)
  expect_false(identical(nth_statement(swapped, 2L), reset))
  expect_identical(nth_statement(validates_between, 1L), forced)
  expect_false(identical(nth_statement(validates_between, 2L), reset))
  # An unbraced body is the statement itself, which the gate above reads for no
  # entry point today and would read for one written that way. It has no second
  # statement, and an empty braced body has neither; both answer that rather
  # than raising a subscript error the gate would report as neither verdict.
  expect_identical(nth_statement(bare, 1L), reset)
  expect_null(nth_statement(bare, 2L))
  expect_null(nth_statement(function() {}, 1L))
})

test_that("the reset scan reads through covr's instrumentation", {
  # The shape covr rewrites a statement into, written out rather than produced
  # by calling covr: covr is supplied by the coverage workflow and is in no
  # dependency field, so a test that called it would put it in one. Measured on
  # covr 3.6.5.9001, which is what the coverage job installed when this gate
  # reported all six entry points against a package that resets in all six.
  # Only the counter's head is substituted in, for the reason its own reader
  # gives; the wrapper around it is the literal covr writes.
  reset <- quote(reset_sent_queries())
  forced <- quote(force(.data))
  counter <- coverage_counter()

  braced <- function() NULL
  body(braced) <- bquote({
    if (TRUE) {
      .(counter)("marginplyr/R/grouping-plan.R:1:1:1:1")
      force(.data)
    }
    if (TRUE) {
      .(counter)("marginplyr/R/grouping-plan.R:2:1:2:1")
      reset_sent_queries()
    }
  })

  unbraced <- function() NULL
  body(unbraced) <- bquote(if (TRUE) {
    .(counter)("marginplyr/R/grouping-plan.R:1:1:1:1")
    reset_sent_queries()
  })

  expect_identical(nth_statement(braced, 1L), forced)
  expect_identical(nth_statement(braced, 2L), reset)
  expect_identical(nth_statement(unbraced, 1L), reset)
  # The instrumented body still answers the other reading, which walks rather
  # than counts positions and is what the wrapper leaves alone.
  expect_true(empties_the_record(braced))

  # An `if (TRUE)` a caller wrote is not a wrapper, so the unwrapping may not
  # take it apart: it has no counter in it, and taking it apart would report a
  # first statement that is not the one the entry point opens with.
  authored <- function() NULL
  body(authored) <- quote({
    if (TRUE) {
      validate()
      reset_sent_queries()
    }
  })
  expect_false(identical(nth_statement(authored, 1L), reset))
})
