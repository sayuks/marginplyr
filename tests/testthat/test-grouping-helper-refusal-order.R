# An invalid Grouping helper call is decidable from the call and the resolved
# plan, so ADR 0005 has it refused before the Margin label collision query
# reads the input. Every input here holds the Margin label in `g`, which the
# check would report if it ran first; the refusal arriving instead is what
# shows it did not.

helper_refusal_data <- function() {
  data.frame(g = c("a", "Total"), v = 1:2)
}

# Each invalid call and the refusal it raises with the check off.
invalid_helper_calls <- function() {
  list(
    list(call = quote(grouping_bit()), message = "requires exactly one column"),
    list(call = quote(grouping_bit(g, g)),
         message = "requires exactly one column"),
    list(call = quote(grouping_bit(g + 1)),
         message = "only accepts bare grouping columns"),
    list(call = quote(grouping_bit(v)), message = "not part of `.by`"),
    list(call = quote(grouping_id(g, nope)), message = "not part of `.by`")
  )
}

expect_helper_refused_first <- function(data, sent_nothing = NULL) {
  for (case in invalid_helper_calls()) {
    expect_error(
      summarize_with_margins(
        data,
        s = !!case$call,
        .grouping = rollup("g"),
        .check_margin_label = TRUE
      ),
      case$message,
      fixed = TRUE,
      class = "marginplyr_error",
      info = deparse1(case$call)
    )
    if (!is.null(sent_nothing)) {
      sent_nothing(deparse1(case$call))
    }
  }
}

expect_no_collision_query <- function(info) {
  expect_false(
    "observed_label_collision" %in% last_sent_queries()$purpose,
    info = info
  )
}

test_that("a local input reports the helper refusal before a collision", {
  data <- helper_refusal_data()

  expect_helper_refused_first(data)
  expect_error(
    summarize_with_margins(
      data, s = grouping_bit(g), .grouping = rollup(g),
      .check_margin_label = TRUE
    ),
    "already present in grouping column",
    class = "marginplyr_error"
  )
})

test_that("an RSQLite input refuses an invalid helper before the query", {
  skip_if_suggest_absent("RSQLite", "DBI")
  withr::local_options(marginplyr.audit_sql = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  data <- dplyr::copy_to(con, helper_refusal_data(), "helper_refusal")

  expect_helper_refused_first(data, expect_no_collision_query)

  # The valid call still sends the check, and it still finds the label.
  expect_error(
    summarize_with_margins(
      data, s = grouping_bit(g), .grouping = rollup(g),
      .check_margin_label = TRUE
    ),
    "already present in grouping column",
    class = "marginplyr_error"
  )
  expect_true("observed_label_collision" %in% last_sent_queries()$purpose)

  # A table that no longer exists cannot be read: a refusal that waited for
  # the query would be replaced by the backend's failure.
  DBI::dbRemoveTable(con, "helper_refusal")
  expect_helper_refused_first(data)
})

test_that("a DuckDB input refuses an invalid helper before the query", {
  skip_if_suggest_absent("duckdb", "DBI")
  withr::local_options(marginplyr.audit_sql = TRUE)
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  data <- dplyr::copy_to(con, helper_refusal_data(), "helper_refusal")

  expect_helper_refused_first(data, expect_no_collision_query)
})

test_that("an Arrow dataset refuses an invalid helper before opening it", {
  skip_if_suggest_absent("arrow")
  dir <- withr::local_tempdir()
  arrow::write_parquet(helper_refusal_data(), file.path(dir, "p.parquet"))
  data <- arrow::open_dataset(dir)

  expect_helper_refused_first(data)

  # With the file gone, any read fails in Arrow rather than in marginplyr.
  unlink(file.path(dir, "p.parquet"))
  expect_helper_refused_first(data)
})

test_that("a dtplyr input reports the helper refusal before a collision", {
  skip_if_suggest_absent("dtplyr")

  expect_helper_refused_first(dtplyr::lazy_dt(helper_refusal_data()))
})

# A helper call wrong on its face -- its arity, a non-bare argument, a repeated
# column -- needs neither the plan nor the input, so ADR 0005 also places it
# ahead of typed metadata acquisition, which on DuckDB is two statements.
call_only_helper_calls <- function() {
  list(
    list(call = quote(grouping_bit(g, h)),
         message = "requires exactly one column"),
    list(call = quote(grouping_bit(g + 1)),
         message = "only accepts bare grouping columns"),
    list(call = quote(grouping_id(g, g)),
         message = "does not accept duplicate columns"),
    list(
      call = quote(dplyr::across(dplyr::everything(), ~ grouping_bit(g, h))),
      message = "requires exactly one column"
    )
  )
}

# The second `.grouping` is settled only by typed metadata, which a lazy input
# does not acquire before the helper refusal.
expect_helper_sends_nothing <- function(data) {
  counter <- duckdb_query_counter()
  on.exit(counter$untrace(), add = TRUE)
  groupings <- rlang::exprs(rollup("g"), rollup(dplyr::where(is.character)))
  for (case in call_only_helper_calls()) for (grouping in groupings) {
    info <- paste(deparse1(case$call), deparse1(grouping))
    counter$queries <- 0L
    error <- expect_error(
      summarize_with_margins(data, s = !!case$call, .grouping = !!grouping),
      case$message,
      fixed = TRUE,
      class = "marginplyr_error",
      info = info
    )
    expect_identical(
      rlang::call_name(conditionCall(error)), "summarize_with_margins",
      info = info
    )
    expect_identical(nrow(last_sent_queries(details = TRUE)), 0L, info = info)
    expect_identical(counter$queries, 0L, info = info)
  }
}

test_that("DuckDB refuses a helper wrong on its face before any statement", {
  skip_if_suggest_absent("duckdb", "DBI")
  withr::local_options(marginplyr.audit_sql = TRUE)
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "helper_call", data.frame(
    g = c("a", "a", "b"), h = c("x", "y", "x"), v = 1:3
  ))
  data <- dplyr::tbl(con, "helper_call")

  expect_helper_sends_nothing(data)

  # A dynamic `PIVOT` cannot give its columns safely, so a metadata refusal
  # advising collection would otherwise answer instead.
  pivot <- dplyr::tbl(con, dbplyr::sql(paste(
    "PIVOT (SELECT g, h, v FROM helper_call) ON h USING sum(v)"
  )), vars = c("g", "x", "y"))
  expect_helper_sends_nothing(pivot)

  # Neither can a relation that no longer exists.
  DBI::dbRemoveTable(con, "helper_call")
  expect_helper_sends_nothing(data)

  # A `.grouping` grammar error keeps its precedence over the helper's.
  expect_error(
    summarize_with_margins(data, s = grouping_bit(g, h), .grouping = rollup()),
    "requires at least one dimension",
    class = "marginplyr_error"
  )
})

# A `.by` refusal precedes a helper refusal when names decide it before typed
# metadata, as an overlap with `.grouping` is. One decided only afterwards, as
# a computed expression is, precedes it on a data frame, which acquires no
# metadata; a SQL input refuses the helper first, before any statement
# (ADR 0005).
test_that("a `.by` error precedes a helper refusal unless metadata decides", {
  by_errors <- rlang::exprs(
    "cannot appear in both `.by` and `.grouping`" = g,
    "`.by` selects existing columns, not computed expressions" = g + 1
  )
  expect_by_refused_first <- function(data, by_errors) {
    for (message in names(by_errors)) {
      expect_error(
        summarize_with_margins(
          data, s = grouping_bit(g, h), .by = !!by_errors[[message]],
          .grouping = rollup(g)
        ),
        message,
        fixed = TRUE,
        class = "marginplyr_error",
        info = message
      )
    }
  }
  local <- data.frame(g = c("a", "a", "b"), h = c("x", "y", "x"), v = 1:3)
  expect_by_refused_first(local, by_errors)

  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "helper_by", local)
  lazy <- dplyr::tbl(con, "helper_by")
  expect_by_refused_first(lazy, by_errors[1L])
  expect_error(
    summarize_with_margins(
      lazy, s = grouping_bit(g, h), .by = g + 1, .grouping = rollup(g)
    ),
    "requires exactly one column",
    fixed = TRUE,
    class = "marginplyr_error"
  )
})

# dtplyr reads its metadata in R, so it keeps the data frame's order.
test_that("a computed `.by` precedes a helper refusal on dtplyr", {
  skip_if_suggest_absent("dtplyr")
  local <- data.frame(g = c("a", "a", "b"), h = c("x", "y", "x"), v = 1:3)
  expect_error(
    summarize_with_margins(
      dtplyr::lazy_dt(local), s = grouping_bit(g, h), .by = g + 1,
      .grouping = rollup(g)
    ),
    "`.by` selects existing columns, not computed expressions",
    fixed = TRUE,
    class = "marginplyr_error"
  )
})
