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
