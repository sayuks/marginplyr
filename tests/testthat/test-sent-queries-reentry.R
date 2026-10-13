test_that("a resumed SQL call keeps only its own Sent queries", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  outer <- dplyr::copy_to(con, data.frame(g = c("a", "b"), v = c(1, 3)),
                          "outer_source")
  inner <- dplyr::copy_to(con, data.frame(k = "x", v = 101), "inner_source")
  expected <- summarize_with_margins(
    outer, total = sum(v, na.rm = TRUE), .grouping = rollup(g)
  )
  expected_record <- last_sent_queries()
  factory <- function() {
    nested <- summarize_with_margins(
      inner, other = sum(v, na.rm = TRUE), .grouping = rollup(k)
    )
    expect_identical(dplyr::collect(nested)$other, c(101, 101))
    rollup(g)
  }
  actual <- summarize_with_margins(
    outer, total = sum(v, na.rm = TRUE), .grouping = factory()
  )
  expect_identical(last_sent_queries(), expected_record)
  expect_identical(dplyr::collect(actual), dplyr::collect(expected))
})

test_that("local summary reentry cannot manufacture a SQL result", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  input <- data.frame(g = c("a", "b"), v = c(1, 3))
  before <- input
  inner <- dplyr::copy_to(con, data.frame(k = "x", v = 101), "inner_source")
  for (inspection in c(FALSE, TRUE)) {
    aggregate <- function(x) {
      if (inspection) {
        inspect_grouping(inner, .grouping = rollup(k))
      } else {
        summarize_with_margins(
          inner, other = sum(v, na.rm = TRUE), .grouping = rollup(k)
        )
      }
      expect_identical(nrow(last_sent_queries()), 0L)
      sum(x)
    }
    result <- summarize_with_margins(
      input, total = aggregate(v), .grouping = rollup(g)
    )
    expect_identical(result$total, c(1, 3, 4))
    expect_identical(nrow(last_sent_queries()), 0L)
    expect_identical(input, before)
  }
})

test_that("every tracked entry restores its own audit setting", {
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  input <- data.frame(g = c("a", "b"), v = c(1, 3))
  verbs <- list(summarize_with_margins, summarise_with_margins,
                expand_with_margins, nest_with_margins,
                nest_by_with_margins, inspect_grouping)
  for (verb in verbs) {
    for (audited in c(FALSE, TRUE)) {
      options(marginplyr.audit_sql = audited)
      expected <- verb(input, .grouping = rollup(g))
      factory <- function() {
        options(marginplyr.audit_sql = !audited)
        on.exit(options(marginplyr.audit_sql = audited), add = TRUE)
        inner_factory <- function() {
          if (audited) {
            expect_error(last_sent_queries(), "marginplyr.audit_sql")
          } else {
            expect_identical(nrow(last_sent_queries()), 0L)
          }
          rollup(g)
        }
        inspect_grouping(input, .grouping = inner_factory())
        if (audited) {
          expect_identical(nrow(last_sent_queries()), 0L)
        } else {
          expect_error(last_sent_queries(), "marginplyr.audit_sql")
        }
        rollup(g)
      }
      actual <- verb(input, .grouping = factory())
      expect_identical(actual, expected)
      expect_identical(getOption("marginplyr.audit_sql"), audited)
      if (audited) {
        expect_identical(nrow(last_sent_queries()), 0L)
      } else {
        expect_error(last_sent_queries(), "marginplyr.audit_sql")
      }
    }
  }
})

test_that("nested SQL inspection preserves both calls' partial records", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  outer <- dplyr::copy_to(con, data.frame(g = c("a", "b"), v = c(1, 3)),
                          "outer_source")
  inner <- dplyr::copy_to(con, data.frame(k = "x", v = 101), "inner_source")
  inspect_grouping(inner, .grouping = rollup(k))
  inner_record <- last_sent_queries()
  inspect_grouping(outer, .grouping = rollup(g))
  outer_record <- last_sent_queries()
  expect_identical(nrow(outer_record), 1L)
  expect_false(identical(inner_record, outer_record))
  observed <- new.env(parent = emptyenv())
  observed$inner <- FALSE
  observed$outer <- FALSE
  inner_select <- function() {
    if (nrow(last_sent_queries()) > 0L) {
      expect_identical(last_sent_queries(), inner_record)
      inspect_grouping(data.frame(z = 1))
      expect_identical(last_sent_queries(), inner_record)
      observed$inner <- TRUE
    }
    "k"
  }
  outer_select <- function() {
    before <- last_sent_queries()
    inspect_grouping(inner, .grouping = rollup(all_of(inner_select())))
    expect_identical(last_sent_queries(), before)
    if (nrow(before) > 0L) {
      expect_identical(before, outer_record)
      observed$outer <- TRUE
    }
    "g"
  }
  actual <- summarize_with_margins(
    outer, total = sum(v, na.rm = TRUE),
    .grouping = rollup(all_of(outer_select())), .sort = "last"
  )
  expect_true(observed$inner)
  expect_true(observed$outer)
  record <- last_sent_queries()
  expect_identical(record[record$purpose != "result", ], outer_record)
  expect_identical(dplyr::collect(actual)$total, c(1, 3, 4))
})

# External causes survive any contextual wrappers added by public verbs.
reentry_has_cause <- function(cnd, cause) {
  while (!is.null(cnd)) {
    if (identical(cnd, cause)) {
      return(TRUE)
    }
    cnd <- cnd$parent
  }
  FALSE
}

test_that("failure restores the outer partial record and releases ownership", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  outer <- dplyr::copy_to(con, data.frame(g = c("a", "b"), v = c(1, 3)),
                          "outer_source")
  inner <- dplyr::copy_to(con, data.frame(k = "x", v = 101), "inner_source")
  inspect_grouping(inner, .grouping = rollup(k))
  inner_record <- last_sent_queries()
  inspect_grouping(outer, .grouping = rollup(g))
  outer_record <- last_sent_queries()
  leaf <- errorCondition("inner failure", class = "reentry_cause")
  outer_leaf <- errorCondition("outer failure", class = "reentry_outer_cause")
  for (mode in c("caught", "propagated", "outer")) {
    observed <- new.env(parent = emptyenv())
    observed$failure <- FALSE
    inner_select <- function() {
      if (mode != "outer" && nrow(last_sent_queries()) > 0L) {
        stop(leaf)
      }
      "k"
    }
    outer_select <- function() {
      if (nrow(last_sent_queries()) == 0L) {
        return("g")
      }
      run_inner <- function() {
        withCallingHandlers(
          inspect_grouping(inner, .grouping = rollup(all_of(inner_select()))),
          error = function(cnd) {
            expect_identical(last_sent_queries(), inner_record)
            observed$failure <- TRUE
          }
        )
      }
      if (mode == "caught") {
        caught <- tryCatch(run_inner(), error = identity)
        expect_true(reentry_has_cause(caught, leaf))
      } else {
        run_inner()
      }
      expect_identical(last_sent_queries(), outer_record)
      if (mode == "outer") {
        stop(outer_leaf)
      }
      "g"
    }
    result <- tryCatch(
      summarize_with_margins(
        outer, total = sum(v, na.rm = TRUE),
        .grouping = rollup(all_of(outer_select())), .sort = "last"
      ),
      error = identity
    )
    record <- last_sent_queries()
    if (mode == "caught") {
      expect_identical(record[record$purpose != "result", ], outer_record)
      expect_identical(dplyr::collect(result)$total, c(1, 3, 4))
    } else {
      expect_true(reentry_has_cause(
        result, if (mode == "outer") outer_leaf else leaf
      ))
      expect_identical(record, outer_record)
    }
    expect_identical(observed$failure, mode != "outer")
    inspect_grouping(data.frame(z = 1))
    expect_identical(nrow(last_sent_queries()), 0L)
  }
})

test_that("warning-handler reentry preserves the outer result and record", {
  old <- options(marginplyr.audit_sql = TRUE, warn = 2)
  on.exit(options(old), add = TRUE)
  input <- data.frame(g = c("a", "b"), v = c(1, 3))
  inner <- dbplyr::tbl_lazy(data.frame(k = "x", v = 101))
  observed <- new.env(parent = emptyenv())
  observed$count <- 0L
  aggregate <- function(x) {
    warning("outer warning", call. = FALSE)
    sum(x)
  }
  result <- withCallingHandlers(
    summarize_with_margins(input, total = aggregate(v), .grouping = rollup(g)),
    warning = function(cnd) {
      observed$count <- observed$count + 1L
      expect_match(conditionMessage(cnd), "outer warning", fixed = TRUE)
      before <- last_sent_queries()
      summarize_with_margins(
        inner, other = sum(v, na.rm = TRUE), .grouping = rollup(k)
      )
      expect_identical(last_sent_queries(), before)
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(observed$count, 1L)
  expect_identical(result$total, c(1, 3, 4))
  expect_identical(nrow(last_sent_queries()), 0L)
  expect_identical(getOption("warn"), 2L)
})
