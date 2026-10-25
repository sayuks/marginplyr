test_that("detailed views preserve default SQL and typed empty schemas", {
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  summarize_with_margins(dbplyr::tbl_lazy(data.frame(v = 1)), total = sum(v))
  plain <- last_sent_queries()
  detailed <- last_sent_queries(details = TRUE)
  expect_identical(names(detailed), c(
    "purpose", "sql", "source", "status", "conditions"
  ))
  expect_identical(detailed[1:2], plain)
  expect_identical(detailed$source, "render")
  expect_identical(detailed$status, "prepared")
  empty_conditions <- dplyr::tibble(
    classes = list(), message = character(), phase = character(),
    handling = character()
  )
  expect_identical(detailed$conditions, list(empty_conditions))
  expect_identical(last_sent_queries(FALSE, TRUE), detailed[FALSE, ])
  invalid_values <- list(
    NULL, NA, logical(), c(TRUE, FALSE), 1, "TRUE", list(TRUE)
  )
  for (invalid in invalid_values) {
    expect_error(last_sent_queries(details = invalid), "details",
                 class = "marginplyr_error")
    expect_identical(last_sent_queries(details = TRUE), detailed)
  }
  inspect_grouping(data.frame(v = 1))
  expect_identical(last_sent_queries(TRUE, TRUE), detailed[FALSE, ])
  expect_identical(last_sent_queries(FALSE, TRUE), detailed[FALSE, ])
})

test_that("label-count completion is independent of public validation", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  remote <- dplyr::copy_to(con, data.frame(g = "a", v = 1), "details_source")
  for (collision in c(FALSE, TRUE)) {
    source <- if (collision) dplyr::mutate(remote, g = "Total") else remote
    run <- function() {
      summarize_with_margins(
        source, total = sum(v), .grouping = rollup(g),
        .check_margin_label = TRUE
      )
    }
    if (collision) {
      expect_error(run(), class = "marginplyr_error")
    } else {
      expect_s3_class(run(), "tbl_sql")
    }
    detailed <- last_sent_queries(FALSE, TRUE)
    expect_identical(detailed$status, "returned")
    expect_identical(detailed$source, "render")
    expect_identical(nrow(detailed$conditions[[1L]]), 0L)
  }
})

test_that("metadata and handled dialect failures have separate outcomes", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  saved <- share_dialect_snapshot()
  on.exit(restore_share_dialect_verdicts(saved), add = TRUE)
  empty_share_dialect_verdicts()
  remote <- dplyr::copy_to(con, data.frame(g = "a", v = 1), "detail_metadata")
  summarize_with_margins(
    remote, total = sum(v), share = share_of_total(total), .grouping = rollup(g)
  )
  rows <- last_sent_queries(FALSE, TRUE)
  expect_identical(rows$purpose, c(
    "selection_proxy", "share_dialect", "share_dialect_control"
  ))
  expect_identical(rows$status, c("returned", "errored", "returned"))
  expect_identical(rows$source, rep("render", 3L))
  expect_identical(nrow(rows$conditions[[1L]]), 0L)
  probe <- rows$conditions[[2L]]
  expect_true("error" %in% probe$classes[[1L]])
  expect_match(probe$message, "sum", fixed = TRUE)
  expect_identical(probe$phase, "invocation")
  expect_identical(probe$handling, "handled")
  expect_identical(nrow(rows$conditions[[3L]]), 0L)
  inspect_grouping(remote, .grouping = rollup(g))
  expect_identical(last_sent_queries(FALSE, TRUE), rows[1L, ])
})

# Controlled driver-boundary conditions cover outcomes real databases need not
# produce. The public verb/accessor remain the observation seam (#791).
with_detail_source <- function(code) {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  remote <- dplyr::copy_to(con, data.frame(g = "a", v = 1), "condition_source")
  code(remote)
}

detail_label_check <- function(remote) {
  summarize_with_margins(
    remote, total = sum(.data$v), .grouping = rollup(dplyr::all_of("g")),
    .check_margin_label = TRUE
  )
}

test_that("owned warnings preserve delivery, order, and warning-to-error", {
  with_detail_source(function(remote) {
    testthat::local_mocked_bindings(collect = function(x, ...) {
      warning("first driver warning", call. = FALSE)
      warning("second driver warning", call. = FALSE)
      data.frame(g = 0)
    }, .package = "dplyr")
    delivered <- character()
    withCallingHandlers(detail_label_check(remote), warning = function(cnd) {
      delivered <<- c(delivered, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    })
    expect_identical(delivered, c(
      "first driver warning", "second driver warning"
    ))
    rows <- last_sent_queries(FALSE, TRUE)
    expect_identical(rows$status, "returned")
    summaries <- rows$conditions[[1L]]
    expect_identical(summaries$message, delivered)
    expect_identical(summaries$phase, rep("invocation", 2L))
    expect_identical(summaries$handling, rep("propagated", 2L))
    old <- options(warn = 2)
    on.exit(options(old), add = TRUE)
    expect_error(detail_label_check(remote), "converted from warning")
    rows <- last_sent_queries(FALSE, TRUE)
    expect_identical(rows$status, "errored")
    expect_identical(rows$conditions[[1L]]$phase, c("invocation", "invocation"))
    expect_true("warning" %in% rows$conditions[[1L]]$classes[[1L]])
    expect_true("error" %in% rows$conditions[[1L]]$classes[[2L]])
    expect_identical(getOption("warn"), 2L)
  })
})

test_that("one compound signal has one summary and preserves its outcome", {
  with_detail_source(function(remote) {
    # A controlled collection boundary supplies overlapping categories that a
    # real driver need not emit; the public accessor observes each signal.
    leaf <- errorCondition("compound driver condition", class = "warning")
    for (terminating in c(FALSE, TRUE)) {
      testthat::local_mocked_bindings(collect = function(...) {
        if (terminating) {
          stop(leaf)
        }
        signalCondition(leaf)
        data.frame(g = 0)
      }, .package = "dplyr")
      run <- function(audited) {
        options(marginplyr.audit_sql = audited)
        delivered <- list()
        invoke <- function() {
          withCallingHandlers(
            detail_label_check(remote),
            condition = function(cnd) {
              delivered[[length(delivered) + 1L]] <<- cnd
            }
          )
        }
        outcome <- tryCatch(invoke(), error = identity)
        list(delivered = delivered, outcome = outcome)
      }
      off <- run(FALSE)
      on <- run(TRUE)
      expect_identical(on, off)
      expect_identical(on$delivered, list(leaf))
      expect_identical(on$outcome, leaf)
      rows <- last_sent_queries(FALSE, TRUE)
      expect_identical(rows$status, "errored")
      expect_identical(rows$conditions[[1L]], dplyr::tibble(
        classes = list(c("warning", "error", "condition")),
        message = "compound driver condition", phase = "invocation",
        handling = "propagated"
      ))
    }
  })
})

test_that("errors and interrupts propagate the original conditions", {
  with_detail_source(function(remote) {
    for (kind in c("error", "interrupt")) {
      leaf <- if (kind == "error") {
        errorCondition("driver failed", class = "detail_driver_error")
      } else {
        structure(
          list(message = "driver interrupted", call = NULL),
          class = c("detail_driver_interrupt", "interrupt", "condition")
        )
      }
      testthat::local_mocked_bindings(collect = function(...) stop(leaf),
                                      .package = "dplyr")
      caught <- tryCatch(
        detail_label_check(remote), error = identity, interrupt = identity
      )
      expect_identical(caught, leaf)
      rows <- last_sent_queries(FALSE, TRUE)
      expect_identical(
        rows$status, if (kind == "error") "errored" else "interrupted"
      )
      expect_identical(rows$conditions[[1L]], dplyr::tibble(
        classes = list(class(leaf)), message = conditionMessage(leaf),
        phase = "invocation", handling = "propagated"
      ))
      rows$conditions[[1L]]$message <- "caller copy"
      rows$conditions[[1L]]$classes[[1L]] <- "caller_class"
      retained <- last_sent_queries(FALSE, TRUE)$conditions[[1L]]
      expect_identical(retained$message, conditionMessage(leaf))
      expect_identical(retained$classes, list(class(leaf)))
    }
  })
})

test_that("render failure and invocation completion are independent", {
  with_detail_source(function(remote) {
    testthat::local_mocked_bindings(sql_render = function(...) {
      warning("diagnostic warning", call. = FALSE)
      stop("diagnostic render failed")
    }, .package = "dbplyr")
    testthat::local_mocked_bindings(collect = function(...) data.frame(g = 0),
                                    .package = "dplyr")
    old <- options(warn = 2)
    on.exit(options(old), add = TRUE)
    expect_s3_class(detail_label_check(remote), "tbl_sql")
    rows <- last_sent_queries(TRUE, TRUE)
    expect_identical(rows$sql, rep(NA_character_, 2L))
    expect_identical(rows$source, rep("render", 2L))
    expect_identical(rows$status, c("returned", "prepared"))
    expected <- c("diagnostic warning", "diagnostic render failed")
    expect_identical(rows$conditions[[1L]]$message, expected)
    expect_identical(rows$conditions[[1L]]$phase, c("render", "render"))
    expect_identical(rows$conditions[[1L]]$handling, c("suppressed", "handled"))
    expect_identical(rows$conditions[[2L]], rows$conditions[[1L]])
    testthat::local_mocked_bindings(
      collect = function(...) stop("invocation failed"), .package = "dplyr"
    )
    expect_error(detail_label_check(remote), "invocation failed")
    row <- last_sent_queries(FALSE, TRUE)
    expect_identical(row$status, "errored")
    expect_identical(
      row$conditions[[1L]]$message, c(expected, "invocation failed")
    )
    expect_identical(
      row$conditions[[1L]]$phase, c("render", "render", "invocation")
    )
    expect_identical(row$conditions[[1L]]$handling,
                     c("suppressed", "handled", "propagated"))
  })
})

test_that("active invocations and unmatched unwinding do not invent success", {
  with_detail_source(function(remote) {
    testthat::local_mocked_bindings(collect = function(...) {
      expect_identical(last_sent_queries(FALSE, TRUE)$status, "invoking")
      invokeRestart("leave_driver")
    }, .package = "dplyr")
    result <- withRestarts(
      detail_label_check(remote), leave_driver = function() "escaped"
    )
    expect_identical(result, "escaped")
    rows <- last_sent_queries(FALSE, TRUE)
    expect_identical(rows$status, "unknown")
    expect_identical(nrow(rows$conditions[[1L]]), 0L)
  })
})

test_that("pending updates retain their owner across failing nested calls", {
  with_detail_source(function(remote) {
    depth <- 0L
    testthat::local_mocked_bindings(collect = function(x, ...) {
      depth <<- depth + 1L
      on.exit({
        depth <<- depth - 1L
      }, add = TRUE)
      if (depth == 2L) {
        expect_identical(last_sent_queries(FALSE, TRUE)$status, "invoking")
        options(marginplyr.audit_sql = FALSE)
        stop("nested driver failure")
      }
      before <- last_sent_queries(FALSE, TRUE)
      expect_error(detail_label_check(remote), "nested driver failure")
      expect_identical(last_sent_queries(FALSE, TRUE), before)
      inspect_grouping(data.frame(v = 1))
      expect_identical(last_sent_queries(FALSE, TRUE), before)
      data.frame(g = 0)
    }, .package = "dplyr")
    detail_label_check(remote)
    rows <- last_sent_queries(TRUE, TRUE)
    expect_identical(rows$status, c("returned", "prepared"))
    expect_identical(vapply(rows$conditions, nrow, integer(1)), c(0L, 0L))
    expect_identical(getOption("marginplyr.audit_sql"), FALSE)
    inspect_grouping(data.frame(v = 1))
    expect_error(last_sent_queries(details = TRUE), "marginplyr.audit_sql",
                 class = "marginplyr_error")
  })
})

test_that("render interrupts keep a prepared row and propagate unchanged", {
  with_detail_source(function(remote) {
    leaf <- structure(list(message = "render interrupted", call = NULL),
                      class = c("interrupt", "condition"))
    testthat::local_mocked_bindings(sql_render = function(...) stop(leaf),
                                    .package = "dbplyr")
    caught <- tryCatch(detail_label_check(remote), interrupt = identity)
    expect_identical(caught, leaf)
    rows <- last_sent_queries(FALSE, TRUE)
    expect_identical(rows$status, "prepared")
    expect_identical(rows$sql, NA_character_)
    expect_identical(rows$conditions[[1L]], dplyr::tibble(
      classes = list(class(leaf)), message = "render interrupted",
      phase = "render", handling = "propagated"
    ))
  })
})

test_that("retrieval and independent SQL leave detailed rows intact", {
  with_detail_source(function(remote) {
    query <- detail_label_check(remote)
    rows <- last_sent_queries(details = TRUE)
    expect_identical(dplyr::collect(query)$total, c(1, 1))
    expect_identical(dplyr::pull(query, "total"), c(1, 1))
    computed <- dplyr::compute(query)
    expect_identical(dplyr::collect(computed)$total, c(1, 1))
    DBI::dbGetQuery(dbplyr::remote_con(remote), "SELECT 42 AS answer")
    expect_identical(last_sent_queries(details = TRUE), rows)
    expect_identical(last_sent_queries(), rows[c("purpose", "sql")])
  })
})
