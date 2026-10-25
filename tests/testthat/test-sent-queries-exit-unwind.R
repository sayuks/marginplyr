test_that("cleanup interrupts release every tracked entry point", {
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  input <- data.frame(g = "a", v = 1)
  remote <- dbplyr::tbl_lazy(input)
  leaf <- structure(list(message = "grouping cleanup interrupted", call = NULL),
                    class = c("cleanup_interrupt", "interrupt", "condition"))
  factory <- function() {
    on.exit(stop(leaf), add = TRUE)
    summarize_with_margins(remote, child = sum(v, na.rm = TRUE))
    expect_identical(nrow(last_sent_queries()), 0L)
    rollup(g)
  }
  verbs <- list(summarize_with_margins, summarise_with_margins,
                expand_with_margins, nest_with_margins,
                nest_by_with_margins, inspect_grouping)
  for (verb in verbs) {
    options(marginplyr.audit_sql = TRUE)
    caught <- tryCatch(verb(input, .grouping = factory()), interrupt = identity)
    expect_identical(caught, leaf)
    expect_identical(nrow(last_sent_queries()), 0L)
    for (audited in c(FALSE, TRUE, TRUE)) {
      options(marginplyr.audit_sql = audited)
      fresh <- summarize_with_margins(remote, fresh = sum(v, na.rm = TRUE))
      options(marginplyr.audit_sql = !audited)
      if (audited) {
        rows <- last_sent_queries(details = TRUE)
        expect_identical(rows$purpose, "result")
        expect_identical(rows$status, "prepared")
        expect_identical(rows$sql, as.character(dbplyr::sql_render(fresh)))
      } else {
        expect_error(last_sent_queries(), "marginplyr.audit_sql",
                     class = "marginplyr_error")
      }
    }
  }
})

test_that("caller promises named like the recording slot remain unforced", {
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  factory <- function(sent_query_scope = stop("caller promise was forced")) {
    inspect_grouping(data.frame(g = 1), .grouping = rollup(g))
    rollup(g)
  }
  result <- summarize_with_margins(
    dbplyr::lazy_frame(g = "a", v = 1), total = sum(v, na.rm = TRUE),
    .grouping = factory()
  )
  expect_identical(last_sent_queries()$sql,
                   as.character(dbplyr::sql_render(result)))
})

test_that("interrupted SQL reentry preserves conditions and audit settings", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  outer <- dplyr::copy_to(con, data.frame(g = "a", v = 1), "interrupt_outer")
  inner <- dplyr::copy_to(con, data.frame(g = "b", v = 2), "interrupt_inner")
  collect <- dplyr::collect
  leaf <- structure(list(message = "owned operation interrupted", call = NULL),
                    class = c("owned_interrupt", "interrupt", "condition"))
  check <- function(source) {
    summarize_with_margins(
      source, total = sum(v, na.rm = TRUE), .grouping = rollup(g),
      .check_margin_label = TRUE
    )
  }
  # Interrupt backend cleanup after its query returned, before the owned
  # observer can report success. Native SIGINT is checked separately by
  # tools/check-sent-query-interrupt.R without replacing package functions.
  run <- function(parent_audited, child_audited, catch_child) {
    options(marginplyr.audit_sql = parent_audited)
    in_child <- FALSE
    before <- NULL
    child_seen <- NULL
    resumed <- FALSE
    testthat::local_mocked_bindings(collect = function(x, ...) {
      if (in_child) {
        on.exit(stop(leaf), add = TRUE)
        return(collect(x, ...))
      }
      if (parent_audited) {
        before <<- last_sent_queries(details = TRUE)
      }
      options(marginplyr.audit_sql = child_audited)
      in_child <<- TRUE
      call_child <- function() {
        withCallingHandlers(check(inner), interrupt = function(cnd) {
          expect_identical(cnd, leaf)
          if (child_audited) {
            child_seen <<- last_sent_queries(details = TRUE)
          } else {
            expect_error(last_sent_queries(), "marginplyr.audit_sql",
                         class = "marginplyr_error")
          }
        })
      }
      if (catch_child) {
        expect_identical(tryCatch(call_child(), interrupt = identity), leaf)
      } else {
        call_child()
      }
      in_child <<- FALSE
      resumed <<- TRUE
      if (parent_audited) {
        expect_identical(last_sent_queries(details = TRUE), before)
      } else {
        expect_error(last_sent_queries(), "marginplyr.audit_sql",
                     class = "marginplyr_error")
      }
      collect(x, ...)
    }, .package = "dplyr")
    result <- tryCatch(check(outer), interrupt = identity)
    expect_identical(resumed, catch_child)
    expect_identical(getOption("marginplyr.audit_sql"), child_audited)
    if (child_audited) {
      expect_identical(child_seen$status, "invoking")
      expect_match(child_seen$sql, "interrupt_inner", fixed = TRUE)
      expect_identical(child_seen$conditions[[1L]], dplyr::tibble(
        classes = list(class(leaf)), message = leaf$message,
        phase = "invocation", handling = "propagated"
      ))
    }
    if (catch_child) {
      expect_s3_class(result, "tbl_sql")
    } else {
      expect_identical(result, leaf)
    }
    if (parent_audited) {
      rows <- last_sent_queries(details = TRUE)
      expect_identical(rows$purpose, if (catch_child) {
        c("observed_label_collision", "result")
      } else {
        "observed_label_collision"
      })
      expect_identical(rows$status, if (catch_child) {
        c("returned", "prepared")
      } else {
        "interrupted"
      })
      expect_identical(rows$sql[[1L]], before$sql[[1L]])
      expect_no_match(paste(rows$sql, collapse = "\n"), "interrupt_inner",
                      fixed = TRUE)
      expect_identical(rows$conditions[[1L]], if (catch_child) {
        before$conditions[[1L]]
      } else {
        dplyr::tibble(classes = list(class(leaf)), message = leaf$message,
                      phase = "invocation", handling = "propagated")
      })
    } else {
      expect_error(last_sent_queries(), "marginplyr.audit_sql",
                   class = "marginplyr_error")
    }
    options(marginplyr.audit_sql = TRUE)
    for (name in c("independent_one", "independent_two")) {
      fresh <- summarize_with_margins(
        dbplyr::lazy_frame(v = 1, .name = name), fresh = sum(v, na.rm = TRUE)
      )
      expect_identical(last_sent_queries()$sql,
                       as.character(dbplyr::sql_render(fresh)))
    }
  }
  for (parent_audited in c(FALSE, TRUE)) {
    for (child_audited in c(FALSE, TRUE)) {
      for (catch_child in c(FALSE, TRUE)) {
        run(parent_audited, child_audited, catch_child)
      }
    }
  }
})
