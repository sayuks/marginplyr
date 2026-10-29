# Controlled collection boundaries supply recoverable signals; the public
# SQLite Margin verb and accessor observe their outcomes (#822).
test_that("a recovered interrupt does not determine a later unmatched exit", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  remote <- dplyr::copy_to(con, data.frame(g = "a", v = 1), "resumed_source")
  leaf <- structure(list(message = "recovered interrupt", call = NULL),
                    class = c("resumed_interrupt", "interrupt", "condition"))
  testthat::local_mocked_bindings(collect = function(...) {
    withRestarts(signalCondition(leaf), resume_driver = function() NULL)
    invokeRestart("leave_driver")
  }, .package = "dplyr")
  delivered <- list()
  result <- withRestarts(withCallingHandlers(
    summarize_with_margins(
      remote, total = sum(.data$v), .grouping = rollup(dplyr::all_of("g")),
      .check_margin_label = TRUE
    ),
    interrupt = function(cnd) {
      delivered[[length(delivered) + 1L]] <<- cnd
      invokeRestart("resume_driver")
    }
  ), leave_driver = function() "escaped")
  expect_identical(result, "escaped")
  expect_identical(delivered, list(leaf))
  rows <- last_sent_queries(FALSE, TRUE)
  expect_identical(rows$status, "unknown")
  expect_identical(rows$conditions[[1L]], dplyr::tibble(
    classes = list(class(leaf)), message = leaf$message,
    phase = "invocation", handling = "propagated"
  ))
})

test_that("recovery preserves later outcomes, caller restarts, and ownership", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  old <- options(marginplyr.audit_sql = FALSE)
  on.exit(options(old), add = TRUE)
  remote <- dplyr::copy_to(con, data.frame(g = "a", v = 1), "recovery_source")
  first <- structure(list(message = "first interrupt", call = NULL),
                     class = c("first_interrupt", "interrupt", "condition"))
  failure <- errorCondition("later error", class = "later_error")
  cancellation <- structure(
    list(message = "later interrupt", call = NULL),
    class = c("later_interrupt", "interrupt", "condition")
  )
  collect <- dplyr::collect
  check <- function() {
    summarize_with_margins(
      remote, total = sum(.data$v), .grouping = rollup(dplyr::all_of("g")),
      .check_margin_label = TRUE
    )
  }
  for (native in c(FALSE, TRUE)) {
    for (ending in c("return", "escape", "error", "interrupt")) {
      run <- function(audited) {
        options(marginplyr.audit_sql = audited)
        delivered <- list()
        original <- NULL
        cleanup <- character()
        resumed <- 0L
        calls <- 0L
        depth <- 0L
        testthat::local_mocked_bindings(collect = function(x, ...) {
          calls <<- calls + 1L
          depth <<- depth + 1L
          on.exit({
            cleanup <<- c(cleanup, if (depth == 1L) "outer" else "inner")
            depth <<- depth - 1L
          }, add = TRUE)
          if (depth == 2L) return(collect(x, ...))
          # Driver-local names must not change the observer's bookkeeping.
          for (name in c("on.exit", "returnValue", "identical")) {
            assign(name, function(...) {
              stop("backend-local function was called")
            }, envir = environment())
          }
          withRestarts({
            withCallingHandlers({
              if (native) rlang::interrupt() else signalCondition(first)
            }, interrupt = function(cnd) {
              original <<- cnd
            })
          }, resume_driver = function() {
            resumed <<- resumed + 1L
          })
          switch(ending,
            return = collect(x, ...),
            escape = invokeRestart("leave_driver", "escaped"),
            error = stop(failure),
            interrupt = stop(cancellation)
          )
        }, .package = "dplyr")
        outcome <- tryCatch(withRestarts(withCallingHandlers(
          check(),
          interrupt = function(cnd) {
            delivered[[length(delivered) + 1L]] <<- cnd
            if (length(delivered) == 1L) {
              before <- if (audited) last_sent_queries(FALSE, TRUE)
              check()
              if (audited) {
                expect_identical(last_sent_queries(FALSE, TRUE), before)
                expect_identical(before$status, "invoking")
              }
              invokeRestart("resume_driver")
            }
          }
        ), leave_driver = identity), error = identity, interrupt = identity)
        expect_identical(resumed, 1L)
        expect_identical(calls, 2L)
        expect_identical(cleanup, c("inner", "outer"))
        expect_identical(length(delivered),
                         if (ending == "interrupt") 2L else 1L)
        expect_identical(delivered[[1L]], original)
        if (!native) expect_identical(original, first)
        if (ending == "interrupt") {
          expect_identical(delivered[[2L]], cancellation)
          expect_identical(outcome, cancellation)
        } else if (ending == "error") {
          expect_identical(outcome, failure)
        } else if (ending == "escape") {
          expect_identical(outcome, "escaped")
        } else {
          expect_s3_class(outcome, "tbl_sql")
          outcome <- as.character(dbplyr::sql_render(outcome))
        }
        summaries <- lapply(delivered, function(cnd) {
          list(classes = class(cnd), message = conditionMessage(cnd))
        })
        if (audited) {
          rows <- last_sent_queries(FALSE, TRUE)
          expect_identical(rows$status, switch(ending,
            return = "returned", escape = "unknown", error = "errored",
            interrupt = "interrupted"
          ))
          conditions <- c(delivered, if (ending == "error") list(failure))
          expect_identical(rows$conditions[[1L]], dplyr::tibble(
            classes = lapply(conditions, class),
            message = c(
              if (native) NA_character_ else first$message,
              if (ending == "error") failure$message,
              if (ending == "interrupt") cancellation$message
            ),
            phase = rep("invocation", length(conditions)),
            handling = rep("propagated", length(conditions))
          ))
          held <- last_sent_queries(details = TRUE)
          expect_identical(last_sent_queries(), held[c("purpose", "sql")])
          expect_identical(last_sent_queries(FALSE), rows[c("purpose", "sql")])
          copy <- held
          copy$conditions[[1L]]$message <- "caller copy"
          expect_identical(last_sent_queries(details = TRUE), held)
          expect_identical(calls, 2L)
          expect_identical(cleanup, c("inner", "outer"))
          expect_identical(length(delivered), length(summaries))
        }
        list(outcome = outcome, summaries = summaries, cleanup = cleanup)
      }
      off <- run(FALSE)
      on <- run(TRUE)
      expect_identical(on, off)
    }
  }
})
