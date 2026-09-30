# Caller handlers retain R's availability and restart boundaries (ADR 0035).
test_that("external warning-handler failure can replace a pending condition", {
  for (warn in c(1L, 2L)) {
    for (kind in c("error", "interrupt", "prior_error_interrupt",
                   "prior_error_handler_error")) {
      old <- options(warn = warn)
      on.exit(options(old), add = TRUE)
      input <- data.frame(g = c("a", "b"), h = c("u", "v"), v = c(2, 5))
      original <- input
      first <- structure(list(message = "first cancellation"),
                         class = c("interrupt", "condition"))
      second <- structure(list(message = "handler cancellation"),
                          class = c("interrupt", "condition"))
      failure <- errorCondition("handler failure",
                                parent = simpleError("handler parent"),
                                payload = new.env(parent = emptyenv()))
      execution <- errorCondition("execution failure",
                                  parent = simpleError("execution parent"))
      failure_bytes <- serialize(failure, NULL)
      execution_bytes <- serialize(execution, NULL)
      effects <- numeric()
      reached <- FALSE
      observed <- list()
      branch <- function(v, omitted) {
        if (length(v) == 2L) {
          reached <<- TRUE
          if (startsWith(kind, "prior_error")) stop(execution)
          signalCondition(first)
          rlang::interrupt()
        }
        effects <<- c(effects, sum(v))
        warning(if (omitted == 0L) "first warning" else "second warning")
        sum(v)
      }
      outcome <- tryCatch(withCallingHandlers(
        summarize_with_margins(
          input, z = branch(.data$v, grouping_bit(g)),
          .grouping = grouping_sets(grouping_set("g"), grouping_set("g"),
                                    grouping_set("h"), grouping_set()),
          .duplicates = "keep"
        ),
        warning = function(cnd) {
          observed[[length(observed) + 1L]] <<- cnd
          if (kind %in% c("error", "prior_error_handler_error")) stop(failure)
          signalCondition(second)
          rlang::interrupt()
        }
      ), error = identity, interrupt = identity)
      expect_true(reached)
      expect_identical(effects, rep(c(2, 5), 3L))
      expect_length(observed, 1L)
      expect_match(conditionMessage(observed[[1L]]), "first warning")
      expect_match(conditionMessage(observed[[1L]]), "1 further grouping set")
      expected <- if (kind %in% c("error", "prior_error_handler_error")) {
        failure
      } else {
        second
      }
      expect_identical(outcome, expected)
      expect_null(outcome$interrupt)
      expect_identical(serialize(failure, NULL), failure_bytes)
      expect_identical(serialize(execution, NULL), execution_bytes)
      expect_identical(input, original)
      expect_identical(getOption("warn"), warn)
      expect_equal(summarize_with_margins(
        input, z = sum(.data$v), .grouping = rollup("g")
      )$z, c(2, 5, 7))
      options(old)
    }
  }
})

test_that("a nested warning can leave a caller-owned callback catcher", {
  failure <- errorCondition("older handler failed")
  caught_inside <- FALSE
  branch <- function(v) {
    if (length(v) == 2L) rlang::interrupt()
    warning("buffered warning")
    sum(v)
  }
  outcome <- tryCatch(withCallingHandlers(
    withCallingHandlers(summarize_with_margins(
      data.frame(g = c("a", "b"), v = c(2, 5)),
      z = branch(.data$v), .grouping = rollup("g")
    ), warning = function(cnd) {
      tryCatch(warning("nested warning"), error = function(cnd) {
        caught_inside <<- TRUE
      })
    }), warning = function(cnd) stop(failure)
  ), error = identity, interrupt = identity)
  expect_identical(outcome, failure)
  expect_false(caught_inside)
})

test_that("warning replay does not reactivate unavailable calling handlers", {
  old <- options(warn = 2L)
  on.exit(options(old), add = TRUE)
  cancellation <- structure(list(message = "summary cancellation"),
                            class = c("interrupt", "condition"))
  events <- character()
  outcome <- NULL
  branch <- function(v) {
    if (length(v) == 2L) {
      signalCondition(cancellation)
      rlang::interrupt()
    }
    warning("buffered warning")
    sum(v)
  }
  older <- function(cnd) {
    if (identical(conditionMessage(cnd), "outer trigger")) {
      outcome <<- tryCatch(summarize_with_margins(
        data.frame(g = c("a", "b"), v = c(2, 5)),
        z = branch(.data$v), .grouping = rollup("g")
      ), error = identity, interrupt = identity)
      invokeRestart("muffleWarning")
    }
    events <<- c(events, "older replay")
  }
  withCallingHandlers(
    withCallingHandlers(warning("outer trigger"), warning = function(cnd) {
      events <<- c(events, "newer")
    }), warning = older
  )
  expect_identical(events, "newer")
  expect_s3_class(outcome, "interrupt")
  expect_identical(outcome$interrupt, cancellation)
  expect_match(conditionMessage(outcome$replay_error), "converted from warning")
})

test_that("direct callback invocation keeps its registered handler available", {
  old <- options(warn = 2L)
  on.exit(options(old), add = TRUE)
  cancellation <- structure(list(message = "summary cancellation"),
                            class = c("interrupt", "condition"))
  observed <- list()
  outcome <- NULL
  branch <- function(v, omitted) {
    if (length(v) == 2L) {
      signalCondition(cancellation)
      rlang::interrupt()
    }
    warning(if (omitted == 0L) "first warning" else "second warning")
    sum(v)
  }
  callback <- function(cnd) {
    if (identical(conditionMessage(cnd), "direct invocation")) {
      outcome <<- tryCatch(summarize_with_margins(
        data.frame(g = c("a", "b"), h = c("u", "v"), v = c(2, 5)),
        z = branch(.data$v, grouping_bit(g)),
        .grouping = grouping_sets(grouping_set("g"), grouping_set("h"),
                                  grouping_set())
      ), error = identity, interrupt = identity)
    } else {
      observed[[length(observed) + 1L]] <<- cnd
      invokeRestart("muffleWarning")
    }
  }
  withCallingHandlers(
    do.call(callback, list(warningCondition("direct invocation"))),
    warning = callback
  )
  expect_identical(outcome, cancellation)
  expect_length(observed, 2L)
  expect_match(conditionMessage(observed[[1L]]), "first warning")
  expect_match(conditionMessage(observed[[2L]]), "second warning")
})

test_that("a warning handler can invoke the caller's original restart token", {
  old <- options(warn = 2L)
  on.exit(options(old), add = TRUE)
  observed <- 0L
  transferred <- 0L
  branch <- function(v) {
    if (length(v) == 2L) rlang::interrupt()
    warning("buffered warning")
    sum(v)
  }
  outcome <- withRestarts({
    token <- findRestart("caller_escape")
    withCallingHandlers(summarize_with_margins(
      data.frame(g = c("a", "b"), v = c(2, 5)),
      z = branch(.data$v), .grouping = rollup("g")
    ), warning = function(cnd) {
      observed <<- observed + 1L
      invokeRestart(token, "caller transfer")
    })
  }, caller_escape = function(value) {
    transferred <<- transferred + 1L
    value
  })
  expect_identical(outcome, "caller transfer")
  expect_identical(observed, 1L)
  expect_identical(transferred, 1L)
})
