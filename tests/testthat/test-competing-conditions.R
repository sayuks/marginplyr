# Acceptance at the public eager summary boundary selected by #756.
test_that("interruption after summary evaluation survives exit replay", {
  checkpoints <- c("combine_margin_branches", "restore_input_window_order")
  for (checkpoint in checkpoints) {
    for (warn in c(1L, 2L)) {
      old <- options(warn = warn)
      on.exit(options(old), add = TRUE)
      input <- data.frame(g = c("a", "b"), v = c(2, 5))
      original <- input
      effects <- numeric()
      reached <- FALSE
      cancellation <- structure(list(message = "assembly cancellation"),
                                class = c("interrupt", "condition"))
      branch <- function(v) {
        effects <<- c(effects, sum(v))
        warning("earlier warning")
        sum(v)
      }
      # These checkpoints run after all caller expressions have completed,
      # while the public summary still owns buffered warnings for exit replay.
      suppressMessages(trace(
        checkpoint, where = asNamespace("marginplyr"), print = FALSE,
        tracer = function() {
          reached <<- TRUE
          signalCondition(cancellation)
          rlang::interrupt()
        }
      ))
      on.exit(suppressMessages(untrace(
        checkpoint, where = asNamespace("marginplyr")
      )), add = TRUE)
      outcome <- tryCatch(withCallingHandlers(
        summarize_with_margins(input, z = branch(.data$v),
                               .grouping = rollup("g")),
        warning = function(cnd) {
          if (warn == 1L) invokeRestart("muffleWarning")
        }
      ), interrupt = identity, error = identity)
      suppressMessages(untrace(checkpoint, where = asNamespace("marginplyr")))
      expect_true(reached)
      expect_identical(effects, c(2, 5, 7))
      expect_s3_class(outcome, "interrupt")
      expect_false(inherits(outcome, "error"))
      if (warn == 2L) {
        expect_identical(outcome$interrupt, cancellation)
        expect_s3_class(outcome$replay_error, "error")
        expect_match(conditionMessage(outcome$replay_error),
                     "converted from warning")
      } else {
        expect_identical(outcome, cancellation)
      }
      expect_identical(input, original)
      expect_identical(getOption("warn"), warn)
      options(old)
      expect_equal(
        summarize_with_margins(input, z = sum(.data$v),
                               .grouping = rollup("g"))$z, c(2, 5, 7)
      )
    }
  }
})

test_that("earlier warnings cannot replace a later summary interrupt", {
  for (warn in c(1L, 2L)) {
    old <- options(warn = warn)
    on.exit(options(old), add = TRUE)
    events <- character()
    input <- data.frame(g = c("a", "b"), v = c(2, 5))
    original <- input
    cancellation <- structure(list(message = "first cancellation"),
                              class = c("interrupt", "condition"))
    summary_fun <- function(v) {
      if (length(v) == 2L) {
        events <<- c(events, "later branch")
        signalCondition(cancellation)
        rlang::interrupt()
      }
      events <<- c(events, "earlier effect")
      warning("earlier warning")
      sum(v)
    }
    outcome <- tryCatch(
      withCallingHandlers(
        summarize_with_margins(
          input,
          z = summary_fun(.data$v),
          .grouping = rollup("g")
        ),
        warning = function(cnd) {
          events <<- c(events, "replay")
          if (warn == 1L) invokeRestart("muffleWarning")
        }
      ),
      interrupt = identity, error = identity
    )
    expect_s3_class(outcome, "interrupt")
    expect_false(inherits(outcome, "error"))
    expect_identical(
      events, c("earlier effect", "earlier effect", "later branch", "replay")
    )
    expect_identical(input, original)
    expect_identical(getOption("warn"), warn)
    if (warn == 2L) {
      expect_s3_class(outcome$replay_error, "error")
      expect_match(conditionMessage(outcome$replay_error),
                   "converted from warning")
      expect_identical(outcome$interrupt, cancellation)
    }
    options(old)
    expect_equal(
      summarize_with_margins(input, z = sum(.data$v),
                             .grouping = rollup("g"))$z,
      c(2, 5, 7)
    )
  }
})

# Multiple branches establish repeated and distinct warning identities before
# cancellation, so replay order and short-circuit are observable at the verb.
test_that("muffled interrupt replay preserves repeated and distinct warnings", {
  old <- options(warn = 2L)
  on.exit(options(old), add = TRUE)
  input <- data.frame(a = c("x", "x", "y"), b = c("u", "v", "v"),
                      v = c(2, 3, 5))
  first <- structure(list(message = "first cancellation"),
                     class = c("interrupt", "condition"))
  effects <- numeric()
  observed <- list()
  run_branch <- function(v, omitted) {
    if (length(v) == 3L) {
      signalCondition(first)
      rlang::interrupt()
    }
    effects <<- c(effects, sum(v))
    warning(if (omitted == 0L) "first warning" else "second warning")
    sum(v)
  }
  outcome <- tryCatch(
    withCallingHandlers(
      summarize_with_margins(
        input, z = run_branch(.data$v, grouping_bit(a)),
        .grouping = grouping_sets(grouping_set("a"), grouping_set("a"),
                                  grouping_set("b"), grouping_set()),
        .duplicates = "keep"
      ),
      warning = function(cnd) {
        observed[[length(observed) + 1L]] <<- cnd
        invokeRestart("muffleWarning")
      }
    ),
    interrupt = identity, error = identity
  )
  expect_s3_class(outcome, "interrupt")
  expect_false(inherits(outcome, "error"))
  expect_identical(effects, c(5, 5, 5, 5, 2, 8))
  expect_match(conditionMessage(observed[[1L]]), "first warning")
  expect_match(conditionMessage(observed[[1L]]), "1 further grouping set")
  expect_false(grepl("marginplyr_key", conditionMessage(observed[[1L]])))
  expect_length(observed, 2L)
  expect_match(conditionMessage(observed[[2L]]), "second warning")
  expect_null(outcome$replay_error)
  expect_identical(outcome, first)
  expect_identical(getOption("warn"), 2L)
})

test_that("replay interruption retains an earlier summary error and chain", {
  cause <- rlang::error_cnd(message = "execution failed",
                            parent = simpleError("original cause"))
  original <- serialize(cause, NULL)
  cancellation <- structure(list(message = "replay cancellation"),
                            class = c("interrupt", "condition"))
  input <- data.frame(g = c("a", "b"), v = c(2, 5))
  effects <- numeric()
  failed <- FALSE
  branch <- function(v) {
    if (length(v) == 2L) {
      failed <<- TRUE
      stop(cause)
    }
    effects <<- c(effects, sum(v))
    warning("earlier warning")
    sum(v)
  }
  # The warning system boundary is reached after the failing branch; injecting
  # there exercises replay without a caller handler's nonlocal transfer.
  reached <- FALSE
  suppressMessages(trace(
    "warning", where = baseenv(), print = FALSE,
    tracer = function() {
      frame <- parent.frame()
      args <- evalq(list(...), frame)
      if (failed && length(args) > 0L &&
            inherits(args[[1L]], "rlang_warning")) {
        reached <<- TRUE
        signalCondition(cancellation)
        rlang::interrupt()
      }
    }
  ))
  on.exit(suppressMessages(untrace("warning", where = baseenv())), add = TRUE)
  outcome <- tryCatch(
    summarize_with_margins(input, z = branch(.data$v), .grouping = rollup("g")),
    interrupt = identity, error = identity
  )
  expect_true(reached)
  expect_s3_class(outcome, "interrupt")
  expect_identical(outcome$interrupt, cancellation)
  expect_identical(outcome$parent$parent, cause)
  expect_identical(serialize(cause, NULL), original)
  expect_match(conditionMessage(outcome$parent), "execution failed")
  expect_false(inherits(outcome, "marginplyr_error"))
  expect_identical(effects, c(2, 5))
})

# testthat's own returning interrupt handler aborts before native fallback.
# Intercept only base notification here; the independent process tests exercise
# real notification, native hooks, and returning handlers without that reporter.
test_that("unclaimed summary interruption enters native cancellation", {
  old <- options(warn = 2L)
  on.exit(options(old), add = TRUE)
  notified <- NULL
  original_signal <- base::signalCondition
  testthat::local_mocked_bindings(
    signalCondition = function(cnd) {
      if (inherits(cnd, "interrupt")) {
        notified <<- cnd
        return(invisible(NULL))
      }
      original_signal(cnd)
    },
    .package = "base"
  )
  branch <- function(v) {
    if (length(v) == 2L) rlang::interrupt()
    warning("earlier warning")
    sum(v)
  }
  native <- tryCatch(
    summarize_with_margins(data.frame(g = c("a", "b"), v = c(2, 5)),
                           z = branch(.data$v), .grouping = rollup("g")),
    interrupt = identity, error = identity
  )
  expect_s3_class(native, "interrupt")
  expect_false(inherits(native, "error"))
  expect_s3_class(notified$interrupt, "interrupt")
  expect_s3_class(notified$replay_error, "error")
})
