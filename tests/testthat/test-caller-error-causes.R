test_that("a summary retains its caller error with a Package condition cause", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  cause <- tryCatch(share_of_total(x), error = identity)
  failure <- rlang::error_cnd(
    "caller_summary_failure", message = "User summary failed",
    parent = cause, token = new.env(parent = emptyenv())
  )
  fail <- function(x) stop(failure)
  actual <- tryCatch(
    summarize_with_margins(data, value = fail(x), .grouping = rollup(g)),
    error = identity
  )
  expect_false(inherits(actual, "marginplyr_error"))
  expect_identical(actual$parent, failure)
})

# Look through backend-owned context without changing the caller's condition.
expect_caller_error <- function(actual, failure) {
  current <- actual
  while (inherits(current, "condition") &&
           !inherits(current, "caller_summary_failure")) {
    current <- current$parent
  }
  expect_identical(current, failure)
  expect_false(inherits(actual, "marginplyr_error"))
}

# Exercise scalar and unnamed-frame summaries under both local naming paths.
# `x`, `g` and `total` are captured for the summary and Grouping data masks.
# nolint start: object_usage_linter.
expect_local_caller_causes <- function(data) {
  caught <- tryCatch(share_of_total(x), error = identity)
  for (nested in c(FALSE, TRUE)) {
    for (multiple in c(FALSE, TRUE)) {
      for (frame in c(FALSE, TRUE)) {
        for (share in c(FALSE, TRUE)) {
          state <- new.env(parent = emptyenv())
          fail <- function(x) {
            cause <- tryCatch(
              if (nested) {
                summarize_with_margins(
                  data, value = sum(x), .grouping = rollup()
                )
              } else {
                stop(caught)
              },
              error = identity
            )
            state$failure <- rlang::error_cnd(
              "caller_summary_failure", message = "User summary failed",
              parent = cause, token = new.env(parent = emptyenv())
            )
            stop(state$failure)
          }
          grouping <- if (multiple) rollup(g) else grouping_set()
          failing <- if (frame) {
            rlang::quos(data.frame(value = fail(x)))
          } else {
            rlang::quos(value = fail(x))
          }
          shares <- if (share) {
            rlang::quos(p = share_of_total(total))
          } else {
            list()
          }
          actual <- tryCatch(
            summarize_with_margins(
              data, total = sum(x), !!!failing, !!!shares,
              .grouping = grouping
            ),
            error = identity
          )
          expect_s3_class(state$failure$parent, "marginplyr_error")
          expect_caller_error(actual, state$failure)
        }
      }
    }
  }
}
# nolint end

test_that("local summaries keep caught and nested Package causes", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  expect_local_caller_causes(data)
  expect_local_caller_causes(tibble::as_tibble(data))
})

test_that("eager data tables keep caller-owned Package cause chains", {
  skip_if_suggest_absent("data.table")
  data <- data.table::data.table(g = c("a", "b"), x = c(1, 3))
  expect_local_caller_causes(data)
})

test_that("ordinary causes, extra wrappers and across keep caller errors", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  for (package in c(FALSE, TRUE)) {
    cause <- if (package) {
      tryCatch(share_of_total(x), error = identity)
    } else {
      rlang::error_cnd("ordinary_cause", message = "Ordinary cause")
    }
    failure <- rlang::error_cnd(
      "caller_summary_failure", message = "User summary failed",
      parent = cause, token = new.env(parent = emptyenv())
    )
    fail <- function(x) stop(failure)
    extra <- function(x) {
      tryCatch(fail(x), error = function(cnd) {
        rlang::abort("Callback wrapper", parent = cnd)
      })
    }
    for (summary in list(
      rlang::quos(value = fail(x)),
      rlang::quos(value = extra(x)),
      rlang::quos(dplyr::across(x, fail))
    )) {
      actual <- tryCatch(
        summarize_with_margins(data, !!!summary, .grouping = rollup(g)),
        error = identity
      )
      expect_caller_error(actual, failure)
    }
  }
})

test_that("dtplyr collection retains a caller error with a Package cause", {
  skip_if_suggest_absent("dtplyr")
  data <- dtplyr::lazy_dt(data.frame(g = c("a", "b"), x = c(1, 3)))
  cause <- tryCatch(share_of_total(x), error = identity)
  failure <- rlang::error_cnd(
    "caller_summary_failure", message = "User summary failed",
    parent = cause, token = new.env(parent = emptyenv())
  )
  fail <- function(x) stop(failure)
  query <- summarize_with_margins(data, value = fail(x), .grouping = rollup(g))
  actual <- tryCatch(dplyr::collect(query), error = identity)
  expect_caller_error(actual, failure)
})

test_that("direct Package refusals retain their public call context", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  failure <- tryCatch(share_of_total(x), error = identity)
  fail <- function(x) stop(failure)
  actual <- tryCatch(
    summarize_with_margins(data, value = fail(x), .grouping = rollup(g)),
    error = identity
  )
  expect_s3_class(actual, "marginplyr_error")
  expect_identical(conditionMessage(actual), conditionMessage(failure))
  expect_identical(actual$parent, failure$parent)
  expect_identical(
    rlang::call_name(conditionCall(actual)), "summarize_with_margins"
  )
  collision <- expect_error(
    summarize_with_margins(data, g = sum(x), .grouping = rollup(g)),
    "cannot overwrite grouping column", class = "marginplyr_error"
  )
  expect_identical(
    rlang::call_name(conditionCall(collision)), "summarize_with_margins"
  )
})

test_that("generated across glue checks retain their own reporting", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  collision <- expect_error(
    summarize_with_margins(
      data,
      dplyr::across(x, sum, .names = "{'g'}", .unpack = identity(FALSE)),
      .grouping = rollup(g)
    ),
    "cannot overwrite grouping column", class = "marginplyr_error"
  )
  expect_null(collision$parent)
  expect_identical(
    rlang::call_name(conditionCall(collision)), "summarize_with_margins"
  )
  cause <- tryCatch(share_of_total(x), error = identity)
  failure <- rlang::error_cnd(
    "caller_summary_failure", message = "Caller name template failed",
    parent = cause, token = new.env(parent = emptyenv())
  )
  actual <- tryCatch(
    summarize_with_margins(
      data,
      dplyr::across(
        x, sum, .names = "{stop(failure)}", .unpack = identity(FALSE)
      ),
      .grouping = rollup(g)
    ),
    error = identity
  )
  expect_caller_error(actual, failure)
  expect_identical(
    rlang::call_name(conditionCall(actual)), "summarize_with_margins"
  )
  wrapped <- rlang::error_cnd(
    "caller_summary_failure", message = "Caught generated collision",
    parent = collision, token = new.env(parent = emptyenv())
  )
  fail <- function(x) stop(wrapped)
  propagated <- tryCatch(
    summarize_with_margins(data, value = fail(x), .grouping = rollup(g)),
    error = identity
  )
  expect_caller_error(propagated, wrapped)
})

test_that("generated selector refusals release their temporary ownership", {
  data <- data.frame(value = 1)
  refusal <- tryCatch(
    summarize_with_margins(
      data, previous = sum(value), source = sum(dplyr::pick(previous)),
      share = share_of_total(source), .grouping = grouping_set()
    ), error = identity
  )
  expect_s3_class(refusal, "marginplyr_error")
  expect_null(refusal$parent)
  expect_identical(
    rlang::call_name(conditionCall(refusal)), "summarize_with_margins"
  )
  failure <- rlang::error_cnd(
    "caller_summary_failure", message = "Caught selector refusal",
    parent = refusal, token = new.env(parent = emptyenv())
  )
  fail <- function(x) stop(failure)
  actual <- tryCatch(
    summarize_with_margins(
      data, source = fail(value), .grouping = grouping_set()
    ), error = identity
  )
  expect_caller_error(actual, failure)
  selected <- tryCatch(
    summarize_with_margins(
      data, previous = sum(value),
      source = sum(dplyr::pick(dplyr::all_of(fail()))),
      share = share_of_total(source), .grouping = grouping_set()
    ), error = identity
  )
  expect_caller_error(selected, failure)
})
