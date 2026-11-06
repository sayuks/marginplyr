visible_closure_data <- function() {
  data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
}

visible_closure_reads <- function() {
  alist(
    (function(prior) {
      copied <- {
        function() sum(prior)
      }
      rm(prior)
      copied()
    })(1),
    (function(prior) {
      copied <- if (TRUE) function() sum(prior)
      rm(prior)
      copied()
    })(1),
    (function(prior) {
      copied <- switch(1L, function() sum(prior))
      rm(prior)
      copied()
    })(1),
    (function(prior) {
      copied <- (function() {
        # The explicit return carries the closure value under test.
        return(function() sum(prior)) # nolint: return_linter.
      })()
      rm(prior)
      copied()
    })(1),
    (function(prior) {
      copied <- (function() {
        invisible(NULL)
        function() sum(prior)
      })()
      rm(prior)
      copied()
    })(1),
    (function(prior) {
      factory <- function() function() sum(prior)
      copied <- factory()
      rm(prior)
      copied()
    })(1)
  )
}

visible_closure_retained <- function(expr) {
  if (!rlang::is_call(expr)) return(expr)
  if (rlang::is_call(expr, "rm")) return(quote(invisible(NULL)))
  as.call(lapply(as.list(expr), visible_closure_retained))
}

expect_retained_visible <- function(data) {
  for (kind in c("parent", "total")) {
    helper <- rlang::call2(paste0("share_of_", kind), quote(value))
    for (read in lapply(visible_closure_reads(), visible_closure_retained)) {
      forward <- summarize_with_margins(
        data, value = sum(.data$x), prior = !!helper, observed = !!read,
        .grouping = rollup("g")
      )
      reverse <- summarize_with_margins(
        data, prior = sum(.data$x), value = !!read, ratio = !!helper,
        .grouping = rollup("g")
      )
      if (!is.data.frame(data)) {
        forward <- dplyr::collect(forward)
        reverse <- dplyr::collect(reverse)
      }
      expect_equal(forward$observed, c(1, 1, 1))
      expect_equal(forward$prior, c(0.25, 0.75, 1))
      expect_equal(reverse$value, c(1, 1, 1))
      expect_equal(reverse$ratio, c(1, 1, 1))
    }
  }
}

expect_visible_closures <- function(data) {
  before <- serialize(data, NULL)
  expect_share_dependencies(data, visible_closure_reads())
  expect_retained_visible(data)
  expect_identical(serialize(data, NULL), before)
}

test_that("visible closure syntax retains eager invocation dependencies", {
  expect_visible_closures(visible_closure_data())
  expect_visible_closures(tibble::as_tibble(visible_closure_data()))
})

test_that("data.table visible closure values retain dependencies", {
  skip_if_suggest_absent("data.table")
  expect_visible_closures(data.table::as.data.table(visible_closure_data()))
})

test_that("dtplyr visible closure values retain dependencies at planning", {
  skip_if_suggest_absent("dtplyr")
  expect_visible_closures(dtplyr::lazy_dt(visible_closure_data()))
})

test_that("visible factory analysis never invokes caller functions", {
  probe <- rlang::env(calls = 0L)
  read <- quote((function(prior) {
    factory <- function() {
      probe$calls <- probe$calls + 1L
      function() sum(prior)
    }
    copied <- factory()
    rm(prior)
    copied()
  })(1))
  expect_share_dependencies(visible_closure_data(), list(read))
  expect_identical(probe$calls, 0L)
})
