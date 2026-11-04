test_that("formal-default removals expose both forbidden share dependencies", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  original <- serialize(data, NULL)
  counts <- new.env(parent = emptyenv())
  counts$calls <- 0L
  touch <- function() {
    counts$calls <- counts$calls + 1L
  }
  removals <- alist(
    base::rm(prior),
    base::remove("prior"),
    base::rm(list = "prior")
  )
  removals <- c(removals, list(rlang::call2(
    "if", TRUE, quote(base::rm(prior))
  )))
  for (input in list(data, dbplyr::tbl_lazy(data))) {
    for (kind in c("parent", "total")) {
      helper <- rlang::call2(paste0("share_of_", kind), quote(value))
      for (removal in removals) {
        for (demand in alist(prior, force(prior))) {
          default <- rlang::expr({
            touch()
            !!removal
            1
          })
          body <- rlang::expr({
            !!demand
            sum(prior)
          })
          definition <- quote(function(prior) NULL)
          definition[[2L]] <- as.pairlist(list(prior = default))
          definition[[3L]] <- body
          expression <- as.call(list(definition))
          expect_error(
            summarize_with_margins(
              input, prior = sum(x), value = !!expression,
              fraction = !!helper, .grouping = rollup(group)
            ),
            "depends on earlier summary alias `prior`",
            class = "marginplyr_error"
          )
          expect_error(
            summarize_with_margins(
              input, value = sum(x), prior = !!helper,
              derived = !!expression, .grouping = rollup(group)
            ),
            "Ordinary summaries cannot use an earlier .* share",
            class = "marginplyr_error"
          )
          expect_identical(counts$calls, 0L)
          expect_identical(serialize(data, NULL), original)
        }
      }
    }
  }
})

test_that("surviving formal bindings retain values and evaluation counts", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3))
  original <- serialize(data, NULL)
  counts <- new.env(parent = emptyenv())
  counts$calls <- 0L
  touch <- function() {
    counts$calls <- counts$calls + 1L
  }
  rm <- function(...) NULL
  expressions <- alist(
    (function(prior = {
      touch()
      2
    }) {
      prior
      sum(prior)
    })(),
    (function(prior = other, other = 2) {
      touch()
      sum(prior)
    })(),
    (function(prior = {
      touch()
      quote(base::rm(prior))
      2
    }) {
      prior
      sum(prior)
    })(),
    (function(prior = {
      touch()
      if (FALSE) base::rm(prior)
      2
    }) {
      prior
      sum(prior)
    })(),
    (function(prior = {
      touch()
      local({
        prior <- 0
        base::rm(prior)
      })
      2
    }) {
      prior
      sum(prior)
    })(),
    (function(prior = {
      touch()
      base::rm(other)
      2
    }, other = 0) {
      prior
      sum(prior)
    })(),
    (function(prior = {
      touch()
      rm(prior)
      2
    }) {
      prior
      sum(prior)
    })(),
    (function(prior = {
      touch()
      base::rm(prior)
      1
    }) {
      prior
      prior <- 2
      sum(prior)
    })()
  )
  for (kind in c("parent", "total")) {
    helper <- rlang::call2(paste0("share_of_", kind), quote(value))
    for (expression in expressions) {
      counts$calls <- 0L
      result <- summarize_with_margins(
        data, prior = sum(x), value = !!expression,
        fraction = !!helper, .grouping = rollup(group)
      )
      expect_equal(result$value, c(2, 2, 2))
      expect_equal(result$fraction, c(1, 1, 1))
      expect_identical(counts$calls, 3L)
      counts$calls <- 0L
      result <- summarize_with_margins(
        data, value = sum(x), prior = !!helper,
        derived = !!expression, .grouping = rollup(group)
      )
      expect_equal(result$derived, c(2, 2, 2))
      expect_identical(counts$calls, 3L)
    }
  }
  expect_identical(serialize(data, NULL), original)
})

test_that("default removals preserve external and ordinary alias lookups", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  original <- serialize(data, NULL)
  outside <- new.env(parent = emptyenv())
  outside$prior <- c(2, 4)
  counts <- new.env(parent = emptyenv())
  counts$calls <- 0L
  touch <- function() {
    counts$calls <- counts$calls + 1L
  }
  for (kind in c("parent", "total")) {
    helper <- rlang::call2(paste0("share_of_", kind), quote(value))
    counts$calls <- 0L
    result <- summarize_with_margins(
      data, prior = sum(x),
      value = (function(prior = {
        touch()
        base::rm(prior)
        1
      }) {
        prior
        sum(get("prior", envir = .env$outside))
      })(),
      fraction = !!helper, .grouping = rollup(group)
    )
    expect_equal(result$value, c(6, 6, 6))
    expect_equal(result$fraction, c(1, 1, 1))
    expect_identical(counts$calls, 3L)
  }
  counts$calls <- 0L
  result <- summarize_with_margins(
    data, prior = sum(x),
    value = (function(prior = {
      touch()
      base::rm(prior)
      1
    }) {
      prior
      sum(prior)
    })(),
    .grouping = rollup(group)
  )
  expect_equal(result$value, c(1, 3, 4))
  expect_identical(counts$calls, 3L)
  expect_identical(outside$prior, c(2, 4))
  expect_identical(serialize(data, NULL), original)
})
