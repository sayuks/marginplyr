test_that("a nested read can force a default that exposes an earlier share", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  expect_error(
    summarize_with_margins(
      data, value = sum(x), prior = share_of_total(value),
      derived = (function(prior = {
        base::rm(prior)
        1
      }) {
        (function() {
          prior
          sum(prior)
        })()
      })(),
      .grouping = rollup(group)
    ),
    "Ordinary summaries cannot use an earlier Total share",
    class = "marginplyr_error"
  )
})

test_that("visible inline scope changes expose both forbidden dependencies", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  original <- serialize(data, NULL)
  counters <- new.env(parent = emptyenv())
  counters$default <- counters$body <- counters$summary <- 0L
  expressions <- alist(
    (function(prior = {
      counters$default <- counters$default + 1L
      base::rm(prior)
      1
    }) {
      (((function() {
        counters$body <- counters$body + 1L
        (prior)
        sum(prior)
      })))()
    })(),
    (function(prior = {
      counters$default <- counters$default + 1L
      base::remove(prior)
      1
    }) {
      ((function() {
        counters$body <- counters$body + 1L
        ((base::force))((prior))
      })())
      sum(prior)
    })(),
    (function(prior = 1) {
      (function() {
        counters$body <- counters$body + 1L
        base::rm(prior, envir = parent.frame())
      })()
      sum(prior)
    })(),
    (function(prior = 1) {
      ((function() {
        counters$body <- counters$body + 1L
        ((base::remove))(list = "prior", envir = ((base::parent.frame())))
        sum(prior)
      })())
    })(),
    (function(prior = 1) {
      (function() {
        counters$body <- counters$body + 1L
        base::rm(prior, inherits = TRUE)
      })()
      sum(prior)
    })()
  )
  for (input in list(data, dbplyr::tbl_lazy(data))) {
    for (kind in c("parent", "total")) {
      helper <- rlang::call2(paste0("share_of_", kind), quote(value))
      for (expression in expressions) {
        expect_error(
          summarize_with_margins(
            input, prior = {
              counters$summary <- counters$summary + 1L
              sum(x)
            }, value = !!expression, fraction = !!helper,
            .grouping = rollup(group)
          ),
          "depends on earlier summary alias `prior`",
          class = "marginplyr_error"
        )
        expect_error(
          summarize_with_margins(
            input, value = {
              counters$summary <- counters$summary + 1L
              sum(x)
            }, prior = !!helper, derived = !!expression,
            .grouping = rollup(group)
          ),
          "Ordinary summaries cannot use an earlier .* share",
          class = "marginplyr_error"
        )
      }
    }
  }
  expect_identical(counters$body, 0L)
  expect_identical(counters$default, 0L)
  expect_identical(counters$summary, 0L)
  expect_identical(serialize(data, NULL), original)
})

test_that("inline scope changes preserve independent and shadowed bindings", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  counts <- new.env(parent = emptyenv())
  counts$calls <- 0L
  touch <- function() {
    counts$calls <- counts$calls + 1L
    2
  }
  outside <- new.env(parent = emptyenv())
  outside$prior <- 2
  rm <- function(...) NULL
  expressions <- alist(
    (function(prior = touch()) {
      (function() {
        base::force(prior)
        sum(prior)
      })()
    })(),
    (function(prior = stop("Default is shadowed.")) {
      (function(prior = touch()) sum(prior))()
    })(),
    (function(prior = {
      base::rm(prior)
      stop("Default is replaced.")
    }) {
      prior <- touch()
      (function() sum(prior))()
    })(),
    (function(prior = {
      base::rm(prior)
      stop("Default is supplied.")
    }) {
      (function() sum(prior))()
    })(touch()),
    (function(prior = {
      base::rm(prior)
      stop("Default is shadowed.")
    }) {
      (function() {
        prior <- touch()
        sum(prior)
      })()
    })(),
    (function(prior = touch()) {
      (function(prior = 0) {
        base::rm(prior)
        sum(prior)
      })()
    })(),
    (function(prior = touch()) {
      (function(prior = touch()) {
        base::rm(prior, envir = parent.frame())
        sum(prior)
      })()
    })(),
    (function(prior = touch()) {
      (function() {
        quote(base::rm(prior, envir = parent.frame()))
        if (FALSE) base::remove(prior, envir = parent.frame())
        rm(prior, envir = parent.frame())
      })()
      sum(prior)
    })(),
    (function(prior = {
      touch()
      base::rm(prior)
      1
    }) {
      (function() {
        prior
        get("prior", envir = .env$outside)
      })()
    })(),
    (function(prior = {
      base::rm(prior)
      touch()
    }) {
      (function() {
        identity({
          prior <- 2
          prior
        })
      })()
      sum(prior)
    })()
  )
  for (kind in c("parent", "total")) {
    helper <- rlang::call2(paste0("share_of_", kind), quote(value))
    counts$calls <- 0L
    control <- summarize_with_margins(
      data, prior = sum(x), value = touch(), fraction = !!helper,
      .grouping = rollup(group)
    )
    control_calls <- counts$calls
    expect_equal(control$value, c(2, 2, 2))
    expect_identical(control_calls, 3L)
    for (expression in expressions) {
      counts$calls <- 0L
      result <- summarize_with_margins(
        data, prior = sum(x), value = !!expression, fraction = !!helper,
        .grouping = rollup(group)
      )
      expect_identical(result, control)
      expect_identical(counts$calls, control_calls)
      counts$calls <- 0L
      result <- summarize_with_margins(
        data, value = sum(x), prior = !!helper, derived = !!expression,
        .grouping = rollup(group)
      )
      expect_equal(result$derived, control$value)
      expect_identical(counts$calls, control_calls)
    }
  }
  expect_identical(outside$prior, 2)
})

test_that("inline scope changes keep ordinary evaluation without shares", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  original <- serialize(data, NULL)
  counts <- new.env(parent = emptyenv())
  counts$calls <- 0L
  touch <- function() {
    counts$calls <- counts$calls + 1L
    1
  }
  control <- summarize_with_margins(
    data, prior = sum(x), derived = {
      touch()
      sum(prior)
    },
    .grouping = rollup(group)
  )
  control_calls <- counts$calls
  expect_equal(control$derived, c(1, 3, 4))
  expect_identical(control_calls, 3L)
  expressions <- alist(
    (function(prior = {
      touch()
      base::rm(prior)
      1
    }) {
      (function() {
        base::force(prior)
        sum(prior)
      })()
    })(),
    (function(prior = 0) {
      (function() {
        touch()
        base::remove("prior", envir = parent.frame())
      })()
      sum(prior)
    })()
  )
  for (expression in expressions) {
    counts$calls <- 0L
    result <- summarize_with_margins(
      data, prior = sum(x), derived = !!expression, .grouping = rollup(group)
    )
    expect_identical(result, control)
    expect_identical(counts$calls, control_calls)
  }
  expect_identical(serialize(data, NULL), original)
})
