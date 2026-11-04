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
  counters$target <- 0L
  make_target <- function() {
    counters$target <- counters$target + 1L
    parent.frame()
  }
  expressions <- alist(
    (function() {
      base::rm("prior", envir = make_target())
      sum(.data$prior)
    })(),
    (function(prior) {
      (function() {
        base::rm("prior", envir = sys.frame(sys.parent()))
      })()
      sum(prior)
    })(1),
    (function(prior) {
      (function(prior) {
        (function() {
          base::rm("prior", envir = base::parent.frame(n = 2L))
        })()
      })(2)
      sum(prior)
    })(1),
    (function(prior) {
      (function() base::rm("prior", inherits = 1L))()
      sum(prior)
    })(1),
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
      (function() {
        counters$body <- counters$body + 1L
        base::rm(prior, envir = base::parent.frame(n = ))
      })()
      sum(prior)
    })(),
    (function(prior = 1) {
      ((function() {
        counters$body <- counters$body + 1L
        ((base::remove))(
          list = "prior", envir = ((base::parent.frame(n = 1L)))
        )
        sum(prior)
      })())
    })(),
    (function(prior = 1) {
      (function() {
        counters$body <- counters$body + 1L
        base::rm(prior, inherits = TRUE)
      })()
      sum(prior)
    })(),
    (function(first = {
      counters$default <- counters$default + 1L
      base::rm(prior)
      1
    }, prior = 0) {
      if (sum(x) < 0) first
      prior <- 2
      (function() base::force(first))()
      sum(prior)
    })(),
    (function(prior = 2, arg = sum(prior)) {
      (function() {
        base::rm(prior, envir = parent.frame())
        arg
      })()
    })(),
    (function(prior = 2) {
      (function(arg) {
        base::rm(prior, envir = parent.frame())
        arg
      })(sum(prior))
    })(),
    (function(prior = {
      base::rm(prior)
      function() NULL
    }) {
      (function() {
        prior()
        sum(prior)
      })()
    })(),
    (function(prior = {
      base::rm(prior)
      function() NULL
    }) {
      (function() {
        prior <- 0
        prior()
      })()
      sum(prior)
    })(),
    (function(prior = 1) {
      (function() {
        rm <- function(...) NULL
        if (length(x) > 0L) rm <- 0
        rm(prior, envir = parent.frame())
        sum(prior)
      })()
    })(),
    (function(prior = 1) {
      (function() {
        rm <- function(...) NULL
        base::assign("rm", 0)
        rm(prior, envir = parent.frame())
        sum(prior)
      })()
    })(),
    (function(prior = 1) {
      (function() {
        `&&` <- function(x, y) y
        FALSE && base::rm(prior, envir = parent.frame())
        sum(prior)
      })()
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
  expect_identical(counters$target, 0L)
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
      (function(prior) {
        (function() base::rm("prior", envir = sys.frame(sys.parent())))()
      })(0)
      sum(prior)
    })(),
    (function(prior = touch()) {
      (function(prior) {
        (function() base::rm("prior", envir = parent.frame(2L)))()
        sum(prior)
      })(touch())
    })(),
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
      stop("Default is replaced in a reachable arm.")
    }) {
      if (TRUE) prior <- touch()
      (function() {
        base::force(prior)
        sum(prior)
      })()
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
    (function(prior = {
      base::rm(prior)
      stop("Default is shadowed in a forced block.")
    }) {
      (function() {
        base::force({
          prior <- touch()
        })
        prior
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
        base::rm(prior, envir = .env$outside)
        assign("prior", 2, envir = .env$outside)
        sum(prior)
      })()
    })(),
    (function(prior = touch()) {
      callback <- function() {
        base::rm(prior, envir = parent.frame())
        sum(prior)
      }
      if (is.function(callback)) sum(prior)
    })(),
    (function(prior = {
      base::rm(prior)
      stop("An uninvoked definition cannot demand this default.")
    }) {
      callback <- function() {
        prior
        sum(prior)
      }
      if (is.function(callback)) touch()
    })(),
    (function(prior = touch()) {
      (function() {
        quote(base::rm(prior, envir = parent.frame()))
        if (FALSE) base::remove(prior, envir = parent.frame())
        rm(prior, envir = parent.frame())
      })()
      sum(prior)
    })(),
    (function(prior = touch()) {
      (function() {
        rm <- function(...) NULL
        rm(prior, envir = parent.frame())
      })()
      sum(prior)
    })(),
    (function(prior = touch()) {
      (function() {
        FALSE && base::rm(prior, envir = parent.frame())
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
    })(),
    (function(prior = {
      base::rm(prior)
      stop("A dead branch must not run.")
    }) {
      if (FALSE) {
        (function() {
          base::force(prior)
          sum(prior)
        })()
      }
      touch()
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
    (function(prior = 0) {
      (function() {
        touch()
        base::rm("prior", envir = sys.frame(sys.parent()))
      })()
      sum(prior)
    })(),
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

test_that("invalid inline argument matching keeps the evaluation error", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3))
  state <- new.env(parent = emptyenv())
  state$evaluated <- FALSE
  expect_error(
    summarize_with_margins(
      data, prior = {
        state$evaluated <- TRUE
        sum(x)
      },
      value = (function(prior = 1) sum(prior))(unused = 2),
      fraction = share_of_total(value), .grouping = rollup(group)
    ),
    "unused argument (unused = 2)",
    fixed = TRUE
  )
  expect_true(state$evaluated)
})
