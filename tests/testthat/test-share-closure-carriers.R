expect_carrier_dependencies <- function(data, expressions) {
  for (kind in c("parent", "total")) {
    share <- rlang::call2(paste0("share_of_", kind), quote(value))
    for (expr in expressions) {
      expect_error(
        summarize_with_margins(
          data, value = sum(.data$x), prior = !!share, observed = !!expr,
          .grouping = rollup("group")
        ),
        "Ordinary summaries cannot use an earlier .* share",
        class = "marginplyr_error"
      )
      expect_error(
        summarize_with_margins(
          data, prior = sum(.data$x), value = !!expr, ratio = !!share,
          .grouping = rollup("group")
        ),
        "depends on earlier summary alias `prior`",
        class = "marginplyr_error"
      )
    }
  }
}

test_that("copied closures use bindings present at invocation", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  expressions <- alist(
    (function(prior) {
      read_prior <- function() sum(prior)
      copied <- read_prior
      rm(prior)
      copied()
    })(1),
    (function(prior) {
      read_prior <- function() sum(prior)
      copied <- read_prior
      again <- copied
      rm(prior)
      again()
    })(1)
  )
  expect_carrier_dependencies(data, expressions)
  expect_carrier_dependencies(tibble::as_tibble(data), expressions)
})

test_that("literal list closures use bindings present at invocation", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  expressions <- alist(
    (function(prior) {
      read_prior <- function() sum(prior)
      functions <- list(read_prior)
      rm(prior)
      functions[[1L]]()
    })(1)
  )
  expect_carrier_dependencies(data, expressions)
  expect_carrier_dependencies(tibble::as_tibble(data), expressions)
})

test_that("returned literal closures use bindings present at invocation", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  expressions <- alist(
    (function(prior) {
      read_prior <- (function() function() sum(prior))()
      rm(prior)
      read_prior()
    })(1)
  )
  expect_carrier_dependencies(data, expressions)
  expect_carrier_dependencies(tibble::as_tibble(data), expressions)
})

closure_carrier_controls <- function() {
  alist(
    (function(prior) {
      read_prior <- function() sum(prior)
      rm(prior)
      read_prior()
    })(1),
    (function(prior) {
      read_prior <- function() sum(prior)
      rm(prior)
      copied <- read_prior
      copied()
    })(1)
  )
}

expect_retained_carriers <- function(data) {
  expressions <- alist(
    (function(prior) {
      read_prior <- function() sum(prior)
      copied <- read_prior
      copied()
    })(1),
    (function(prior) {
      read_prior <- function() sum(prior)
      copied <- read_prior
      again <- copied
      again()
    })(1),
    (function(prior) {
      read_prior <- function() sum(prior)
      functions <- list(read_prior)
      functions[[1L]]()
    })(1),
    (function(prior) {
      read_prior <- (function() function() sum(prior))()
      read_prior()
    })(1)
  )
  for (kind in c("parent", "total")) {
    share <- rlang::call2(paste0("share_of_", kind), quote(value))
    for (expr in expressions) {
      forward <- summarize_with_margins(
        data, value = sum(.data$x), prior = !!share, observed = !!expr,
        .grouping = rollup("group")
      )
      expect_equal(forward$observed, c(1, 1, 1))
      expect_equal(forward$prior, c(0.25, 0.75, 1))
      reverse <- summarize_with_margins(
        data, prior = sum(.data$x), value = !!expr, ratio = !!share,
        .grouping = rollup("group")
      )
      expect_equal(reverse$value, c(1, 1, 1))
      expect_equal(reverse$ratio, c(1, 1, 1))
    }
  }
}

test_that("direct and post-removal closure copies still refuse dependencies", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  expect_carrier_dependencies(data, closure_carrier_controls())
  expect_carrier_dependencies(tibble::as_tibble(data),
                              closure_carrier_controls())
})

test_that("retained bindings remain valid through every closure carrier", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  expect_retained_carriers(data)
  expect_retained_carriers(tibble::as_tibble(data))
})

test_that("data.table closure carriers retain their invocation dependencies", {
  skip_if_suggest_absent("data.table")
  data <- data.table::data.table(
    group = c("a", "b"), x = c(1, 3), prior = c(100, 300)
  )
  expressions <- alist(
    (function(prior) {
      read_prior <- function() sum(prior)
      copied <- read_prior
      rm(prior)
      copied()
    })(1),
    (function(prior) {
      read_prior <- function() sum(prior)
      copied <- read_prior
      again <- copied
      rm(prior)
      again()
    })(1),
    (function(prior) {
      read_prior <- function() sum(prior)
      functions <- list(read_prior)
      rm(prior)
      functions[[1L]]()
    })(1),
    (function(prior) {
      read_prior <- (function() function() sum(prior))()
      rm(prior)
      read_prior()
    })(1)
  )
  expect_carrier_dependencies(data, expressions)
  expect_carrier_dependencies(data, closure_carrier_controls())
  expect_retained_carriers(data)
})

test_that("unsupported literal list indices retain their R diagnostic", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3))
  for (index in c(Inf, 1e20)) {
    message <- if (is.infinite(index)) {
      "attempt to apply non-function"
    } else {
      "subscript out of bounds"
    }
    error <- expect_error(
      summarize_with_margins(
        data, value = sum(.data$x), ratio = share_of_total(value),
        observed = (function() {
          functions <- list(function() 1)
          functions[[!!index]]()
        })(),
        .grouping = rollup("group")
      ),
      message
    )
    expect_false(inherits(error, "marginplyr_error"))
  }
})

test_that("closure analysis does not dispatch on injected list indices", {
  local_mocked_s3_method("is.na", "share_closure_index", function(x) {
    stop("Index inspection dispatched caller code.")
  })
  index <- structure(1, class = "share_closure_index")
  data <- data.frame(group = c("a", "b"), x = c(1, 3))
  result <- summarize_with_margins(
    data, value = sum(.data$x), ratio = share_of_total(value),
    observed = (function(prior) {
      functions <- list(function() sum(prior))
      functions[[!!index]]()
    })(1),
    .grouping = rollup("group")
  )
  expect_equal(result$observed, c(1, 1, 1))
})

test_that("returned recursive closures retain independent results", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3))
  result <- summarize_with_margins(
    data, value = sum(.data$x), ratio = share_of_total(value),
    observed = (function() {
      read_prior <- (
        function() function(n) if (n == 0) 1 else read_prior(n - 1)
      )()
      read_prior(2)
    })(),
    .grouping = rollup("group")
  )
  expect_equal(result$observed, c(1, 1, 1))
})

test_that("returned closures preserve the runtime caller environment path", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  expressions <- alist(
    (function(prior) {
      read_prior <- (function() {
        function() sum(parent.env(parent.env(parent.frame()))$prior)
      })()
      rm(prior)
      read_prior()
    })(1),
    (function(prior) {
      read_prior <- (function() {
        function() {
          sum(
            parent.env(parent.env(parent.env(parent.env(environment()))))$prior
          )
        }
      })()
      rm(prior)
      read_prior()
    })(1),
    (function(prior) {
      remove_prior <- (function() {
        function() rm("prior", envir = parent.frame())
      })()
      remove_prior()
      sum(prior)
    })(1)
  )
  expect_carrier_dependencies(data, expressions)
})

test_that("returned closure arguments run in the runtime caller", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  result <- summarize_with_margins(
    data, value = sum(.data$x), prior = share_of_total(value),
    observed = (function(prior) {
      read_prior <- (function() function(value) sum(value))()
      rm(prior)
      (function(prior) read_prior(prior))(2)
    })(1),
    .grouping = rollup("group")
  )
  expect_equal(result$observed, c(2, 2, 2))
})
