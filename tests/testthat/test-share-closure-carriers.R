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

closure_carrier_dependencies <- function() {
  alist(
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
}

test_that("closure carriers use bindings present at invocation", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  expect_carrier_dependencies(data, closure_carrier_dependencies())
  expect_carrier_dependencies(tibble::as_tibble(data),
                              closure_carrier_dependencies())
})

computed_head_dependencies <- function() {
  as.list(quote(alist(
    (function(prior) {
      read <- function() sum(prior)
      functions <- list(read, {
        rm(prior)
        read <- function() 7
      })
      functions[[1L]]()
    })(1),
    (function(prior) {
      read <- function() 7
      list({
        rm(prior)
        read <- function() sum(prior)
      }, read)[[2L]]()
    })(1),
    list(prior, function() 7)[[2L]](),
    (identity(function() sum(prior)))(),
    list(lapply(1, function(i) sum(prior)), function() 7)[[2L]](),
    list(function(arg = prior) sum(arg))[[1L]](),
    (function() function(arg = prior) sum(arg))()(),
    (function(prior) {
      list(rm(prior), function() sum(prior))[[2L]]()
    })(1),
    (function(prior) {
      list(rm(prior), function() 7)[[2L]]()
      sum(prior)
    })(1),
    (function(prior) {
      (function() {
        list(rm("prior", envir = parent.frame()), function() 7)[[2L]]()
      })()
      sum(prior)
    })(1)
  )))[-1L]
}

test_that("computed closure heads retain their evaluated dependencies", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  expect_carrier_dependencies(data, computed_head_dependencies())
  expect_carrier_dependencies(tibble::as_tibble(data),
                              computed_head_dependencies())
})

test_that("data.table computed closure heads retain evaluated dependencies", {
  skip_if_suggest_absent("data.table")
  data <- data.table::data.table(
    group = c("a", "b"), x = c(1, 3), prior = c(100, 300)
  )
  expect_carrier_dependencies(data, computed_head_dependencies())
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
  external <- rlang::env(prior = 1)
  expressions <- c(
    as.list(quote(alist(
      (function(prior) {
        read <- function() 1
        list(read, {
          rm(prior)
          read <- function() sum(prior)
        })[[1L]]()
      })(1),
      (function(arg = prior) 1)(2),
      list(function(arg = prior) 1)[[1L]](2),
      (function() function(arg = prior) 1)()(2)
    )))[-1L],
    alist(
      list(external$prior, function() sum(external$prior))[[2L]](),
      (function(prior) {
        list(prior, function() sum(prior))[[2L]]()
      })(1),
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
  expect_carrier_dependencies(data, closure_carrier_dependencies())
  expect_carrier_dependencies(data, closure_carrier_controls())
  expect_retained_carriers(data)
})

test_that("computed closure dependency refusal leaves lazy input unread", {
  skip_if_suggest_absent("dtplyr")
  probe <- new.env(parent = emptyenv())
  probe$calls <- 0L
  observe <- function(x) {
    probe$calls <- probe$calls + 1L
    x
  }
  data <- dtplyr::lazy_dt(data.frame(group = c("a", "b"), x = c(1, 3)))
  data <- dplyr::filter(data, observe(x) > 0)
  expect_carrier_dependencies(data, computed_head_dependencies())
  expect_identical(probe$calls, 0L)
  expect_equal(dplyr::collect(data)$x, c(1, 3))
  expect_identical(probe$calls, 1L)
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

test_that("supplied list arguments retain member dependencies", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  reads <- alist(
    (function(prior) {
      carrier <- list(read = function() sum(prior))
      rm(prior)
      (function(value) value$read())(carrier)
    })(1),
    (function(prior) {
      carrier <- list(read = function() sum(prior))
      rm(prior)
      (function(value) {
        (function(forwarded) {
          copied <- forwarded
          copied$read()
        })(value)
      })(carrier)
    })(1),
    (function(prior) {
      rm(prior)
      (function(value = list(read = function() sum(prior))) value$read())()
    })(1)
  )
  expect_carrier_dependencies(data, reads)
})

test_that("inline list factories retain returned member dependencies", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  reads <- alist(
    (function(prior) {
      carrier <- (function() list(read = function() sum(prior)))()
      rm(prior)
      carrier$read()
    })(1),
    (function(prior) {
      carrier <- (function() {
        read <- function() sum(prior)
        # The explicit return carries the list value under test.
        return(list(read = read)) # nolint: return_linter.
      })()
      rm(prior)
      carrier$read()
    })(1),
    (function(prior) {
      carrier <- list(outer = list(read = function() sum(prior)))
      forwarded <- (function(value) value)(carrier)
      rm(prior)
      forwarded$outer$read()
    })(1)
  )
  expect_carrier_dependencies(data, reads)
})

test_that("forced list formals preserve member identity after caller removal", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  read <- quote((function(prior) {
    carrier <- list(read = function() sum(prior))
    rm(prior)
    (function(value) {
      force(value)
      rm("carrier", envir = parent.frame())
      value$read()
    })(carrier)
  })(1))
  expect_carrier_dependencies(data, list(read))
})

test_that("inline list calls retain independent and local bindings", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  reads <- alist(
    (function(prior) {
      carrier <- list(read = function() sum(prior), safe = function() 1)
      rm(prior)
      (function(value) value$safe())(carrier)
    })(1),
    (function(prior) {
      carrier <- list(read = function() sum(prior))
      (function(value) value$read())(carrier)
    })(1),
    (function(prior) {
      carrier <- (function() list(read = function() sum(prior)))()
      carrier$read()
    })(1),
    (function(prior) {
      carrier <- (function(prior) {
        list(outer = list(read = function() sum(prior)))
      })(1)
      rm(prior)
      carrier$outer$read()
    })(1)
  )
  expect_share_dependency_values(data, reads, expected = 1)
})

test_that("inline list planning does not run caller code", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  probe <- rlang::env(built = 0L, invoked = 0L, forced = 0L)
  read <- rlang::quo((function(prior) {
    carrier <- (function() {
      probe$built <- probe$built + 1L
      list(read = function() {
        probe$invoked <- probe$invoked + 1L
        sum(prior)
      })
    })()
    rm(prior)
    (function(value) value$read())(carrier)
  })({
    probe$forced <- probe$forced + 1L
    1
  }))
  expect_carrier_dependencies(data, list(read))
  expect_identical(probe$built, 0L)
  expect_identical(probe$invoked, 0L)
  expect_identical(probe$forced, 0L)
})
