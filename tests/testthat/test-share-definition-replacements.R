definition_body_reads <- function() {
  as.list(quote(alist({
    f <- function() 7
    body(f) <- quote(sum(prior))
    f()
  })))[-1L]
}

test_that("literal body replacement refreshes closure dependencies", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  expect_share_dependencies(data, definition_body_reads())
})

definition_formal_reads <- function() {
  as.list(quote(alist(
    {
      f <- function(prior) sum(prior)
      formals(f)$prior <- NULL
      f()
    },
    {
      f <- function(arg = 7) arg
      formals(f)$arg <- quote(sum(prior))
      f()
    },
    {
      f <- function(arg = 7) arg
      formals(f)[["arg"]] <- quote(sum(prior))
      f()
    }
  )))[-1L]
}

test_that("formal replacements refresh closure dependencies", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  expect_share_dependencies(data, definition_formal_reads())
})

definition_explicit_reads <- function() {
  as.list(quote(alist(
    {
      f <- function() 7
      f <- base::`body<-`(f, value = quote(sum(prior)))
      f()
    },
    {
      f <- function(prior) sum(prior)
      f <- base::`formals<-`(f, value = NULL)
      f()
    },
    {
      f <- function(arg = 7) arg
      f <- base::`formals<-`(f, value = alist(arg = sum(prior)))
      f()
    }
  )))[-1L]
}

test_that("explicit base definition setters retain replacement dependencies", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  expect_share_dependencies(data, definition_explicit_reads())
})

definition_controls <- function() {
  as.list(quote(alist(
    (function(prior) {
      f <- function() sum(prior)
      body(f) <- quote(11)
      rm(prior)
      f()
    })(1),
    (function(prior) {
      f <- function() sum(prior)
      formals(f) <- alist(prior = 11)
      rm(prior)
      f()
    })(1),
    (function(prior) {
      f <- function() sum(prior)
      formals(f)$prior <- 11
      rm(prior)
      f()
    })(1),
    (function(prior) {
      f <- function() sum(prior)
      f <- base::`body<-`(f, value = quote(11))
      rm(prior)
      f()
    })(1),
    (function(prior) {
      f <- function() sum(prior)
      f <- base::`formals<-`(f, value = list(prior = 11))
      rm(prior)
      f()
    })(1),
    {
      f <- function() 11
      f()
    }
  )))[-1L]
}

test_that("proven independent definitions replace obsolete dependencies", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  expect_share_dependency_values(data, definition_controls(), 11)
})

definition_replacement_reads <- function() {
  c(definition_body_reads(), definition_formal_reads(),
    definition_explicit_reads())
}

expect_definition_replacements <- function(data) {
  expect_share_dependencies(data, definition_replacement_reads())
  expect_share_dependencies(data, list(quote({
    f <- function() sum(prior)
    f()
  })))
  expect_share_dependency_values(data, definition_controls(), 11)
  for (read in definition_replacement_reads()) {
    ordinary <- summarize_with_margins(
      data, prior = sum(.data$x), observed = !!read, .grouping = rollup("g")
    )
    expect_equal(ordinary$observed, c(1, 3, 4))
  }
}

test_that("definition replacements preserve eager values and input", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  for (input in list(data, tibble::as_tibble(data))) {
    before <- serialize(input, NULL)
    expect_definition_replacements(input)
    expect_identical(serialize(input, NULL), before)
  }
})

test_that("data.table definition replacements preserve eager input", {
  skip_if_suggest_absent("data.table")
  data <- data.table::data.table(
    g = c("a", "b"), x = c(1, 3), prior = c(10, 20)
  )
  before <- data.table::copy(data)
  expect_definition_replacements(data)
  expect_equal(data, before)
})

test_that("definition replacement refusal leaves lazy input unread", {
  skip_if_suggest_absent("dtplyr")
  probe <- rlang::env(calls = 0L)
  observe <- function(x) {
    probe$calls <- probe$calls + 1L
    x
  }
  data <- dtplyr::lazy_dt(data.frame(g = c("a", "b"), x = c(1, 3)))
  data <- dplyr::filter(data, observe(x) > 0)
  expect_share_dependencies(data, definition_replacement_reads())
  expect_identical(probe$calls, 0L)
  expect_equal(dplyr::collect(data)$x, c(1, 3))
  expect_identical(probe$calls, 1L)
})

test_that("definition replacement planning leaves caller code unexecuted", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  probe <- rlang::env(written = 0L, invoked = 0L)
  read <- rlang::quo({
    f <- function() 7
    body(f) <- {
      probe$written <- probe$written + 1L
      quote({
        probe$invoked <- probe$invoked + 1L
        sum(prior)
      })
    }
    f()
  })
  expect_share_dependencies(data, list(read))
  expect_identical(probe$written, 0L)
  expect_identical(probe$invoked, 0L)
})

test_that("definition setters follow ordinary function lookup", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  expect_share_dependency_values(data, alist(
    {
      `body<-` <- function(fun, value) fun
      f <- function() 11
      body(f) <- quote(sum(prior))
      f()
    },
    {
      `formals<-` <- function(fun, value) fun
      f <- function(prior = 11) sum(prior)
      formals(f)$prior <- NULL
      f()
    }
  ), 11)
  expect_share_dependencies(data, alist(
    {
      `body<-` <- function(fun, value) fun
      f <- function() 7
      f <- base::`body<-`(f, value = quote(sum(prior)))
      f()
    },
    {
      `formals<-` <- 0
      f <- function(prior) sum(prior)
      formals(f)$prior <- NULL
      f()
    }
  ))
})

test_that("unproven nested definition writes cannot reuse the old proof", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  expect_share_dependencies(data, alist({
    f <- function(prior) sum(prior)
    formals(f)["prior"] <- NULL
    f()
  }))
})

test_that("ordinary search-path body setters retain share dependencies", {
  calls <- alist(
    marginplyr::summarize_with_margins(
      data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20)),
      source = sum(x), prior = marginplyr::share_of_parent(source),
      observed = {
        f <- function() 7
        body(f) <- quote(sum(prior))
        f()
      }, .grouping = marginplyr::rollup("g")
    ),
    marginplyr::summarize_with_margins(
      data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20)),
      source = sum(x), prior = marginplyr::share_of_total(source),
      observed = {
        f <- function() 7
        body(f) <- quote(sum(prior))
        f()
      }, .grouping = marginplyr::rollup("g")
    ),
    marginplyr::summarize_with_margins(
      data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20)),
      prior = sum(x), observed = {
        f <- function() 7
        body(f) <- quote(sum(prior))
        f()
      }, ratio = marginplyr::share_of_parent(observed),
      .grouping = marginplyr::rollup("g")
    ),
    marginplyr::summarize_with_margins(
      data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20)),
      prior = sum(x), observed = {
        f <- function() 7
        body(f) <- quote(sum(prior))
        f()
      }, ratio = marginplyr::share_of_total(observed),
      .grouping = marginplyr::rollup("g")
    )
  )
  # The global caller reaches the attached methods setter in an ordinary R
  # session; the test namespace would instead reach base through its imports.
  for (call in calls) {
    expect_error(eval(call, globalenv()), class = "marginplyr_error")
  }
})

definition_callback_reads <- function() {
  reads <- as.list(quote(alist(
    {
      f <- function(ignore) 7
      f <- base::`body<-`(f, value = {
        quote(sum(prior))
      })
      lapply(list(NULL), f)[[1L]]
    },
    {
      f <- function(ignore) 7
      f <- base::`body<-`(f, value = {
        quote(sum(prior))
      })
      vapply(1, f, numeric(1))
    },
    {
      f <- function(ignore) 7
      f <- base::`body<-`(f, envir = environment(), value = quote(sum(prior)))
      lapply(list(NULL), f)[[1L]]
    },
    {
      f <- function(ignore) 7
      f <- base::`body<-`(f, envir = environment(), value = quote(sum(prior)))
      vapply(1, f, numeric(1))
    },
    {
      f <- function(ignore, arg = 7) arg
      f <- base::`formals<-`(f, value = {
        alist(ignore = , arg = sum(prior))
      })
      lapply(list(NULL), f)[[1L]]
    },
    {
      f <- function(ignore, arg = 7) arg
      f <- base::`formals<-`(f, value = {
        alist(ignore = , arg = sum(prior))
      })
      vapply(1, f, numeric(1))
    },
    {
      f <- function(ignore, arg = 7) arg
      f <- base::`formals<-`(
        f, envir = environment(), value = alist(ignore = , arg = sum(prior))
      )
      lapply(list(NULL), f)[[1L]]
    },
    {
      f <- function(ignore, arg = 7) arg
      f <- base::`formals<-`(
        f, envir = environment(), value = alist(ignore = , arg = sum(prior))
      )
      vapply(1, f, numeric(1))
    },
    {
      f <- (function(prior) function(ignore) sum(prior))(7)
      environment(f) <- {
        environment()
      }
      lapply(list(NULL), f)[[1L]]
    },
    {
      f <- (function(prior) function(ignore) sum(prior))(7)
      environment(f) <- {
        environment()
      }
      vapply(1, f, numeric(1))
    }
  )))[-1L]
  lapply(reads, rlang::new_quosure, env = globalenv())
}

test_that("callbacks retain unresolved closure definition dependencies", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  before <- serialize(data, NULL)
  expect_share_dependencies(data, definition_callback_reads())
  expect_identical(serialize(data, NULL), before)
})

definition_stock_body_reads <- function() {
  reads <- as.list(quote(alist(
    {
      f <- function(ignore) 7
      body(f) <- quote(sum(prior))
      f(NULL)
    },
    {
      f <- function(ignore) 7
      f <- methods::`body<-`(f, value = quote(sum(prior)))
      f(NULL)
    },
    {
      f <- function(ignore) 7
      f <- base::`body<-`(f, value = quote(sum(prior)))
      f(NULL)
    },
    {
      f <- function(ignore) 7
      f <- methods::`body<-`(f, value = quote(sum(prior)))
      lapply(list(NULL), f)[[1L]]
    },
    {
      f <- function(ignore) 7
      f <- base::`body<-`(f, value = quote(sum(prior)))
      vapply(1, f, numeric(1))
    }
  )))[-1L]
  lapply(reads, rlang::new_quosure, env = globalenv())
}

test_that("stock methods and base body setters preserve public controls", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  before <- serialize(data, NULL)
  expect_share_dependencies(data, definition_stock_body_reads())
  controls <- lapply(c(definition_controls(), alist(
    (function(prior) {
      f <- function(ignore) sum(prior)
      f <- methods::`body<-`(f, value = quote(11))
      rm(prior)
      lapply(list(NULL), f)[[1L]]
    })(1),
    (function(prior) {
      f <- function(ignore) sum(prior)
      body(f) <- quote(11)
      rm(prior)
      vapply(1, f, numeric(1))
    })(1),
    (function(prior) {
      f <- function(ignore) sum(prior)
      f <- base::`body<-`(f, value = quote(11))
      rm(prior)
      lapply(list(NULL), f)[[1L]]
    })(1),
    {
      `body<-` <- function(fun, value) fun
      f <- function(ignore) 11
      body(f) <- quote(sum(prior))
      vapply(1, f, numeric(1))
    },
    (function(prior) {
      `body<-` <- 0
      f <- function(ignore) sum(prior)
      body(f) <- quote(11)
      rm(prior)
      f(NULL)
    })(1)
  )), rlang::new_quosure, env = globalenv())
  expect_share_dependency_values(data, controls, 11)
  for (read in c(definition_stock_body_reads(), definition_callback_reads())) {
    ordinary <- summarize_with_margins(
      data, prior = sum(.data$x), observed = !!read, .grouping = rollup("g")
    )
    expect_equal(ordinary$observed, c(1, 3, 4))
  }
  expect_identical(serialize(data, NULL), before)
})

test_that("unresolved callback planning never executes caller code", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  probe <- rlang::env(written = 0L, invoked = 0L)
  caller <- rlang::env(globalenv(), probe = probe)
  reads <- alist(
    {
      f <- function(ignore) 7
      body(f) <- {
        probe$written <- probe$written + 1L
        quote({
          probe$invoked <- probe$invoked + 1L
          sum(prior)
        })
      }
      lapply(list(NULL), f)[[1L]]
    },
    {
      f <- function(ignore) 7
      f <- methods::`body<-`(f, envir = environment(), value = {
        probe$written <- probe$written + 1L
        quote({
          probe$invoked <- probe$invoked + 1L
          sum(prior)
        })
      })
      vapply(1, f, numeric(1))
    },
    {
      f <- function(ignore) 7
      f <- base::`body<-`(f, value = {
        probe$written <- probe$written + 1L
        quote({
          probe$invoked <- probe$invoked + 1L
          sum(prior)
        })
      })
      vapply(1, f, numeric(1))
    }
  )
  reads <- lapply(reads, rlang::new_quosure, env = caller)
  expect_share_dependencies(data, reads)
  expect_identical(probe$written, 0L)
  expect_identical(probe$invoked, 0L)
})

test_that("stock setters and unresolved callbacks refuse before lazy reads", {
  skip_if_suggest_absent("dtplyr")
  probe <- rlang::env(calls = 0L)
  observe <- function(x) {
    probe$calls <- probe$calls + 1L
    x
  }
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  before <- serialize(data, NULL)
  lazy <- dtplyr::lazy_dt(data, immutable = TRUE)
  lazy <- dplyr::filter(lazy, observe(x) > 0)
  expect_share_dependencies(
    lazy, c(definition_stock_body_reads(), definition_callback_reads())
  )
  expect_identical(probe$calls, 0L)
  expect_identical(serialize(data, NULL), before)
  expect_equal(dplyr::collect(lazy)$x, c(1, 3))
  expect_identical(probe$calls, 1L)
  expect_identical(serialize(data, NULL), before)
})

test_that("a dispatched methods body setter cannot claim a base replacement", {
  registry <- new.env(parent = globalenv())
  signature <- c(fun = "function", envir = "missing", value = "call")
  probe <- rlang::env(calls = 0L)
  methods::setMethod("body<-", signature, function(fun, envir, value) {
    if (!identical(formals(fun), as.pairlist(alist(ignore = )))) {
      return(base::`body<-`(fun, envir = envir, value = value))
    }
    probe$calls <- probe$calls + 1L
    base::`body<-`(fun, envir = envir, value = quote(sum(prior)))
  }, where = registry)
  on.exit(methods::removeMethod("body<-", signature, where = registry),
          add = TRUE)
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  read <- rlang::new_quosure(quote({
    f <- function(ignore) 7
    f <- methods::`body<-`(f, value = quote(sum(11)))
    vapply(1, f, numeric(1))
  }), globalenv())
  expect_share_dependencies(data, list(read))
  expect_identical(probe$calls, 0L)
  ordinary <- summarize_with_margins(
    data, prior = sum(.data$x), observed = !!read, .grouping = rollup("g")
  )
  expect_equal(ordinary$observed, c(1, 3, 4))
  expect_identical(probe$calls, 3L)
})
