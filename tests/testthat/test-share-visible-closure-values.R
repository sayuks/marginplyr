visible_closure_data <- function() {
  data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
}

visible_closure_reads <- function() {
  alist(
    (function(prior) {
      copied <- switch("read", read = function() sum(prior))
      rm(prior)
      copied()
    })(1),
    (function(prior) {
      copied <- (function() {
        switch("read", safe = function() 7,
               read = return(function() sum(prior)))
        function() 7
      })()
      rm(prior)
      copied()
    })(1),
    (function(prior) {
      copied <- (function() {
        if (TRUE) return(function() sum(prior))
        function() 7
      })()
      rm(prior)
      copied()
    })(1),
    (function(prior) {
      copied <- (function() {
        if (FALSE) {
          function() 7
        } else {
          returned <- function() sum(prior)
          if (TRUE) return(returned)
        }
        function() 7
      })()
      rm(prior)
      copied()
    })(1),
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

expect_visible_closure_values <- function(data, reads, expected = c(1, 1, 1)) {
  for (kind in c("parent", "total")) {
    helper <- rlang::call2(paste0("share_of_", kind), quote(value))
    for (read in reads) {
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
      expect_equal(forward$observed, expected)
      expect_equal(forward$prior, c(0.25, 0.75, 1))
      expect_equal(reverse$value, expected)
      expect_equal(reverse$ratio, c(1, 1, 1))
    }
  }
}

expect_visible_closures <- function(data) {
  before <- serialize(data, NULL)
  expect_share_dependencies(data, visible_closure_reads())
  expect_visible_closure_values(
    data, lapply(visible_closure_reads(), visible_closure_retained)
  )
  expect_visible_closure_values(data, alist(
    (function(prior) {
      copied <- (function() {
        # The explicit return retains a binding made inside its argument.
        return({ # nolint: return_linter.
          prior <- 7
          function() sum(prior)
        })
      })()
      rm(prior)
      copied()
    })(1),
    (function(prior) {
      copied <- switch("safe", read = function() sum(prior), function() 7)
      rm(prior)
      copied()
    })(1),
    (function(prior) {
      copied <- switch("safe", read = function() sum(prior),
                       safe = function() 7)
      rm(prior)
      copied()
    })(1),
    (function(prior) {
      copied <- (function(prior) {
        switch("read", read = function() sum(prior))
      })(7)
      rm(prior)
      copied()
    })(1),
    (function(prior) {
      copied <- (function() {
        if (TRUE) return(function() 7)
        function() sum(prior)
      })()
      rm(prior)
      copied()
    })(1),
    (function(prior) {
      copied <- (function(prior) {
        if (TRUE) return(function() sum(prior))
        function() 100
      })(7)
      rm(prior)
      copied()
    })(1)
  ), expected = c(7, 7, 7))
  expect_visible_closure_values(data, alist(
    (function(prior) {
      copied <- (function() {
        if (TRUE) return(function() sum(prior))
        rm(prior, envir = parent.frame())
        function() 7
      })()
      copied()
    })(1)
  ))
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
      if (TRUE) return(function() sum(prior))
      function() 7
    }
    copied <- factory()
    rm(prior)
    copied()
  })(1))
  expect_share_dependencies(visible_closure_data(), list(read))
  expect_identical(probe$calls, 0L)
})

test_that("selected blocks and branches retain closure list members", {
  carriers <- alist(
    { list(callback, safe) },
    {
      local_callback <- callback
      list(local_callback, safe)
    },
    if (TRUE) list(callback, safe) else list(safe, callback),
    switch("read", read = list(callback, safe), list(safe, callback))
  )
  reads <- function(remove = quote(rm(prior)), key = 1L) {
    lapply(carriers, function(carrier) {
      substitute((function(prior) {
        callback <- function() sum(prior)
        safe <- function() 7
        functions <- CARRIER
        REMOVE
        functions[[KEY]]()
      })(1), list(CARRIER = carrier, REMOVE = remove, KEY = key))
    })
  }
  data <- visible_closure_data()
  expect_share_dependency_values(data, reads(remove = NULL), expected = 1)
  expect_share_dependency_values(data, reads(key = 2L), expected = 7)
  expect_share_dependencies(data, reads())
})

test_that("closure list branch planning preserves ordinary switch lookup", {
  probe <- rlang::env(calls = 0L)
  record <- function() {
    probe$calls <- probe$calls + 1L
  }
  caller <- rlang::env(record = record)
  read <- rlang::new_quosure(quote((function(prior) {
    callback <- function() sum(prior)
    functions <- switch("read", read = {
      record()
      list(callback)
    })
    rm(prior)
    functions[[1L]]()
  })(1)), caller)

  data <- visible_closure_data()
  expect_share_dependencies(data, list(read))
  expect_identical(probe$calls, 0L)

  caller$switch <- function(...) list(function() 7)
  expect_share_dependency_values(data, list(read), expected = 7)
  expect_identical(probe$calls, 0L)
})

test_that("an explicit return from a block bypasses the closure list binding", {
  read <- quote(length((function(prior) {
    functions <- {
      if (TRUE) return(list(function() sum(prior)))
      list(function() 7)
    }
    rm(prior)
    functions[[1L]]()
  })(1)))
  expect_share_dependency_values(
    visible_closure_data(), list(read), expected = 1
  )
})

remapped_closure_read <- function(carrier, invoke, remove = quote(rm(prior)),
                                  callback = quote(function() sum(prior))) {
  substitute((function(prior) {
    callback <- CALLBACK
    safe <- function() 7
    functions <- CARRIER
    REMOVE
    INVOKE
  })(1), list(CARRIER = carrier, INVOKE = invoke, REMOVE = remove,
              CALLBACK = callback))
}

test_that("reversing a plain list preserves the selected closure", {
  data <- visible_closure_data()
  carrier <- quote(rev(list(safe, callback)))
  read <- function(key, remove = quote(rm(prior))) {
    remapped_closure_read(carrier, bquote(functions[[.(key)]]()), remove)
  }
  expect_share_dependencies(data, list(read(1L)))
  expect_share_dependency_values(data, list(read(1L, NULL)), expected = 1)
  expect_share_dependency_values(data, list(read(2L)), expected = 7)
})

test_that("plain list naming preserves the selected closure", {
  data <- visible_closure_data()
  cases <- list(
    alist(unname(list(a = callback, b = safe), force = TRUE),
          functions[[1L]](), functions[[2L]]()),
    alist(structure(list(a = callback, b = safe), names = c("x", "y")),
          functions$x(), functions$y()),
    alist(stats::setNames(list(callback, safe), c("x", "y")),
          functions$x(), functions$y())
  )
  reads <- function(member, ...) {
    lapply(cases, function(case) {
      remapped_closure_read(case[[1L]], case[[member]], ...)
    })
  }
  expect_share_dependency_values(data, reads(2L, remove = NULL), expected = 1)
  expect_share_dependency_values(data, reads(3L), expected = 7)
  expect_share_dependencies(data, reads(2L))
})

test_that("repeating, converting, and slicing a list preserves closures", {
  data <- visible_closure_data()
  cases <- list(
    alist(rep(list(safe, callback), times = 2L), functions[[4L]](),
          functions[[3L]]()),
    alist(rep(list(callback, safe)), functions[[1L]](), functions[[2L]]()),
    alist(as.list(list(callback, safe)), functions[[1L]](), functions[[2L]]()),
    alist(utils::tail(list(safe, callback, safe), n = -1L), functions[[1L]](),
          functions[[2L]]())
  )
  reads <- function(member, ...) {
    lapply(cases, function(case) {
      remapped_closure_read(case[[1L]], case[[member]], ...)
    })
  }
  expect_share_dependency_values(data, reads(2L, remove = NULL), expected = 1)
  expect_share_dependency_values(data, reads(3L), expected = 7)
  independent <- reads(2L, callback = quote(function() 9))
  expect_share_dependency_values(data, independent, expected = 9)
  expect_share_dependencies(data, reads(2L))
})

test_that("plain list remapping preserves caller lookup and S3 dispatch", {
  caller <- rlang::env()
  data <- visible_closure_data()
  bare <- rlang::new_quosure(remapped_closure_read(
    quote(rev(list(safe, callback))), quote(functions[[1L]]())
  ), caller)
  qualified <- rlang::new_quosure(remapped_closure_read(
    quote(base::rev(list(safe, callback))), quote(functions[[1L]]())
  ), caller)

  caller$rev <- function(x) x
  expect_share_dependency_values(data, list(bare), expected = 7)
  expect_share_dependencies(data, list(qualified))

  caller$rev.list <- function(x) x
  expect_share_dependency_values(data, list(qualified), expected = 7)

  caller$rev.marginplyr_test_list <- function(x) x
  classed <- rlang::new_quosure(remapped_closure_read(
    quote(base::rev(structure(list(safe, callback),
                              class = "marginplyr_test_list"))),
    quote(functions[[1L]]())
  ), caller)
  expect_share_dependency_values(data, list(classed), expected = 7)
})

test_that("planning a list remapping leaves its elements unevaluated", {
  probe <- rlang::env(calls = 0L)
  record <- function() {
    probe$calls <- probe$calls + 1L
  }
  read <- remapped_closure_read(
    quote(base::rev(list(safe, {
      record()
      callback
    }))),
    quote(functions[[1L]]())
  )
  expect_share_dependencies(visible_closure_data(), list(read))
  expect_identical(probe$calls, 0L)
})

test_that("remapping leaves unselected inline closures unevaluated", {
  read <- quote((function(prior) {
    rm(prior)
    rev(list(function() sum(prior), function() 7))[[1L]]()
  })(1))
  expect_share_dependency_values(visible_closure_data(), list(read), 7)
})

test_that("partial names on list remapping do not warn during planning", {
  rlang::local_options(warnPartialMatchArgs = TRUE)
  read <- remapped_closure_read(
    quote(unname(ob = list(callback))), quote(functions[[1L]]())
  )
  expect_no_warning(
    expect_share_dependencies(visible_closure_data(), list(read))
  )
})

test_that("computed list transfer heads use caller value lookup", {
  remapping <- rlang::new_quosure(quote(tryCatch(
    (function(prior) {
      callback <- function() sum(prior)
      callbacks <- (rev)(list(function() 7, callback))
      rm(prior)
      callbacks[[1L]]()
    })(1),
    error = function(cnd) 7
  )), rlang::env(rev = 0L))
  branch <- rlang::new_quosure(quote(tryCatch(
    (function(prior) {
      callback <- function() sum(prior)
      callbacks <- (switch)("read", read = list(callback))
      rm(prior)
      callbacks[[1L]]()
    })(1),
    error = function(cnd) 7
  )), rlang::env(switch = 0L))
  expect_share_dependency_values(
    visible_closure_data(), list(remapping, branch), expected = 7
  )
})
