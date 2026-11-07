nested_closure_list_data <- function() {
  data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
}

nested_closure_list_reads <- function(body = quote(sum(prior)),
                                      remove = quote(rm(prior))) {
  paths <- as.list(quote(alist(
    carrier$outer$read(),
    carrier[[1L]][[1L]](),
    carrier[[c("outer", "read")]](),
    carrier[[base::c(1L, 1L)]](),
    {
      inner <- carrier$outer
      inner$read()
    },
    {
      copied <- carrier[[1L]][[1L]]
      copied()
    }
  )))[-1L]
  lapply(paths, function(path) {
    substitute((function(prior) {
      inner <- list(read = function() BODY)
      carrier <- list(outer = inner)
      REMOVE
      PATH
    })(1), list(BODY = body, REMOVE = remove, PATH = path))
  })
}

test_that("nested literal named members retain invocation dependencies", {
  read <- quote((function(prior) {
    carrier <- list(outer = list(read = function() sum(prior)))
    rm(prior)
    carrier$outer$read()
  })(1))
  expect_share_dependencies(nested_closure_list_data(), list(read))
})

test_that("nested recursive literal indices retain invocation dependencies", {
  reads <- alist(
    (function(prior) {
      carrier <- list(outer = list(read = function() sum(prior)))
      rm(prior)
      carrier[[c("outer", "read")]]()
    })(1),
    (function(prior) {
      carrier <- list(outer = list(read = function() sum(prior)))
      rm(prior)
      carrier[[c(1L, 1L)]]()
    })(1)
  )
  expect_share_dependencies(nested_closure_list_data(), reads)
})

expect_nested_closure_lists <- function(data) {
  before <- serialize(data, NULL)
  expect_share_dependencies(data, nested_closure_list_reads())
  expect_share_dependency_values(
    data, nested_closure_list_reads(remove = quote(NULL)), expected = 1
  )
  expect_share_dependency_values(
    data, nested_closure_list_reads(body = quote(7)), expected = 7
  )
  expect_identical(serialize(data, NULL), before)
}

test_that("nested list paths and extracted bindings preserve eager behavior", {
  expect_nested_closure_lists(nested_closure_list_data())
  expect_nested_closure_lists(tibble::as_tibble(nested_closure_list_data()))
})

test_that("data.table nested lists preserve invocation dependencies", {
  skip_if_suggest_absent("data.table")
  expect_nested_closure_lists(data.table::as.data.table(
    nested_closure_list_data()
  ))
})

test_that("nested closure lists preserve lexical frames across extra scopes", {
  reads <- alist(
    (function(prior) {
      inner <- list(read = function() sum(prior))
      middle <- list(inner)
      outer <- list(middle)
      rm(prior)
      (function() {
        copied <- outer[[1L]][[1L]]
        copied[[1L]]()
      })()
    })(1),
    (function(prior) {
      carrier <- list(list((function() function() sum(prior))()))
      rm(prior)
      carrier[[c(1L, 1L)]]()
    })(1)
  )
  expect_share_dependencies(nested_closure_list_data(), reads)
})

test_that("nested list planning never invokes or forces caller code", {
  data <- nested_closure_list_data()
  before <- serialize(data, NULL)
  probe <- rlang::env(calls = 0L, forced = 0L)
  read <- quote((function(prior) {
    carrier <- list(list(function() {
      probe$calls <- probe$calls + 1L
      sum(prior)
    }))
    rm(prior)
    carrier[[c(1L, 1L)]]()
  })({
    probe$forced <- probe$forced + 1L
    1
  }))
  expect_share_dependencies(data, list(read))
  expect_identical(probe$calls, 0L)
  expect_identical(probe$forced, 0L)
  expect_identical(serialize(data, NULL), before)
})

test_that("unknown nested-list returns and unselected closures remain legal", {
  probe <- rlang::env(calls = 0L)
  unknown <- function(x) {
    probe$calls <- probe$calls + 1L
    list(outer = list(read = function() 7))
  }
  read <- rlang::quo((function(prior) {
    carrier <- unknown(list(outer = list(read = function() sum(prior))))
    rm(prior)
    carrier$outer$read()
  })(1))
  expect_share_dependency_values(nested_closure_list_data(), list(read), 7)
  expect_identical(probe$calls, 12L)
  reads <- alist(
    (function(prior) {
      carrier <- list(outer = list(
        dependent = function() sum(prior), read = function() 7
      ))
      rm(prior)
      carrier$outer$read()
    })(1),
    (function(prior) {
      carrier <- list(list(function() 7, function() sum(prior)))
      rm(prior)
      carrier[[c(1L, 1L)]]()
    })(1)
  )
  expect_share_dependency_values(nested_closure_list_data(), reads, 7)
})

test_that("recursive list paths preserve ordinary caller function lookup", {
  probe <- rlang::env(calls = 0L)
  c <- function(...) {
    probe$calls <- probe$calls + 1L
    base::c(1L, 2L)
  }
  read <- rlang::quo((function(prior) {
    carrier <- list(list(function() sum(prior), function() 7))
    rm(prior)
    carrier[[c(1L, 1L)]]()
  })(1))
  expect_share_dependency_values(nested_closure_list_data(), list(read), 7)
  expect_identical(probe$calls, 12L)
})

test_that("empty literal names do not select unnamed nested closures", {
  read <- quote((function(prior) {
    carrier <- list(outer = list(function() sum(prior), safe = function() 7))
    rm(prior)
    selected <- carrier[[c("outer", "")]]
    if (is.null(selected)) 7 else selected()
  })(1))
  expect_share_dependency_values(nested_closure_list_data(), list(read), 7)
})

test_that("dollar members preserve invalid subscript diagnostics", {
  data <- nested_closure_list_data()
  error <- expect_error(
    summarize_with_margins(
      data, value = sum(.data$x), prior = share_of_total(value),
      observed = (function(prior) {
        carrier <- list(outer = list(read = function() sum(prior)))
        rm(prior)
        base::`$`(carrier, c("outer"))$read()
      })(1),
      .grouping = rollup("g")
    ),
    "invalid subscript type"
  )
  expect_false(inherits(error, "marginplyr_error"))
})

test_that("nested closure dependency refusal leaves lazy input unread", {
  skip_if_suggest_absent("dtplyr")
  probe <- rlang::env(calls = 0L)
  observe <- function(x) {
    probe$calls <- probe$calls + 1L
    x
  }
  data <- dtplyr::lazy_dt(nested_closure_list_data())
  data <- dplyr::filter(data, observe(x) > 0)
  expect_share_dependencies(data, nested_closure_list_reads())
  expect_identical(probe$calls, 0L)
  expect_equal(dplyr::collect(data)$x, c(1, 3))
  expect_identical(probe$calls, 1L)
})

test_that("fractional nested indices select the same closures as R", {
  reads <- alist(
    (function(prior) {
      carrier <- list(list(function() sum(prior)))
      rm(prior)
      carrier[[c(1.9, 1.9)]]()
    })(1),
    (function(prior) {
      carrier <- list(list(function() 7),
                      list(function() 7, function() sum(prior)))
      rm(prior)
      carrier[[c(2.9, 2.9)]]()
    })(1)
  )
  data <- nested_closure_list_data()
  expect_identical(lapply(reads, eval, envir = data), list(400, 400))
  expect_share_dependencies(data, reads)
})
