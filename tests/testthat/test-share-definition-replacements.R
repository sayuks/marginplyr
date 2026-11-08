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
