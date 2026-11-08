enclosure_mask_reads <- function() {
  alist((function(prior) {
    f <- function() sum(prior)
    environment(f) <- parent.env(environment())
    f()
  })(7), (function(prior) {
    f <- function() sum(prior)
    f <- base::`environment<-`(f, value = parent.env(environment()))
    f()
  })(7), {
    f <- (function(prior) function() sum(prior))(7)
    environment(f) <- environment()
    f()
  }, (function(prior) {
    f <- function() 7
    body(f) <- quote(sum(prior))
    environment(f) <- parent.env(environment())
    f()
  })(7))
}

test_that("enclosure replacement exposes eager share dependencies", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  expect_share_dependencies(data, enclosure_mask_reads())
})

test_that("unchanged closure enclosures preserve independent values", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  expect_share_dependency_values(data, alist((function(prior) {
    f <- function() sum(prior)
    environment(f) <- environment(f)
    f()
  })(7)), 7)
})

test_that("independent replacement discards the obsolete captured frame", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  outside <- list2env(list(prior = 11), parent = baseenv())
  expect_share_dependency_values(data, list(rlang::quo((function(prior) {
    f <- function() sum(prior)
    environment(f) <- outside
    rm(prior)
    f()
  })(7))), 11)
})

enclosure_control_reads <- function() {
  alist((function(prior) {
    f <- function() sum(prior)
    f()
  })(7), (function(prior) {
    f <- function() sum(prior)
    environment(f) <- environment()
    f()
  })(7), (function(prior) {
    f <- function() sum(prior)
    environment(f) <- environment(f)
    f()
  })(7), {
    `environment<-` <- function(fun, value) fun
    f <- function() 7
    environment(f) <- environment()
    f()
  }, (function(prior) {
    f <- function() sum(prior)
    unchanged <- f
    environment(f) <- parent.env(environment())
    unchanged()
  })(7))
}

test_that("enclosure setters preserve ordinary lookup and direct refusals", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  expect_share_dependency_values(data, enclosure_control_reads(), 7)
  expect_share_dependencies(data, alist({
    f <- function() sum(prior)
    f()
  }, (function(prior) {
    `environment<-` <- 0
    f <- function() sum(prior)
    environment(f) <- parent.env(environment())
    f()
  })(7), (function(prior) {
    `environment<-` <- function(fun, value) fun
    f <- function() sum(prior)
    f <- base::`environment<-`(f, value = parent.env(environment()))
    f()
  })(7)))
})

test_that("enclosure changes preserve ordinary summaries and eager input", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  before <- serialize(data, NULL)
  for (read in enclosure_mask_reads()) {
    result <- summarize_with_margins(
      data, prior = sum(.data$x), observed = !!read, .grouping = rollup("g")
    )
    expect_equal(result$observed, c(1, 3, 4))
  }
  expect_identical(serialize(data, NULL), before)
})

test_that("enclosure replacement refuses lazy dependencies before reading", {
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
  expect_share_dependencies(lazy, enclosure_mask_reads())
  expect_share_dependencies(lazy, alist({
    f <- function() sum(prior)
    f()
  }))
  expect_identical(probe$calls, 0L)
  expect_identical(serialize(data, NULL), before)
  expect_equal(dplyr::collect(lazy)$x, c(1, 3))
  expect_identical(probe$calls, 1L)
})

test_that("lazy enclosure controls retain independent and ordinary values", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  before <- serialize(data, NULL)
  lazy <- dtplyr::lazy_dt(data, immutable = TRUE)
  outside <- list2env(list(prior = 11), parent = baseenv())
  independent <- rlang::quo((function(prior) {
    f <- function() sum(prior)
    environment(f) <- outside
    rm(prior)
    f()
  })(7))
  for (kind in c("parent", "total")) {
    for (read in c(enclosure_control_reads()[1:3], list(independent))) {
      expected <- if (rlang::is_quosure(read)) 11 else 7
      share <- rlang::call2(paste0("share_of_", kind), quote(value))
      forward <- summarize_with_margins(
        lazy, value = sum(.data$x), prior = !!share, observed = !!read,
        .grouping = rollup("g")
      )
      forward <- dplyr::collect(forward)
      expect_equal(forward$observed, rep(expected, 3L))
      expect_equal(forward$prior, c(0.25, 0.75, 1))
      reverse <- summarize_with_margins(
        lazy, prior = sum(.data$x), value = !!read, ratio = !!share,
        .grouping = rollup("g")
      )
      reverse <- dplyr::collect(reverse)
      expect_equal(reverse$value, rep(expected, 3L))
      expect_equal(reverse$ratio, c(1, 1, 1))
    }
  }
  for (read in enclosure_mask_reads()) {
    ordinary <- summarize_with_margins(
      lazy, prior = sum(.data$x), observed = !!read, .grouping = rollup("g")
    )
    expect_equal(dplyr::collect(ordinary)$observed, c(10, 20, 30))
  }
  expect_identical(serialize(data, NULL), before)
})

test_that("enclosure replacement planning never executes caller code", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  probe <- rlang::env(written = 0L, invoked = 0L)
  read <- rlang::quo((function(prior) {
    f <- function() {
      probe$invoked <- probe$invoked + 1L
      sum(prior)
    }
    environment(f) <- {
      probe$written <- probe$written + 1L
      parent.env(environment())
    }
    f()
  })(7))
  expect_share_dependencies(data, list(read))
  expect_identical(probe$written, 0L)
  expect_identical(probe$invoked, 0L)
  target <- function() {
    probe$written <- probe$written + 1L
    parent.frame()
  }
  opaque <- rlang::quo((function(prior) {
    f <- function() sum(prior)
    environment(f) <- target()
    f()
  })(7))
  expect_share_dependencies(data, list(opaque))
  expect_identical(probe$written, 0L)
})

test_that("an active enclosure binding is never forced to prove independence", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  probe <- rlang::env(calls = 0L)
  outside <- new.env(parent = baseenv())
  makeActiveBinding("prior", function() {
    probe$calls <- probe$calls + 1L
    11
  }, outside)
  read <- rlang::quo((function(prior) {
    f <- function() sum(prior)
    environment(f) <- outside
    f()
  })(7))
  expect_share_dependencies(data, list(read))
  expect_identical(probe$calls, 0L)
})
