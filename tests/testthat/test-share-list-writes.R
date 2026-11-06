list_write_data <- function() {
  data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
}

list_write_reads <- function() {
  alist(
    (function(prior) {
      carrier <- list()
      carrier[[1L]] <- function() sum(prior)
      rm(prior)
      carrier[[1L]]()
    })(1),
    (function(prior) {
      carrier <- list(function() 7)
      carrier[[1L]] <- function() sum(prior)
      rm(prior)
      carrier[[1L]]()
    })(1),
    (function(prior) {
      carrier <- list()
      carrier$read <- function() sum(prior)
      rm(prior)
      carrier$read()
    })(1)
  )
}


list_write_finish <- function(result) {
  if (is.data.frame(result)) result else dplyr::collect(result)
}

expect_list_write_refusals <- function(data, reads = list_write_reads()) {
  for (kind in c("parent", "total")) {
    for (read in reads) {
      helper <- rlang::call2(paste0("share_of_", kind), quote(value))
      expect_error(
        list_write_finish(summarize_with_margins(
          data, value = sum(.data$x), prior = !!helper, observed = !!read,
          .grouping = rollup("g")
        )),
        "Ordinary summaries cannot use an earlier", class = "marginplyr_error"
      )
      expect_error(
        list_write_finish(summarize_with_margins(
          data, prior = sum(.data$x), value = !!read, ratio = !!helper,
          .grouping = rollup("g")
        )),
        "depends on earlier summary alias", class = "marginplyr_error"
      )
    }
  }
}

list_write_controls <- function() {
  alist(
    (function(prior) {
      carrier <- list()
      carrier[[1L]] <- function() sum(prior)
      carrier[[1L]]()
    })(1),
    (function(prior) {
      carrier <- list(function() 7)
      carrier[[1L]] <- function() sum(prior)
      carrier[[1L]]()
    })(1),
    (function(prior) {
      carrier <- list()
      carrier$read <- function() sum(prior)
      carrier$read()
    })(1),
    (function(prior) {
      carrier <- list(function() sum(prior))
      carrier[[1L]] <- function() 7
      rm(prior)
      carrier[[1L]]()
    })(1),
    (function(prior) {
      carrier <- list(read = function() sum(prior))
      carrier$read <- function() 7
      rm(prior)
      carrier$read()
    })(1)
  )
}

expect_list_write_values <- function(data) {
  reads <- list_write_controls()
  expected <- c(1, 1, 1, 7, 7)
  for (kind in c("parent", "total")) {
    helper <- rlang::call2(paste0("share_of_", kind), quote(value))
    for (index in seq_along(reads)) {
      read <- reads[[index]]
      forward <- list_write_finish(summarize_with_margins(
        data, value = sum(.data$x), prior = !!helper, observed = !!read,
        .grouping = rollup("g")
      ))
      reverse <- list_write_finish(summarize_with_margins(
        data, prior = sum(.data$x), value = !!read, ratio = !!helper,
        .grouping = rollup("g")
      ))
      expect_equal(forward$observed, rep(expected[[index]], 3L))
      expect_equal(forward$prior, c(0.25, 0.75, 1))
      expect_equal(reverse$value, rep(expected[[index]], 3L))
      expect_equal(reverse$ratio, c(1, 1, 1))
    }
  }
}

expect_list_writes <- function(data) {
  before <- serialize(data, NULL)
  expect_list_write_refusals(data)
  expect_list_write_refusals(data, alist((function(prior) {
    carrier <- list(function() sum(prior))
    rm(prior)
    carrier[[1L]]()
  })(1)))
  expect_list_write_values(data)
  expect_identical(serialize(data, NULL), before)
}

test_that("literal list writes retain eager invocation dependencies", {
  expect_list_writes(list_write_data())
  expect_list_writes(tibble::as_tibble(list_write_data()))
})

test_that("data.table literal list writes retain invocation dependencies", {
  skip_if_suggest_absent("data.table")
  expect_list_writes(data.table::as.data.table(list_write_data()))
})

test_that("collected dtplyr list writes retain invocation dependencies", {
  skip_if_suggest_absent("dtplyr")
  expect_list_writes(dtplyr::lazy_dt(list_write_data()))
})

test_that("list write analysis never invokes installed caller closures", {
  probe <- rlang::env(calls = 0L)
  read <- quote((function(prior) {
    carrier <- list()
    carrier[[1L]] <- function() {
      probe$calls <- probe$calls + 1L
      sum(prior)
    }
    rm(prior)
    carrier[[1L]]()
  })(1))
  expect_list_write_refusals(list_write_data(), list(read))
  expect_identical(probe$calls, 0L)
})

test_that("list write refusal leaves lazy input unread", {
  skip_if_suggest_absent("dtplyr")
  probe <- rlang::env(calls = 0L)
  observe <- function(x) {
    probe$calls <- probe$calls + 1L
    x
  }
  data <- dplyr::filter(dtplyr::lazy_dt(list_write_data()), observe(x) > 0)
  expect_list_write_refusals(data)
  expect_identical(probe$calls, 0L)
  expect_equal(dplyr::collect(data)$x, c(1, 3))
  expect_identical(probe$calls, 1L)
})
