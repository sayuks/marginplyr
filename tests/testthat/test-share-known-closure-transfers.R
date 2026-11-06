known_closure_transfer_data <- function() {
  data.frame(group = c("a", "b"), x = c(1, 3), prior = c(100, 300))
}

expect_transfer_dependencies <- function(data, expressions) {
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

known_return_transfers <- function() {
  alist(
    (function(prior) {
      read <- function() sum(prior)
      copied <- identity(read)
      rm(prior)
      copied()
    })(1),
    (function(prior) {
      read <- function() sum(prior)
      copied <- base::invisible(read)
      rm(prior)
      copied()
    })(1)
  )
}

test_that("known base return calls retain closure dependencies", {
  data <- known_closure_transfer_data()
  expect_transfer_dependencies(data, known_return_transfers())
  expect_transfer_dependencies(tibble::as_tibble(data),
                               known_return_transfers())
})

test_that("data.table known return calls retain closure dependencies", {
  skip_if_suggest_absent("data.table")
  expect_transfer_dependencies(
    data.table::as.data.table(known_closure_transfer_data()),
    known_return_transfers()
  )
})

literal_assignment_transfers <- function() {
  alist(
    (function(prior) {
      read <- function() sum(prior)
      assign("copied", read)
      rm(prior)
      copied()
    })(1),
    (function(prior) {
      read <- function() sum(prior)
      copied <- NULL
      base::assign(value = read, x = "copied", envir = environment())
      rm(prior)
      copied()
    })(1)
  )
}

test_that("literal base assignment retains closure dependencies", {
  data <- known_closure_transfer_data()
  expect_transfer_dependencies(data, literal_assignment_transfers())
  expect_transfer_dependencies(tibble::as_tibble(data),
                               literal_assignment_transfers())
})

test_that("data.table literal assignment retains closure dependencies", {
  skip_if_suggest_absent("data.table")
  expect_transfer_dependencies(
    data.table::as.data.table(known_closure_transfer_data()),
    literal_assignment_transfers()
  )
})

test_that("known closure transfers respect retained lexical bindings", {
  inputs <- list(known_closure_transfer_data(),
                 tibble::as_tibble(known_closure_transfer_data()))
  if (suggest_available("data.table")) {
    inputs[[3L]] <- data.table::as.data.table(known_closure_transfer_data())
  }
  expressions <- alist(
    (function(prior) {
      read <- function() sum(prior)
      copied <- identity(read)
      copied()
    })(1),
    (function(prior) {
      read <- function() sum(prior)
      copied <- invisible(read)
      copied()
    })(1),
    (function(prior) {
      read <- function() sum(prior)
      assign("copied", read)
      copied()
    })(1)
  )
  for (data in inputs) {
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
})

test_that("known closure return calls follow function lookup", {
  expressions <- alist(
    (function(prior) {
      identity <- 0
      read <- function() sum(prior)
      copied <- identity(read)
      rm(prior)
      copied()
    })(1),
    (function(prior) {
      invisible <- function(x) function() 7
      read <- function() sum(prior)
      copied <- base::invisible(x = read)
      rm(prior)
      copied()
    })(1),
    (function(prior) {
      read <- function() sum(prior)
      rm(prior)
      assign("copied", read)
      copied()
    })(1)
  )
  expect_transfer_dependencies(known_closure_transfer_data(), expressions)
})

test_that("planning does not execute caller closure transfer functions", {
  for (callee in c("identity", "invisible", "assign")) {
    probe <- new.env(parent = emptyenv())
    probe$calls <- 0L
    transfer <- function(x) {
      probe$calls <- probe$calls + 1L
      function() 7
    }
    caller <- rlang::env()
    caller[[callee]] <- if (callee == "assign") {
      function(x, value) {
        probe$calls <- probe$calls + 1L
        base::assign(x, function() 7, envir = parent.frame())
      }
    } else {
      transfer
    }
    expr <- if (callee == "assign") {
      quote((function(prior) {
        read <- function() sum(prior)
        assign("copied", read)
        rm(prior)
        copied()
      })(1))
    } else {
      rlang::expr((function(prior) {
        read <- function() sum(prior)
        copied <- !!rlang::call2(callee, quote(read))
        rm(prior)
        copied()
      })(1))
    }
    result <- summarize_with_margins(
      known_closure_transfer_data(), value = sum(.data$x),
      prior = share_of_total(value),
      observed = !!rlang::new_quosure(expr, caller),
      .grouping = rollup("group")
    )
    expect_equal(result$observed, c(7, 7, 7))
    expect_identical(probe$calls, 3L)
  }
})
