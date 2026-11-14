known_closure_transfer_data <- function() {
  data.frame(group = c("a", "b"), x = c(1, 3), prior = c(100, 300))
}

expect_transfer_dependencies <- function(data, expressions) {
  before <- serialize(data, NULL)
  materialize <- if (is.data.frame(data)) identity else dplyr::collect
  for (kind in c("parent", "total")) {
    share <- rlang::call2(paste0("share_of_", kind), quote(value))
    for (expr in expressions) {
      expect_error(
        materialize(summarize_with_margins(
          data, value = sum(.data$x), prior = !!share, observed = !!expr,
          .grouping = rollup("group")
        )),
        "Ordinary summaries cannot use an earlier .* share",
        class = "marginplyr_error"
      )
      expect_error(
        materialize(summarize_with_margins(
          data, prior = sum(.data$x), value = !!expr, ratio = !!share,
          .grouping = rollup("group")
        )),
        "depends on earlier summary alias `prior`",
        class = "marginplyr_error"
      )
    }
  }
  expect_identical(serialize(data, NULL), before)
}

known_return_transfers <- function() {
  alist(
    (function(prior) {
      read <- function() sum(prior)
      copied <- base::forceAndCall(1L, base::identity, read)
      rm(prior)
      copied()
    })(11),
    (function(prior) {
      # get0() reads this binding by a literal string.
      read <- function() sum(prior) # nolint: object_usage_linter.
      copied <- base::get0("read")
      rm(prior)
      copied()
    })(11),
    (function(prior) {
      # eval() reads this binding through the quoted expression.
      read <- function() sum(prior) # nolint: object_usage_linter.
      copied <- base::eval(base::quote(read))
      rm(prior)
      copied()
    })(11),
    (function(prior) {
      read <- function() sum(prior)
      copied <- base::evalq(read)
      rm(prior)
      copied()
    })(11),
    (function(prior) {
      read <- function() sum(prior)
      copied <- base::withVisible(read)$value
      rm(prior)
      copied()
    })(11),
    (function(prior) {
      read <- function() sum(prior)
      copied <- base::unname(read)
      rm(prior)
      copied()
    })(11),
    (function(prior) {
      copied <- base::structure(function() sum(prior))
      rm(prior)
      copied()
    })(11),
    (function(prior) {
      # get() reads this binding by a literal string.
      read <- function() sum(prior) # nolint: object_usage_linter.
      copied <- base::get(x = "read", envir = environment(),
                          mode = "function", inherits = FALSE)
      rm(prior)
      copied()
    })(1),
    (function(prior) {
      read <- function() sum(prior)
      copied <- match.fun(read)
      rm(prior)
      copied()
    })(1),
    (function(prior) {
      # get() reads this binding by a literal string.
      read <- function() sum(prior) # nolint: object_usage_linter.
      copied <- get("read")
      rm(prior)
      copied()
    })(1),
    (function(prior) {
      read <- function() sum(prior)
      copied <- force(read)
      rm(prior)
      copied()
    })(1),
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

test_that("dtplyr known transfers refuse dependencies before reading input", {
  skip_if_suggest_absent("dtplyr")
  probe <- rlang::env(calls = 0L)
  observe <- function(x) {
    probe$calls <- probe$calls + 1L
    x
  }
  data <- dtplyr::lazy_dt(known_closure_transfer_data())
  data <- dplyr::filter(data, observe(x) > 0)
  expect_transfer_dependencies(data, known_return_transfers())
  expect_identical(probe$calls, 0L)
  expect_equal(dplyr::collect(data)$x, c(1, 3))
  expect_identical(probe$calls, 1L)
})

expect_retained_lazy_transfers <- function(data) {
  before <- serialize(data, NULL)
  for (transfer in alist(force(read), get("read"), match.fun(read))) {
    read <- rlang::expr((function(prior) {
      # The injected transfer reads this local binding.
      read <- function() sum(prior) # nolint: object_usage_linter.
      copied <- !!transfer
      copied()
    })(1))
    for (kind in c("parent", "total")) {
      share <- rlang::call2(paste0("share_of_", kind), quote(value))
      forward <- dplyr::collect(summarize_with_margins(
        data, value = sum(.data$x), prior = !!share, observed = !!read,
        .grouping = rollup("group")
      ))
      reverse <- dplyr::collect(summarize_with_margins(
        data, prior = sum(.data$x), value = !!read, ratio = !!share,
        .grouping = rollup("group")
      ))
      expect_equal(forward$observed, c(1, 1, 1))
      expect_equal(forward$prior, c(0.25, 0.75, 1))
      expect_equal(reverse$value, c(1, 1, 1))
      expect_equal(reverse$ratio, c(1, 1, 1))
    }
  }
  expect_identical(serialize(data, NULL), before)
}

test_that("dtplyr known transfers keep retained bindings legal at collection", {
  skip_if_suggest_absent("dtplyr")
  expect_retained_lazy_transfers(dtplyr::lazy_dt(known_closure_transfer_data()))
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
      copied <- match.fun(read)
      copied()
    })(1),
    (function(prior) {
      read <- function() sum(prior)
      copied <- get("read")
      copied()
    })(1),
    (function(prior) {
      read <- function() sum(prior)
      copied <- force(read)
      copied()
    })(1),
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
    })(1),
    (function(prior) {
      delayedAssign("copied", function() sum(prior))
      copied()
    })(1),
    (function(prior) {
      base::delayedAssign("copied", function() sum(prior))
      copied()
    })(1),
    (function(prior) {
      copied <- function() sum(prior)
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
  for (callee in c("identity", "invisible", "force", "get", "match.fun",
                   "assign", "delayedAssign")) {
    probe <- new.env(parent = emptyenv())
    probe$calls <- 0L
    transfer <- function(x) {
      probe$calls <- probe$calls + 1L
      function() 7
    }
    caller <- rlang::env()
    caller[[callee]] <- if (callee %in% c("assign", "delayedAssign")) {
      function(x, value) {
        probe$calls <- probe$calls + 1L
        base::assign(x, function() 7, envir = parent.frame())
      }
    } else {
      transfer
    }
    expr <- if (callee %in% c("assign", "delayedAssign")) {
      rlang::expr((function(prior) {
        read <- function() sum(prior)
        !!rlang::call2(callee, "copied", quote(read))
        rm(prior)
        copied()
      })(1))
    } else {
      rlang::expr((function(prior) {
        read <- function() sum(prior)
        copied <- !!rlang::call2(callee,
                                 if (callee == "get") "read" else quote(read))
        rm(prior)
        copied()
      })(1))
    }
    expr <- rlang::new_quosure(expr, caller)
    if (callee %in% c("get", "assign", "delayedAssign")) {
      result <- summarize_with_margins(
        known_closure_transfer_data(), value = sum(.data$x),
        prior = share_of_total(value), observed = !!expr,
        .grouping = rollup("group")
      )
      expect_equal(result$observed, c(7, 7, 7))
      expect_identical(probe$calls, 3L)
    } else {
      # A transfer body with more than one statement may return its argument
      # (#1670), so the share refuses without calling it.
      expect_transfer_dependencies(known_closure_transfer_data(), list(expr))
      expect_identical(probe$calls, 0L)
      caller[[callee]] <- function(x) function() 7
      result <- summarize_with_margins(
        known_closure_transfer_data(), value = sum(.data$x),
        prior = share_of_total(value), observed = !!expr,
        .grouping = rollup("group")
      )
      expect_equal(result$observed, c(7, 7, 7))
    }
  }
})

test_that("known transfers never invoke a closure during planning", {
  probe <- rlang::env(calls = 0L)
  for (transfer in alist(force(read), get("read"), match.fun(read))) {
    read <- rlang::quo((function(prior) {
      read <- function() {
        probe$calls <- probe$calls + 1L
        sum(prior)
      }
      copied <- !!transfer
      rm(prior)
      copied()
    })(1))
    expect_transfer_dependencies(known_closure_transfer_data(), list(read))
  }
  expect_identical(probe$calls, 0L)
})

test_that("external get targets and independent closures stay legal", {
  external <- rlang::env(read = function() 7)
  expressions <- alist(
    (function(prior) {
      read <- function() sum(prior)
      copied <- get("read", envir = external, inherits = FALSE)
      rm(prior)
      copied()
    })(1),
    (function(prior) {
      read <- function() 7
      copied <- force(read)
      rm(prior)
      copied()
    })(1),
    (function(prior) {
      read <- function() 7
      copied <- get("read")
      rm(prior)
      copied()
    })(1),
    (function(prior) {
      read <- function() 7
      copied <- match.fun(read)
      rm(prior)
      copied()
    })(1)
  )
  for (kind in c("parent", "total")) {
    share <- rlang::call2(paste0("share_of_", kind), quote(value))
    for (expr in expressions) {
      forward <- summarize_with_margins(
        known_closure_transfer_data(), value = sum(.data$x), prior = !!share,
        observed = !!expr, .grouping = rollup("group")
      )
      reverse <- summarize_with_margins(
        known_closure_transfer_data(), prior = sum(.data$x), value = !!expr,
        ratio = !!share, .grouping = rollup("group")
      )
      expect_equal(forward$observed, c(7, 7, 7))
      expect_equal(reverse$value, c(7, 7, 7))
      expect_equal(forward$prior, c(0.25, 0.75, 1))
      expect_equal(reverse$ratio, c(1, 1, 1))
    }
  }
})

# Delayed bindings retain the closure until its eventual invocation.
delayed_literal_transfers <- function() {
  alist(
    (function(prior) {
      delayedAssign("copied", function() sum(prior))
      rm(prior)
      copied()
    })(1),
    (function(prior) {
      base::delayedAssign(value = function() sum(prior), x = "copied",
                          eval.env = environment(), assign.env = environment())
      rm(prior)
      # delayedAssign() installs this function through a literal string name.
      copied() # nolint: object_usage_linter.
    })(1)
  )
}

test_that("literal delayed closure bindings retain invocation dependencies", {
  data <- known_closure_transfer_data()
  expect_transfer_dependencies(data, delayed_literal_transfers())
  expect_transfer_dependencies(tibble::as_tibble(data),
                               delayed_literal_transfers())
})

test_that("data.table delayed closures retain invocation dependencies", {
  skip_if_suggest_absent("data.table")
  expect_transfer_dependencies(
    data.table::as.data.table(known_closure_transfer_data()),
    delayed_literal_transfers()
  )
})

test_that("planning refuses dependencies without invoking delayed closures", {
  probe <- rlang::env(calls = 0L)
  for (callee in c("delayedAssign", "base::delayedAssign")) {
    bind <- rlang::call2(
      rlang::parse_expr(callee), "copied", rlang::expr(function() {
        probe$calls <- probe$calls + 1L
        sum(prior)
      })
    )
    expr <- rlang::quo((function(prior) {
      !!bind
      rm(prior)
      copied()
    })(1))
    expect_transfer_dependencies(known_closure_transfer_data(), list(expr))
  }
  expect_identical(probe$calls, 0L)
})

test_that("ordinary delayed values run only when the summary demands them", {
  data <- known_closure_transfer_data()
  before <- serialize(data, NULL)
  probe <- rlang::env(values = 0L, calls = 0L)
  for (callee in c("delayedAssign", "base::delayedAssign")) {
    bind <- rlang::call2(
      rlang::parse_expr(callee), "copied", rlang::expr({
        probe$values <- probe$values + 1L
        function() {
          probe$calls <- probe$calls + 1L
          sum(prior)
        }
      })
    )
    expr <- rlang::quo((function(prior) {
      !!bind
      prior <- 7
      copied() + copied()
    })(1))
    result <- summarize_with_margins(
      data, value = sum(.data$x), ratio = share_of_total(value),
      observed = !!expr, .grouping = rollup("group")
    )
    expect_equal(result$observed, c(14, 14, 14))
  }
  expect_identical(probe$values, 6L)
  expect_identical(probe$calls, 12L)
  expect_identical(serialize(data, NULL), before)
})

known_converter_transfers <- function() {
  alist(
    (function(prior) {
      read <- function() sum(prior)
      copied <- base::as.function(read)
      rm(prior)
      copied()
    })(11),
    (function(prior) {
      read <- function() sum(prior)
      copied <- base::Vectorize(read)
      rm(prior)
      copied()
    })(11),
    (function(prior) {
      read <- function() sum(prior)
      copied <- rlang::as_function(read)
      rm(prior)
      copied()
    })(11),
    (function(prior) {
      read <- function() sum(prior)
      copied <- compiler::cmpfun(read)
      rm(prior)
      copied()
    })(11)
  )
}

test_that("known closure converters retain invocation dependencies", {
  expect_transfer_dependencies(
    known_closure_transfer_data(), known_converter_transfers()
  )
  expect_transfer_dependencies(
    tibble::as_tibble(known_closure_transfer_data()),
    known_converter_transfers()
  )
})

test_that("data.table closure converters retain invocation dependencies", {
  skip_if_suggest_absent("data.table")
  expect_transfer_dependencies(
    data.table::as.data.table(known_closure_transfer_data()),
    known_converter_transfers()
  )
})

test_that("dtplyr closure converters refuse dependencies without reading", {
  skip_if_suggest_absent("dtplyr")
  probe <- rlang::env(calls = 0L)
  observe <- function(x) {
    probe$calls <- probe$calls + 1L
    x
  }
  data <- dtplyr::lazy_dt(known_closure_transfer_data(), immutable = TRUE)
  data <- dplyr::filter(data, observe(x) > 0)
  expect_transfer_dependencies(data, known_converter_transfers())
  expect_identical(probe$calls, 0L)
  expect_equal(dplyr::collect(data)$x, c(1, 3))
  expect_identical(probe$calls, 1L)
})

known_converter_controls <- function(retain = TRUE) {
  body <- if (retain) quote(sum(prior)) else 7
  remove <- if (retain) quote(NULL) else quote(rm(prior))
  transfers <- list(
    quote(base::as.function(x = read)),
    quote(base::Vectorize(FUN = read)),
    quote(rlang::as_function(x = read)),
    quote(compiler::cmpfun(f = read, options = list(optimize = 0)))
  )
  lapply(transfers, function(transfer) {
    rlang::expr((function(prior) {
      # The injected converter reads this local binding.
      read <- function() !!body # nolint: object_usage_linter.
      copied <- !!transfer
      !!remove
      copied()
    })(11))
  })
}

expect_converter_values <- function(data, expressions, expected) {
  before <- serialize(data, NULL)
  materialize <- if (is.data.frame(data)) identity else dplyr::collect
  for (kind in c("parent", "total")) {
    share <- rlang::call2(paste0("share_of_", kind), quote(value))
    for (expr in expressions) {
      forward <- materialize(summarize_with_margins(
        data, value = sum(.data$x), prior = !!share, observed = !!expr,
        .grouping = rollup("group")
      ))
      reverse <- materialize(summarize_with_margins(
        data, prior = sum(.data$x), value = !!expr, ratio = !!share,
        .grouping = rollup("group")
      ))
      expect_equal(forward$observed, rep(expected, 3L))
      expect_equal(forward$prior, c(0.25, 0.75, 1))
      expect_equal(reverse$value, rep(expected, 3L))
      expect_equal(reverse$ratio, c(1, 1, 1))
    }
  }
  expect_identical(serialize(data, NULL), before)
}

expect_legal_converter_values <- function(data) {
  expect_converter_values(data, known_converter_controls(), 11)
  expect_converter_values(data, known_converter_controls(retain = FALSE), 7)
}

test_that("closure converters preserve legal lexical bindings", {
  data <- known_closure_transfer_data()
  expect_legal_converter_values(data)
  expect_legal_converter_values(tibble::as_tibble(data))
})

test_that("data.table closure converters preserve legal bindings", {
  skip_if_suggest_absent("data.table")
  expect_legal_converter_values(
    data.table::as.data.table(known_closure_transfer_data())
  )
})

test_that("dtplyr closure converters preserve legal bindings at collection", {
  skip_if_suggest_absent("dtplyr")
  expect_legal_converter_values(
    dtplyr::lazy_dt(known_closure_transfer_data(), immutable = TRUE)
  )
})

test_that("closure converter planning never invokes captured closures", {
  probe <- rlang::env(calls = 0L)
  for (transfer in alist(base::as.function(read), base::Vectorize(read),
                         rlang::as_function(read), compiler::cmpfun(read))) {
    read <- rlang::quo((function(prior) {
      # The injected converter reads this local binding.
      read <- function() { # nolint: object_usage_linter.
        probe$calls <- probe$calls + 1L
        sum(prior)
      }
      attr(read, "tag") <- 1L
      copied <- !!transfer
      rm(prior)
      copied()
    })(11))
    expect_transfer_dependencies(known_closure_transfer_data(), list(read))
  }
  expect_identical(probe$calls, 0L)
})

test_that("caller namespace converters follow ordinary lookup at execution", {
  for (expr in known_converter_transfers()) {
    probe <- rlang::env(calls = 0L)
    caller <- rlang::env()
    caller[["::"]] <- function(package, name) {
      function(x) {
        probe$calls <- probe$calls + 1L
        function() 7
      }
    }
    read <- rlang::new_quosure(expr, caller)
    expect_transfer_dependencies(known_closure_transfer_data(), list(read))
    expect_identical(probe$calls, 0L)
    caller[["::"]] <- function(package, name) function(x) function() 7
    expect_converter_values(known_closure_transfer_data(), list(read), 7)
  }
})

test_that("base closure conversion preserves caller generic methods", {
  for (method in c("as.function.function", "as.function.default")) {
    probe <- rlang::env(calls = 0L)
    caller <- rlang::env()
    caller[[method]] <- function(x, ...) {
      probe$calls <- probe$calls + 1L
      function() 7
    }
    read <- rlang::new_quosure(known_converter_transfers()[[1L]], caller)
    expect_converter_values(known_closure_transfer_data(), list(read), 7)
    expect_identical(probe$calls, 12L)
  }
})

test_that("base closure converters follow ordinary function lookup", {
  data <- known_closure_transfer_data()
  for (converter in c("as.function", "Vectorize")) {
    probe <- rlang::env(calls = 0L)
    caller <- rlang::env()
    caller[[converter]] <- function(x) {
      probe$calls <- probe$calls + 1L
      function() 7
    }
    read <- rlang::expr((function(prior) {
      # The injected converter reads this local binding.
      read <- function() sum(prior) # nolint: object_usage_linter.
      copied <- !!rlang::call2(converter, quote(read))
      rm(prior)
      copied()
    })(11))
    read <- rlang::new_quosure(read, caller)
    expect_transfer_dependencies(data, list(read))
    expect_identical(probe$calls, 0L)
    caller[[converter]] <- function(x) function() 7
    expect_converter_values(data, list(read), 7)

    caller[[converter]] <- 0L
    expect_transfer_dependencies(data, list(read))
  }
})

test_that("as.function honors a class assigned to a local closure", {
  probe <- rlang::env(calls = 0L)
  caller <- rlang::env()
  caller$as.function.margin_share_closure <- function(x, ...) {
    probe$calls <- probe$calls + 1L
    function() 7
  }
  writes <- alist(
    class(read) <- "margin_share_closure",
    class(read) <- c("margin_share_closure", "function"),
    attr(read, "class") <- "margin_share_closure",
    attributes(read) <- list(class = "margin_share_closure")
  )
  for (write in writes) {
    read <- rlang::expr((function(prior) {
      read <- function() sum(prior)
      !!write
      copied <- base::as.function(read)
      rm(prior)
      copied()
    })(11))
    read <- rlang::new_quosure(read, caller)
    expect_converter_values(known_closure_transfer_data(), list(read), 7)
  }
  expect_identical(probe$calls, 48L)
})

test_that("closure class writes retain direct and default-converted reads", {
  writes <- alist(
    class(read) <- "margin_share_default",
    class(read) <- NULL,
    attr(read, "class") <- "margin_share_default",
    attributes(read) <- list(class = "margin_share_default")
  )
  for (index in seq_along(writes)) {
    transfers <- if (index <= 2L) {
      alist(read, base::as.function(read))
    } else {
      alist(read)
    }
    for (transfer in transfers) {
      read <- rlang::expr((function(prior) {
        # The injected write and transfer use this local binding.
        read <- function() sum(prior) # nolint: object_usage_linter.
        !!writes[[index]]
        copied <- !!transfer
        rm(prior)
        copied()
      })(11))
      expect_transfer_dependencies(known_closure_transfer_data(), list(read))
    }
  }
})

nonclass_attribute_transfers <- function() {
  alist(
    (function(prior) {
      read <- function() sum(prior)
      attr(read, "tag") <- 1L
      copied <- base::as.function(read)
      rm(prior)
      copied()
    })(11),
    (function(prior) {
      read <- function() sum(prior)
      class(read) <- "margin_share_default"
      base::attr(read, which = "tag") <- 1L
      copied <- base::as.function(read)
      rm(prior)
      copied()
    })(11)
  )
}

test_that("non-class attributes retain default-converted dependencies", {
  expect_transfer_dependencies(
    known_closure_transfer_data(), nonclass_attribute_transfers()
  )
})

test_that("dtplyr non-class attributes retain converted dependencies", {
  skip_if_suggest_absent("dtplyr")
  probe <- rlang::env(calls = 0L)
  observe <- function(x) {
    probe$calls <- probe$calls + 1L
    x
  }
  data <- dtplyr::lazy_dt(known_closure_transfer_data(), immutable = TRUE)
  data <- dplyr::filter(data, observe(x) > 0)
  expect_transfer_dependencies(data, nonclass_attribute_transfers())
  expect_identical(probe$calls, 0L)
  expect_equal(dplyr::collect(data)$x, c(1, 3))
  expect_identical(probe$calls, 1L)
})

expect_attribute_controls <- function(data) {
  independent <- quote((function(prior) {
    read <- function() 7
    attr(read, "tag") <- 1L
    copied <- base::as.function(read)
    rm(prior)
    copied()
  })(11))
  expect_converter_values(data, list(independent), 7)

  writes <- alist(
    {
      class(read) <- "margin_share_default"
      attr(read, "tag") <- 1L
    },
    {
      field <- "class"
      attr(read, field) <- "margin_share_default"
      attr(read, "tag") <- 1L
    },
    {
      `attr<-` <- function(x, which, value) {
        base::structure(x, class = "margin_share_default")
      }
      attr(read, "tag") <- 1L
    }
  )
  for (write in writes) {
    expr <- rlang::expr((function(prior) {
      # S3 dispatch resolves this method by its name.
      as.function.margin_share_default <- # nolint: object_usage_linter.
        function(x, ...) function() 7
      # The injected attribute write and converter read this binding.
      read <- function() sum(prior) # nolint: object_usage_linter.
      !!write
      copied <- base::as.function(read)
      rm(prior)
      copied()
    })(11))
    expect_converter_values(data, list(expr), 7)
  }
}

test_that("non-class writes preserve independent closures and caller methods", {
  expect_attribute_controls(known_closure_transfer_data())
})

test_that("dtplyr non-class writes preserve legal converters at collection", {
  skip_if_suggest_absent("dtplyr")
  expect_attribute_controls(
    dtplyr::lazy_dt(known_closure_transfer_data(), immutable = TRUE)
  )
})

custom_attribute_closures <- function() {
  alist(
    (function(prior) {
      `attr<-` <- function(x, which, value) function() 7
      read <- function() sum(prior)
      attr(read, "tag") <- 1L
      copied <- base::Vectorize(read)
      rm(prior)
      copied()
    })(11),
    (function(prior) {
      `attr<-` <- function(x, which, value) function() 7
      read <- function() sum(prior)
      attr(read, "tag") <- 1L
      copied <- read
      rm(prior)
      copied()
    })(11)
  )
}

test_that("caller attribute setters can return an independent closure", {
  expect_converter_values(
    known_closure_transfer_data(), custom_attribute_closures(), 7
  )
})

test_that("dtplyr executes independent attribute-setter closures lazily", {
  skip_if_suggest_absent("dtplyr")
  expect_converter_values(
    dtplyr::lazy_dt(known_closure_transfer_data(), immutable = TRUE),
    custom_attribute_closures(), 7
  )
})

attribute_setter_dependencies <- function() {
  alist(
    (function(prior) {
      read <- function() sum(prior)
      base::attr(read, "tag") <- 1L
      copied <- base::Vectorize(read)
      rm(prior)
      copied()
    })(11),
    (function(prior) {
      `attr<-` <- function(x, which, value) x
      read <- function() sum(prior)
      attr(read, "tag") <- 1L
      copied <- base::Vectorize(read)
      rm(prior)
      copied()
    })(11),
    (function(prior) {
      `attr<-` <- function(x, which, value) function() sum(prior)
      read <- function() 7
      attr(read, "tag") <- 1L
      copied <- base::Vectorize(read)
      rm(prior)
      copied()
    })(11)
  )
}

test_that("attribute setter results retain their own share dependencies", {
  expect_transfer_dependencies(
    known_closure_transfer_data(), attribute_setter_dependencies()
  )
})

test_that("dtplyr attribute setter results retain forbidden dependencies", {
  skip_if_suggest_absent("dtplyr")
  expect_transfer_dependencies(
    dtplyr::lazy_dt(known_closure_transfer_data(), immutable = TRUE),
    attribute_setter_dependencies()
  )
})

test_that("attribute setter planning invokes neither setter nor closure", {
  skip_if_suggest_absent("dtplyr")
  data <- data.table::as.data.table(known_closure_transfer_data())
  before <- serialize(data, NULL)
  probe <- rlang::env(setters = 0L, closures = 0L, reads = 0L)
  observe <- function(x) {
    probe$reads <- probe$reads + 1L
    x
  }
  input <- dtplyr::lazy_dt(data, immutable = TRUE)
  input <- dplyr::filter(input, observe(x) > 0)
  expr <- quote((function(prior) {
    `attr<-` <- function(x, which, value) {
      probe$setters <- probe$setters + 1L
      function() {
        probe$closures <- probe$closures + 1L
        7
      }
    }
    read <- function() sum(prior)
    attr(read, "tag") <- 1L
    copied <- base::Vectorize(read)
    rm(prior)
    copied()
  })(11))
  for (kind in c("parent", "total")) {
    helper <- rlang::call2(paste0("share_of_", kind), quote(value))
    result <- summarize_with_margins(
      input, prior = sum(.data$x), value = !!expr, ratio = !!helper,
      .grouping = rollup("group")
    )
    expect_s3_class(result, "dtplyr_step")
    expect_identical(probe$setters, 0L)
    expect_identical(probe$closures, 0L)
    expect_identical(probe$reads, 0L)
    collected <- dplyr::collect(result)
    expect_equal(collected$value, c(7, 7, 7))
    expect_equal(collected$ratio, c(1, 1, 1))
    expect_gt(probe$setters, 0L)
    expect_gt(probe$closures, 0L)
    expect_gt(probe$reads, 0L)
    probe$setters <- probe$closures <- probe$reads <- 0L
  }
  expect_identical(serialize(data, NULL), before)
})

mixed_converter_read <- function(dependent, invoke_method, probe) {
  force(probe)
  body <- if (dependent) quote(sum(prior)) else 7
  predicate <- if (invoke_method) quote(sum(x) > 0) else quote(sum(x) < 0)
  rlang::quo((function(prior) {
    # S3 dispatch resolves this method by its written name.
    as.function.alternative <- function(x, ...) { # nolint: object_usage_linter.
      probe$methods <- probe$methods + 1L
      function() {
        probe$closures <- probe$closures + 1L
        7
      }
    }
    read <- function() !!body
    box <- list2env(list(read = read))
    alias <- box
    if (!!predicate) {
      read <- function() sum(prior)
      class(read) <- "alternative"
      alias <- list2env(list(read = read))
    }
    converted <- base::as.function(alias$read)
    rm(prior)
    converted()
  })(11))
}

test_that("mixed closure dispatch keeps known default dependencies", {
  probe <- rlang::env(methods = 0L, closures = 0L)
  read <- mixed_converter_read(TRUE, FALSE, probe)
  expect_transfer_dependencies(known_closure_transfer_data(), list(read))
  expect_identical(probe$methods, 0L)
  expect_identical(probe$closures, 0L)
})

test_that("dtplyr mixed dispatch retains default dependencies without reads", {
  skip_if_suggest_absent("dtplyr")
  data <- data.table::as.data.table(known_closure_transfer_data())
  before <- serialize(data, NULL)
  probe <- rlang::env(methods = 0L, closures = 0L, reads = 0L)
  observe <- function(x) {
    probe$reads <- probe$reads + 1L
    x
  }
  input <- dtplyr::lazy_dt(data, immutable = TRUE)
  input <- dplyr::filter(input, observe(x) > 0)
  read <- mixed_converter_read(TRUE, FALSE, probe)
  expect_transfer_dependencies(input, list(read))
  expect_identical(probe$methods, 0L)
  expect_identical(probe$closures, 0L)
  expect_identical(probe$reads, 0L)
  expect_identical(serialize(data, NULL), before)
})

test_that("mixed dispatch preserves independent conversion outcomes", {
  probe <- rlang::env(methods = 0L, closures = 0L)
  reads <- lapply(c(FALSE, TRUE), function(invoke) {
    mixed_converter_read(FALSE, invoke, probe)
  })
  expect_converter_values(known_closure_transfer_data(), reads, 7)
  expect_gt(probe$methods, 0L)
  expect_gt(probe$closures, 0L)
})

test_that("dtplyr mixed independent dispatch waits for explicit collection", {
  skip_if_suggest_absent("dtplyr")
  data <- data.table::as.data.table(known_closure_transfer_data())
  before <- serialize(data, NULL)
  input <- dtplyr::lazy_dt(data, immutable = TRUE)
  probe <- rlang::env(methods = 0L, closures = 0L)
  reads <- lapply(c(FALSE, TRUE), function(invoke) {
    mixed_converter_read(FALSE, invoke, probe)
  })
  for (kind in c("parent", "total")) {
    helper <- rlang::call2(paste0("share_of_", kind), quote(value))
    for (read in reads) {
      forward <- summarize_with_margins(
        input, value = sum(.data$x), prior = !!helper, observed = !!read,
        .grouping = rollup("group")
      )
      reverse <- summarize_with_margins(
        input, prior = sum(.data$x), value = !!read, ratio = !!helper,
        .grouping = rollup("group")
      )
      expect_identical(probe$methods, 0L)
      expect_identical(probe$closures, 0L)
      forward <- dplyr::collect(forward)
      reverse <- dplyr::collect(reverse)
      expect_equal(forward$observed, c(7, 7, 7))
      expect_equal(forward$prior, c(0.25, 0.75, 1))
      expect_equal(reverse$value, c(7, 7, 7))
      expect_equal(reverse$ratio, c(1, 1, 1))
      probe$methods <- probe$closures <- 0L
    }
  }
  expect_identical(serialize(data, NULL), before)
})

partial_vectorize_read <- function(dependent, probe) {
  force(probe)
  body <- if (dependent) quote(sum(prior)) else 7
  predicate <- quote(sum(x) < 0)
  rlang::quo((function(prior) {
    box <- list2env(list(read = function() {
      probe$closures <- probe$closures + 1L
      !!body
    }))
    alias <- box
    if (!!predicate) {
      alias <- list2env(list(read = function(unused = 7) {
        probe$closures <- probe$closures + 1L
        sum(prior)
      }))
    }
    converted <- base::Vectorize(alias$read)
    rm(prior)
    converted()
  })(11))
}

test_that("Vectorize keeps known zero-formal alternative dependencies", {
  probe <- rlang::env(closures = 0L)
  read <- partial_vectorize_read(TRUE, probe)
  expect_transfer_dependencies(known_closure_transfer_data(), list(read))
  expect_identical(probe$closures, 0L)
})

test_that("dtplyr Vectorize alternatives retain dependencies without reads", {
  skip_if_suggest_absent("dtplyr")
  data <- data.table::as.data.table(known_closure_transfer_data())
  before <- serialize(data, NULL)
  probe <- rlang::env(closures = 0L, reads = 0L)
  observe <- function(x) {
    probe$reads <- probe$reads + 1L
    x
  }
  input <- dtplyr::lazy_dt(data, immutable = TRUE)
  input <- dplyr::filter(input, observe(x) > 0)
  read <- partial_vectorize_read(TRUE, probe)
  expect_transfer_dependencies(input, list(read))
  expect_identical(probe$closures, 0L)
  expect_identical(probe$reads, 0L)
  expect_identical(serialize(data, NULL), before)
})

test_that("Vectorize leaves nonzero-formal alternative bodies opaque", {
  probe <- rlang::env(closures = 0L)
  read <- partial_vectorize_read(FALSE, probe)
  expect_converter_values(known_closure_transfer_data(), list(read), 7)
  expect_gt(probe$closures, 0L)
})

test_that("dtplyr independent Vectorize alternatives remain lazy and legal", {
  skip_if_suggest_absent("dtplyr")
  data <- data.table::as.data.table(known_closure_transfer_data())
  before <- serialize(data, NULL)
  input <- dtplyr::lazy_dt(data, immutable = TRUE)
  probe <- rlang::env(closures = 0L)
  read <- partial_vectorize_read(FALSE, probe)
  for (kind in c("parent", "total")) {
    helper <- rlang::call2(paste0("share_of_", kind), quote(value))
    forward <- summarize_with_margins(
      input, value = sum(.data$x), prior = !!helper, observed = !!read,
      .grouping = rollup("group")
    )
    reverse <- summarize_with_margins(
      input, prior = sum(.data$x), value = !!read, ratio = !!helper,
      .grouping = rollup("group")
    )
    expect_identical(probe$closures, 0L)
    forward <- dplyr::collect(forward)
    reverse <- dplyr::collect(reverse)
    expect_equal(forward$observed, c(7, 7, 7))
    expect_equal(forward$prior, c(0.25, 0.75, 1))
    expect_equal(reverse$value, c(7, 7, 7))
    expect_equal(reverse$ratio, c(1, 1, 1))
    expect_gt(probe$closures, 0L)
    probe$closures <- 0L
  }
  expect_identical(serialize(data, NULL), before)
})

test_that("base unname preserves caller attribute dispatch", {
  probe <- rlang::env(calls = 0L)
  caller <- rlang::env()
  registry <- get(".__S3MethodsTable__.", envir = baseenv())
  methods <- c("names.margin_transfer", "names<-.margin_transfer")
  on.exit(rm(list = methods, envir = registry), add = TRUE)
  registerS3method("names", "margin_transfer", function(x) "tag",
                   envir = baseenv())
  registerS3method("names<-", "margin_transfer", function(x, value) {
    probe$calls <- probe$calls + 1L
    function() 7
  }, envir = baseenv())
  read <- rlang::quo((function(prior) {
    read <- function() sum(prior)
    class(read) <- "margin_transfer"
    copied <- base::unname(read)
    rm(prior)
    copied()
  })(11))
  read <- rlang::quo_set_env(read, caller)
  expect_converter_values(known_closure_transfer_data(), list(read), 7)
  expect_identical(probe$calls, 12L)
})

additional_base_return_calls <- function() {
  list(
    structure = quote(base::structure(read)),
    unname = quote(base::unname(read)),
    withVisible = quote(base::withVisible(read)$value),
    eval = quote(base::eval(base::quote(read))),
    evalq = quote(base::evalq(read)),
    get0 = quote(base::get0("read")),
    forceAndCall = quote(base::forceAndCall(1L, base::identity, read))
  )
}

additional_return_controls <- function(retain = TRUE) {
  body <- if (retain) quote(sum(prior)) else 7
  remove <- if (retain) quote(NULL) else quote(rm(prior))
  lapply(additional_base_return_calls(), function(transfer) {
    rlang::expr((function(prior) {
      # The injected transfer reads this local binding.
      read <- function() !!body # nolint: object_usage_linter.
      copied <- !!transfer
      !!remove
      copied()
    })(11))
  })
}

expect_additional_returns <- function(data) {
  expect_converter_values(data, additional_return_controls(), 11)
  expect_converter_values(
    data, additional_return_controls(retain = FALSE), 7
  )
}

test_that("additional base returns preserve legal lexical bindings", {
  data <- known_closure_transfer_data()
  expect_additional_returns(data)
  expect_additional_returns(tibble::as_tibble(data))
})

test_that("data.table additional base returns preserve legal bindings", {
  skip_if_suggest_absent("data.table")
  expect_additional_returns(
    data.table::as.data.table(known_closure_transfer_data())
  )
})

test_that("additional base returns never execute closures during planning", {
  probe <- rlang::env(calls = 0L)
  for (transfer in additional_base_return_calls()) {
    read <- rlang::quo((function(prior) {
      # The injected transfer reads this local binding.
      read <- function() { # nolint: object_usage_linter.
        probe$calls <- probe$calls + 1L
        sum(prior)
      }
      copied <- !!transfer
      rm(prior)
      copied()
    })(11))
    expect_transfer_dependencies(known_closure_transfer_data(), list(read))
  }
  expect_identical(probe$calls, 0L)
})

test_that("additional base return calls respect caller replacements", {
  transfers <- additional_base_return_calls()
  for (callee in names(transfers)) {
    probe <- rlang::env(calls = 0L)
    caller <- rlang::env()
    replacement <- function(...) {
      probe$calls <- probe$calls + 1L
      value <- function() 7
      if (identical(callee, "withVisible")) list(value = value) else value
    }
    fresh <- if (identical(callee, "withVisible")) {
      function(...) list(value = function() 7)
    } else {
      function(...) function() 7
    }
    transfer <- transfers[[callee]]
    if (identical(callee, "withVisible")) {
      transfer[[2L]][[1L]] <- rlang::sym(callee)
    } else {
      transfer[[1L]] <- rlang::sym(callee)
    }
    caller[[callee]] <- replacement
    read <- rlang::expr((function(prior) {
      # The injected transfer reads this local binding.
      read <- function() sum(prior) # nolint: object_usage_linter.
      copied <- !!transfer
      rm(prior)
      copied()
    })(11))
    read <- rlang::new_quosure(read, caller)
    if (callee %in% c("eval", "get0")) {
      # The replacement receives a quoted name or a string, never the closure.
      expect_converter_values(known_closure_transfer_data(), list(read), 7)
      expect_identical(probe$calls, 12L)
    } else {
      expect_transfer_dependencies(known_closure_transfer_data(), list(read))
      expect_identical(probe$calls, 0L)
      caller[[callee]] <- fresh
      if (identical(callee, "withVisible")) {
        # A list() body may call a rebound `list` (#1670).
        expect_transfer_dependencies(known_closure_transfer_data(), list(read))
      } else {
        expect_converter_values(known_closure_transfer_data(), list(read), 7)
      }
    }
    caller[[callee]] <- 0L
    expect_transfer_dependencies(known_closure_transfer_data(), list(read))
  }
})

test_that("additional qualified returns respect caller namespace operators", {
  transfers <- additional_base_return_calls()
  for (callee in names(transfers)) {
    probe <- rlang::env(calls = 0L)
    caller <- rlang::env()
    caller[["::"]] <- function(package, name) {
      function(...) {
        probe$calls <- probe$calls + 1L
        value <- function() 7
        if (identical(callee, "withVisible")) list(value = value) else value
      }
    }
    transfer <- transfers[[callee]]
    read <- rlang::expr((function(prior) {
      # The injected transfer reads this local binding.
      read <- function() sum(prior) # nolint: object_usage_linter.
      copied <- !!transfer
      rm(prior)
      copied()
    })(11))
    read <- rlang::new_quosure(read, caller)
    if (identical(callee, "get0")) {
      # The replacement receives a string, never the closure.
      expect_converter_values(known_closure_transfer_data(), list(read), 7)
      expect_identical(probe$calls, 12L)
    } else {
      expect_transfer_dependencies(known_closure_transfer_data(), list(read))
      expect_identical(probe$calls, 0L)
      fresh <- if (identical(callee, "withVisible")) {
        quote(function(...) list(value = function() 7))
      } else {
        quote(function(...) function() 7)
      }
      caller[["::"]] <- rlang::new_function(alist(package = , name = ), fresh)
      if (identical(callee, "withVisible")) {
        # A list() body may call a rebound `list` (#1670).
        expect_transfer_dependencies(known_closure_transfer_data(), list(read))
      } else {
        expect_converter_values(known_closure_transfer_data(), list(read), 7)
      }
    }
    caller[["::"]] <- 0L
    expect_transfer_dependencies(known_closure_transfer_data(), list(read))
  }
})

test_that("dtplyr additional return construction stays lazy", {
  skip_if_suggest_absent("dtplyr")
  data <- data.table::as.data.table(known_closure_transfer_data())
  before <- serialize(data, NULL)
  probe <- rlang::env(reads = 0L)
  observe <- function(x) {
    probe$reads <- probe$reads + 1L
    x
  }
  input <- dtplyr::lazy_dt(data, immutable = TRUE)
  input <- dplyr::filter(input, observe(x) > 0)
  for (retain in c(TRUE, FALSE)) {
    expected <- if (retain) c(11, 11, 11) else c(7, 7, 7)
    for (read in additional_return_controls(retain)) {
      for (kind in c("parent", "total")) {
        helper <- rlang::call2(paste0("share_of_", kind), quote(value))
        forward <- summarize_with_margins(
          input, value = sum(.data$x), prior = !!helper, observed = !!read,
          .grouping = rollup("group")
        )
        reverse <- summarize_with_margins(
          input, prior = sum(.data$x), value = !!read, ratio = !!helper,
          .grouping = rollup("group")
        )
        expect_identical(probe$reads, 0L)
        forward <- dplyr::collect(forward)
        reverse <- dplyr::collect(reverse)
        expect_equal(forward$observed, expected)
        expect_equal(forward$prior, c(0.25, 0.75, 1))
        expect_equal(reverse$value, expected)
        expect_equal(reverse$ratio, c(1, 1, 1))
        expect_gt(probe$reads, 0L)
        probe$reads <- 0L
      }
    }
  }
  expect_identical(serialize(data, NULL), before)
})

# Each form binds a closure reading `prior`, removes the formal, and invokes
# it through a base selector, local() environment, stock callback, caller
# function or local setter the walk reads only as a set of possible closures
# (#1666).
unresolved_closure_transfers <- function(retain = FALSE) {
  forms <- alist(
    (function(prior) {
      l <- list(f = function() sum(prior))
      g <- .subset2(l, exact = TRUE, "f")
      rm(prior)
      g()
    })(11),
    (function(prior) {
      f <- local(function() sum(prior), envir = new.env())
      rm(prior)
      f()
    })(11),
    (function(prior) {
      l <- list(f = function() sum(prior))
      g <- l[["f", exact = NA]]
      rm(prior)
      g()
    })(11),
    (function(prior) {
      l <- list(function() 7, function() sum(prior))
      g <- l[[3 - 1]]
      rm(prior)
      g()
    })(11),
    (function(prior) {
      l <- list(a = list(f = function() sum(prior)))
      g <- l[[c("a", "f", recursive = TRUE)]]
      rm(prior)
      g()
    })(11),
    (function(prior) {
      l <- Filter(is.function, list(function() sum(prior), function() 7))
      g <- l[[1]]
      rm(prior)
      g()
    })(11),
    (function(prior) {
      l <- list(f = function() sum(prior))
      # `ident` is bound in the quosure environment.
      l <- ident(l) # nolint: object_usage_linter.
      rm(prior)
      l$f()
    })(11),
    (function(prior) {
      f <- function() sum(prior)
      # `ident` is bound in the quosure environment.
      f <- ident(f) # nolint: object_usage_linter.
      rm(prior)
      f()
    })(11),
    (function(prior) {
      f <- function() sum(prior)
      rm(prior)
      # `ident` is bound in the quosure environment.
      ident(f)() # nolint: object_usage_linter.
    })(11),
    (function(prior) {
      l <- list(f = function() sum(prior))
      # `ids` is bound in the quosure environment.
      copied <- ids$f(l) # nolint: object_usage_linter.
      rm(prior)
      copied$f()
    })(11),
    (function(prior) {
      l <- list(f = function() sum(prior))
      # `touch` is bound in the quosure environment.
      copied <- touch(l) # nolint: object_usage_linter.
      rm(prior)
      copied$f()
    })(11),
    (function(prior, k) {
      e <- new.env()
      # A local `$<-` makes codetools read these fields as globals.
      e$s <- function(x, name, value) x # nolint: object_usage_linter.
      if (k) e$s <- function(x, name, value) { # nolint: object_usage_linter.
        x
      }
      `$<-` <- e$s
      l <- list(f = function() sum(prior))
      # The local `$<-` makes codetools read the field as a global.
      l$g <- 1 # nolint: object_usage_linter.
      rm(prior)
      l$f()
    })(11, TRUE),
    (function(prior) {
      `$<-` <- function(x, name, value, `{` = 1) {
        x
      }
      l <- list(f = function() sum(prior))
      # The local `$<-` makes codetools read the field as a global.
      l$g <- 1 # nolint: object_usage_linter.
      rm(prior)
      l$f()
    })(11)
  )
  if (retain) {
    forms <- lapply(forms, remove_rm_statements)
  }
  caller <- rlang::env(
    rlang::global_env(), ident = function(x) x,
    ids = list(f = function(x) x), touch = function(x) {
      force(x)
      x
    }
  )
  lapply(forms, rlang::new_quosure, env = caller)
}

# The same form with its `rm(prior)` statement dropped, so the closure reads
# the formal rather than the mask.
remove_rm_statements <- function(expr) {
  if (!is.call(expr)) return(expr)
  if (identical(expr[[1L]], quote(`{`))) {
    keep <- !vapply(as.list(expr)[-1L], function(statement) {
      rlang::is_call(statement, "rm")
    }, logical(1))
    expr <- as.call(c(list(expr[[1L]]), as.list(expr)[-1L][keep]))
  }
  as.call(lapply(as.list(expr), remove_rm_statements))
}

test_that("an unresolved closure invocation after a removal refuses", {
  data <- known_closure_transfer_data()
  expect_transfer_dependencies(data, unresolved_closure_transfers())
  expect_converter_values(data, unresolved_closure_transfers(retain = TRUE), 11)
  # A local() value that is not function syntax carries no closure, and a
  # getElement() field of a fresh environment is not read as a list member.
  expect_converter_values(data, alist(
    (function(prior) {
      held <- local(7)
      rm(prior)
      held
    })(11),
    (function(prior) {
      fields <- new.env()
      fields$read <- function() 7
      held <- getElement(fields, "read")
      rm(prior)
      held()
    })(11)
  ), 7)
  for (expr in unresolved_closure_transfers()) {
    expect_equal(
      dplyr::summarize(data, prior = sum(.data$x), value = !!expr)$value, 4
    )
  }
})

# A caller function whose result does not spell a formal may still return its
# argument through a lookup the walk does not read, so only a visibly fresh
# result clears the closures its arguments carry (#1666).
test_that("a caller function returning its argument indirectly refuses", {
  data <- known_closure_transfer_data()
  caller <- rlang::env(
    rlang::global_env(),
    via_get = function(x) get("x"),
    via_env = function(x) as.list(environment())[[1L]],
    fresh = function(x) function() 7,
    # A list() body may call a rebound `list` (#1670).
    fresh_list = function(x) list(f = function() 7, n = 1L)
  )
  forms <- alist(
    (function(prior) {
      f <- function() sum(prior)
      # `via_get` is bound in the quosure environment.
      f <- via_get(f) # nolint: object_usage_linter.
      rm(prior)
      f()
    })(11),
    (function(prior) {
      l <- list(f = function() sum(prior))
      # `via_get` is bound in the quosure environment.
      l <- via_get(l) # nolint: object_usage_linter.
      rm(prior)
      l$f()
    })(11),
    (function(prior) {
      f <- function() sum(prior)
      # `via_env` is bound in the quosure environment.
      f <- via_env(f) # nolint: object_usage_linter.
      rm(prior)
      f()
    })(11),
    (function(prior) {
      l <- list(f = function() sum(prior))
      # `fresh_list` is bound in the quosure environment.
      l <- fresh_list(l) # nolint: object_usage_linter.
      rm(prior)
      l$f()
    })(11)
  )
  expect_transfer_dependencies(
    data, lapply(forms, rlang::new_quosure, env = caller)
  )
  fresh <- alist(
    (function(prior) {
      f <- function() sum(prior)
      # `fresh` is bound in the quosure environment.
      f <- fresh(f) # nolint: object_usage_linter.
      rm(prior)
      f()
    })(11)
  )
  expect_converter_values(
    data, lapply(fresh, rlang::new_quosure, env = caller), 7
  )
})

test_that("caller calls R refuses or defaults stay with their caller", {
  data <- known_closure_transfer_data()
  caller <- rlang::env(
    rlang::global_env(), held = 1, ident = function(x = function() 7) x
  )
  # A default stands in for an omitted or empty argument, so no input
  # closure passes through the call.
  omitted <- alist(
    (function(prior) {
      # `ident` is bound in the quosure environment.
      g <- ident() # nolint: object_usage_linter.
      rm(prior)
      g()
    })(11),
    (function(prior) {
      # `ident` is bound in the quosure environment.
      g <- ident(x = ) # nolint: object_usage_linter.
      rm(prior)
      g()
    })(11)
  )
  expect_converter_values(
    data, lapply(omitted, rlang::new_quosure, env = caller), 7
  )
  # R refuses to call a caller value that is not a function, so the call
  # carries no closure and the share keeps the ordinary condition.
  applied <- rlang::new_quosure(quote((function(prior) {
    f <- function() sum(prior)
    # `held` is bound in the quosure environment.
    g <- (held)(f) # nolint: object_usage_linter.
    rm(prior)
    g()
  })(11)), caller)
  ordinary <- expect_error(summarize_with_margins(
    data, prior = sum(.data$x), value = !!applied, .grouping = rollup("group")
  ))
  for (helper in rlang::syms(c("share_of_parent", "share_of_total"))) {
    actual <- expect_error(summarize_with_margins(
      data, prior = sum(.data$x), value = !!applied,
      ratio = (!!helper)(value), .grouping = rollup("group")
    ))
    expect_identical(class(actual), class(ordinary))
    expect_identical(conditionMessage(actual), conditionMessage(ordinary))
  }
})

# A selector or callback that caller code computes may pick any closure its
# carrier holds, as may a function literal passed through a caller function
# or a caller function that rebinds a constructor (#1666).
carried_closure_transfers <- function(retain = FALSE) {
  forms <- alist(
    (function(prior) {
      l <- list(f = function() sum(prior))
      # `key` is bound in the quosure environment.
      g <- l[[key()]] # nolint: object_usage_linter.
      rm(prior)
      g()
    })(11),
    (function(prior) {
      l <- Filter(function(f) is.function(f), list(function() sum(prior)))
      rm(prior)
      l[[1]]()
    })(11),
    (function(prior) {
      # `touch` is bound in the quosure environment.
      g <- touch(function() sum(prior)) # nolint: object_usage_linter.
      rm(prior)
      g()
    })(11),
    (function(prior) {
      l <- list(f = function() sum(prior))
      # `shadow` is bound in the quosure environment.
      l <- shadow(l) # nolint: object_usage_linter.
      rm(prior)
      l$f()
    })(11)
  )
  if (retain) {
    forms <- lapply(forms, remove_rm_statements)
  }
  caller <- rlang::env(
    rlang::global_env(), key = function() "f",
    touch = function(x) {
      force(x)
      x
    },
    shadow = function(x) {
      list <- function(...) x
      list(f = function() 7)
    }
  )
  lapply(forms, rlang::new_quosure, env = caller)
}

test_that("computed selectors and callbacks keep the carrier's closures", {
  data <- known_closure_transfer_data()
  expect_transfer_dependencies(data, carried_closure_transfers())
  expect_converter_values(data, carried_closure_transfers(retain = TRUE), 11)
})

# A caller function that binds a local before returning it leaves the walk
# unable to tell what the local holds: a closure over the argument, an
# assign() or a value the body built returns what the argument may carry,
# and the share refuses after a removal (#1670).
test_that("a caller decorator returning a local refuses", {
  data <- known_closure_transfer_data()
  caller <- rlang::env(
    rlang::global_env(),
    logged = function(f) {
      force(f)
      g <- function(...) f(...)
      g
    },
    wrapped = function(x) {
      y <- function() x()
      y
    },
    assigned = function(x) {
      y <- 1
      assign("y", x)
      y
    },
    fresh = function(x) {
      y <- function() 7
      n <- list(1L, c(2L, 3L))
      y
    },
    fresh_list = function(x) {
      y <- list(f = function() 7, n = c(1L, 2L))
      y
    }
  )
  calls <- rlang::syms(c("logged", "wrapped", "assigned", "fresh"))
  forms <- lapply(calls, function(call) {
    rlang::expr((function(prior) {
      f <- function() sum(prior)
      h <- (!!call)(f)
      rm(prior)
      h()
    })(11))
  })
  forms <- c(forms, list(quote((function(prior) {
    f <- function() sum(prior)
    # `fresh_list` is bound in the quosure environment.
    h <- fresh_list(f) # nolint: object_usage_linter.
    rm(prior)
    h$f()
  })(11))))
  expect_transfer_dependencies(
    data, lapply(forms, rlang::new_quosure, env = caller)
  )
})

# A returned function literal is fresh only when it names nothing: one whose
# body reaches the argument through get() or a local alias returns what the
# argument carried, through a caller function or an lapply() or Map()
# callback alike (#1670).
test_that("a returned closure reaching its argument indirectly refuses", {
  data <- known_closure_transfer_data()
  caller <- rlang::env(
    rlang::global_env(),
    via_get = function(x) function() get("x")(),
    via_list = function(x) list(f = function() get("x")()),
    via_alias = function(x) {
      g <- x
      function() g()
    }
  )
  forms <- c(
    lapply(rlang::syms(c("via_get", "via_alias")), function(call) {
      rlang::expr((function(prior) {
        f <- function() sum(prior)
        h <- (!!call)(f)
        rm(prior)
        h()
      })(11))
    }),
    alist(
      (function(prior) {
        f <- function() sum(prior)
        # `via_list` is bound in the quosure environment.
        h <- via_list(f) # nolint: object_usage_linter.
        rm(prior)
        h$f()
      })(11),
      (function(prior) {
        f <- function() sum(prior)
        h <- lapply(list(f), function(x) {
          g <- x
          function() g()
        })
        rm(prior)
        h[[1L]]()
      })(11),
      (function(prior) {
        f <- function() sum(prior)
        h <- Map(function(x) {
          g <- x
          function() g()
        }, list(f))
        rm(prior)
        h[[1L]]()
      })(11)
    )
  )
  expect_transfer_dependencies(
    data, lapply(forms, rlang::new_quosure, env = caller)
  )
  fresh <- alist(
    (function(prior) {
      f <- function() sum(prior)
      h <- lapply(list(f), function(x) function() 7)
      rm(prior)
      h[[1L]]()
    })(11),
    (function(prior) {
      f <- function() sum(prior)
      h <- Map(function(x) function() 7, list(f))
      rm(prior)
      h[[1L]]()
    })(11)
  )
  expect_converter_values(data, fresh, 7)
})

# Only a definition whose body is one expression -- a function literal naming
# nothing, a constant, or a `list()` or `c()` of those, calling base functions
# where it runs -- behind formals with no computed default returns a fresh
# value. A local binding, an earlier statement, a computed default, an `if`
# or a rebound `list`, `c` or `(` may leave what the argument carried in the
# result through code the walk does not read (#1666, #1670).
test_that("a caller function returns a fresh value only as one literal", {
  data <- known_closure_transfer_data()
  bind_default <- function(x, z = (y <- x)) {
    y <- function() 7
    z
    y
  }
  caller <- rlang::env(
    rlang::global_env(),
    src = function(x) {
      y <- 1
      source(textConnection("y <- x"), local = TRUE)
      y
    },
    src_exprs = function(x) {
      y <- 1
      source(exprs = parse(text = "y <- x"), local = TRUE)
      y
    },
    status = function(x) {
      y <- 1
      e <- sys.status()$sys.frames
      e[[length(e)]][["y"]] <- x
      y
    },
    assign_default = function(x, z = assign("y", x)) {
      y <- 1
      z
      y
    },
    bind_default = bind_default,
    condition = function(x) {
      y <- 1
      if (is.null(assign("y", get("x")))) y else y
    },
    default_only = function(x, z = assign("y", x)) function() 7,
    branch_only = function(x, flag = TRUE) if (flag) function() 7 else 8,
    formal_list = function(x, list) list(function() 7)
  )
  rebound <- rlang::env(
    caller,
    list = function(...) get("x", envir = parent.frame()),
    c = function(...) get("x", envir = parent.frame()),
    `(` = function(...) get("x", envir = parent.frame())
  )
  caller$rebound_list <- rlang::new_function(
    alist(x = ), quote(list(function() 7)), rebound
  )
  caller$rebound_c <- rlang::new_function(
    alist(x = ), quote(c(function() 7)), rebound
  )
  caller$rebound_paren <- rlang::new_function(
    alist(x = ), quote((function() 7)), rebound
  )
  calls <- c("src", "src_exprs", "status", "assign_default", "bind_default",
             "condition", "default_only", "branch_only", "rebound_list",
             "rebound_c", "rebound_paren")
  forms <- lapply(rlang::syms(calls), function(call) {
    rlang::expr((function(prior) {
      f <- function() sum(prior)
      h <- (!!call)(f)
      rm(prior)
      h()
    })(11))
  })
  forms <- c(forms, alist(
    (function(prior) {
      f <- function() sum(prior)
      y <- f
      h <- lapply(list(1), function(x) {
        y <- 1
        rm(list = "y")
        y
      })
      rm(prior)
      h[[1L]]()
    })(11),
    (function(prior) {
      f <- function() sum(prior)
      y <- f
      g <- function(x) {
        y <- 1
        rm(list = "y")
        y
      }
      h <- g(f)
      rm(prior)
      h()
    })(11),
    (function(prior) {
      f <- function() sum(prior)
      nchar <- function() assign("y", f, envir = parent.frame())
      h <- lapply(list(1), function(x) {
        y <- 1
        nchar()
        y
      })
      rm(prior)
      h[[1L]]()
    })(11),
    (function(prior) {
      f <- function() sum(prior)
      list <- function(...) get("x", envir = parent.frame())
      h <- lapply(base::list(f), function(x) list(function() 7))
      rm(prior)
      h[[1L]]()
    })(11),
    (function(prior) {
      f <- function() sum(prior)
      h <- lapply(list(f), function(x) base::list(function() 7))
      rm(prior)
      h[[1L]][[1L]]()
    })(11),
    (function(prior) {
      f <- function() sum(prior)
      # `formal_list` is bound in the quosure environment.
      h <- formal_list( # nolint: object_usage_linter.
        f, function(...) get("x", envir = parent.frame())
      )
      rm(prior)
      h()
    })(11)
  ))
  expect_transfer_dependencies(
    data, lapply(forms, rlang::new_quosure, env = caller)
  )
  # A `{` or list() body may call a rebound callee (#1670); only a body that
  # is exactly a constant or a closed function literal calls none.
  fresh <- rlang::env(
    rlang::global_env(),
    braced = function(x, n = 1L) {
      function() 7
    },
    listed = function(x) list(f = function() 7, n = c(1L, 2L)),
    literal = function(x, n = 1L) function() 7
  )
  kept <- alist(
    (function(prior) {
      f <- function() sum(prior)
      # `braced` is bound in the quosure environment.
      h <- braced(f) # nolint: object_usage_linter.
      rm(prior)
      h()
    })(11),
    (function(prior) {
      f <- function() sum(prior)
      # `listed` is bound in the quosure environment.
      h <- listed(f) # nolint: object_usage_linter.
      rm(prior)
      h$f()
    })(11)
  )
  expect_transfer_dependencies(
    data, lapply(kept, rlang::new_quosure, env = fresh)
  )
  literal <- quote((function(prior) {
    f <- function() sum(prior)
    # `literal` is bound in the quosure environment.
    h <- literal(f) # nolint: object_usage_linter.
    rm(prior)
    h()
  })(11))
  expect_converter_values(
    data, list(rlang::new_quosure(literal, fresh)), 7
  )
  # A caller `::` is read only when its whole body is exactly the function
  # literal, which calls nothing but `function` before returning it.
  # The call is built so that no undeclared package name is spelled in source.
  qualified_head <- call("::", as.name("pkg"), as.name("fresh"))
  qualified <- bquote((function(prior) {
    f <- function() sum(prior)
    h <- .(qualified_head)(f)
    rm(prior)
    h()
  })(11))
  operators <- list(
    staged = function(package, name) {
      y <- 1
      function(x) function() 7
    },
    shadowing = function(list, name) function(x) list(function() 7)
  )
  expect_transfer_dependencies(data, lapply(operators, function(operator) {
    rlang::new_quosure(qualified, rlang::env(`::` = operator))
  }))
  operators <- list(
    braced = function(package, name) {
      function(x) function() 7
    },
    literal = function(package, name) function(x) function() 7
  )
  expect_transfer_dependencies(data, list(rlang::new_quosure(
    qualified, rlang::env(`::` = operators$braced)
  )))
  expect_converter_values(data, list(rlang::new_quosure(
    qualified, rlang::env(`::` = operators$literal)
  )), 7)
})

test_that("a `function` formal or a reference body is not fresh", {
  data <- known_closure_transfer_data()
  store <- new.env()
  caller <- rlang::env(
    rlang::global_env(),
    rebinds = rlang::new_function(
      rlang::pairlist2(x = , `function` = ), quote(function() 7)
    ),
    defaulted = rlang::new_function(
      rlang::pairlist2(x = , `function` = 1), quote(function() 7)
    ),
    referenced = rlang::new_function(rlang::pairlist2(x = ), store),
    callback = rlang::new_function(
      rlang::pairlist2(i = , `function` = ), quote(function() 7)
    ),
    listed = function(i) list(function() 7)
  )
  forms <- alist(
    (function(prior) {
      f <- function() sum(prior)
      # `rebinds` is bound in the quosure environment.
      h <- rebinds(f, function(...) f) # nolint: object_usage_linter.
      rm(prior)
      h()
    })(11),
    (function(prior) {
      f <- function() sum(prior)
      # `defaulted` is bound in the quosure environment.
      h <- defaulted(f, function(...) f) # nolint: object_usage_linter.
      rm(prior)
      h()
    })(11),
    (function(prior) {
      f <- function() sum(prior)
      # `referenced` is bound in the quosure environment.
      s <- referenced(f) # nolint: object_usage_linter.
      s$f <- f
      rm(prior)
      s$f()
    })(11),
    (function(prior) {
      f <- function() sum(prior)
      # `callback` is bound in the quosure environment.
      h <- lapply( # nolint: object_usage_linter.
        list(f), callback, function(...) f
      )
      rm(prior)
      h[[1L]]()
    })(11),
    (function(prior) {
      f <- function() sum(prior)
      # `listed` is bound in the quosure environment.
      h <- lapply(list(f), listed) # nolint: object_usage_linter.
      rm(prior)
      h[[1L]][[1L]]()
    })(11)
  )
  expect_transfer_dependencies(
    data, lapply(forms, rlang::new_quosure, env = caller)
  )
  # A `function` that returns the calling frame's `x` when it has one, and
  # otherwise builds the closure base `function` would.
  rebinder <- function(formals, body, ...) {
    frame <- parent.frame()
    if (exists("x", envir = frame, inherits = FALSE)) {
      return(get("x", envir = frame))
    }
    eval(rlang::call2(base::`function`, formals, substitute(body)), frame)
  }
  shadowing <- rlang::new_function(
    rlang::pairlist2(package = , name = , `function` = rebinder),
    quote(function(x) function() 7)
  )
  qualified_head <- call("::", as.name("pkg"), as.name("fresh"))
  qualified <- bquote((function(prior) {
    f <- function() sum(prior)
    h <- .(qualified_head)(f)
    rm(prior)
    h()
  })(11))
  expect_transfer_dependencies(data, list(rlang::new_quosure(
    qualified, rlang::env(`::` = shadowing)
  )))
  # An enclosure binding `function`, at any depth, rebinds the literal too.
  rebinding <- rlang::env(rlang::global_env(), `function` = rebinder)
  nested <- rlang::env(rebinding)
  active <- rlang::env(rlang::global_env())
  active_reads <- 0L
  makeActiveBinding("function", function() {
    active_reads <<- active_reads + 1L
    rebinder
  }, active)
  lazy <- rlang::env(rlang::global_env())
  delayedAssign("function", rebinder, assign.env = lazy)
  enclosed <- rlang::env(
    rlang::global_env(),
    fresh = rlang::new_function(
      alist(x = ), quote(function() 7), env = rebinding
    ),
    deep = rlang::new_function(alist(x = ), quote(function() 7), env = nested),
    on_active = rlang::new_function(
      alist(x = ), quote(function() 7), env = active
    ),
    on_lazy = rlang::new_function(alist(x = ), quote(function() 7), env = lazy),
    callback = rlang::new_function(
      alist(x = ), quote(function() 7), env = rebinding
    )
  )
  enclosed_forms <- alist(
    (function(prior) {
      f <- function() sum(prior)
      # `fresh` is bound in the quosure environment.
      h <- fresh(f) # nolint: object_usage_linter.
      rm(prior)
      h()
    })(11),
    (function(prior) {
      f <- function() sum(prior)
      # `deep` is bound in the quosure environment.
      h <- deep(f) # nolint: object_usage_linter.
      rm(prior)
      h()
    })(11),
    (function(prior) {
      f <- function() sum(prior)
      # `on_active` is bound in the quosure environment.
      h <- on_active(f) # nolint: object_usage_linter.
      rm(prior)
      h()
    })(11),
    (function(prior) {
      f <- function() sum(prior)
      # `on_lazy` is bound in the quosure environment.
      h <- on_lazy(f) # nolint: object_usage_linter.
      rm(prior)
      h()
    })(11),
    (function(prior) {
      f <- function() sum(prior)
      # `callback` is bound in the quosure environment.
      h <- lapply(list(f), callback) # nolint: object_usage_linter.
      rm(prior)
      h[[1L]]()
    })(11)
  )
  expect_transfer_dependencies(
    data, lapply(enclosed_forms, rlang::new_quosure, env = enclosed)
  )
  # Planning reads neither binding's value.
  expect_identical(active_reads, 0L)
  expect_true(rlang::env_binding_are_lazy(lazy, "function"))
  # A classed enclosure's `[[` method cannot hide its binding.
  assign("[[.marginplyr_trick_env", function(x, i, ...) base::`function`,
         envir = globalenv())
  withr::defer(rm("[[.marginplyr_trick_env", envir = globalenv()))
  classed <- rlang::env(rlang::global_env(), `function` = rebinder)
  class(classed) <- "marginplyr_trick_env"
  on_classed <- rlang::env(
    rlang::global_env(),
    on_classed = rlang::new_function(
      alist(x = ), quote(function() 7), env = classed
    )
  )
  expect_transfer_dependencies(data, list(rlang::new_quosure(quote(
    (function(prior) {
      f <- function() sum(prior)
      # `on_classed` is bound in the quosure environment.
      h <- on_classed(f) # nolint: object_usage_linter.
      rm(prior)
      h()
    })(11)
  ), on_classed)))
  operator <- rlang::new_function(
    alist(package = , name = ), quote(function(x) function() 7),
    env = rebinding
  )
  expect_transfer_dependencies(data, list(rlang::new_quosure(
    qualified, rlang::env(`::` = operator)
  )))
  # A missing `function` formal in an enclosure is read without forcing it,
  # and an ordinary source through such a caller stays shareable.
  missing_formal <- (function(`function`) environment())()
  twice <- rlang::env(
    rlang::global_env(),
    twice = rlang::new_function(alist(x = ), quote(list(x)), missing_formal)
  )
  expect_converter_values(data, list(rlang::new_quosure(quote(
    (function(prior) {
      f <- function() 7
      # `twice` is bound in the quosure environment.
      h <- twice(f) # nolint: object_usage_linter.
      h[[1L]]()
    })(11)
  ), twice)), 7)
})

test_that("a caller function's callees rebound by the expression refuse", {
  data <- known_closure_transfer_data()
  withr::defer(
    rm(list = intersect("list", ls(globalenv())), envir = globalenv())
  )
  rebinding <- quote(function(...) {
    .Primitive("list")(get("x", envir = parent.frame()))
  })
  global <- as.name(".GlobalEnv")
  writes <- list(
    bquote(assign("list", .(rebinding), envir = environment(CALL))),
    bquote(assign("list", .(rebinding), envir = globalenv())),
    bquote(assign("list", .(rebinding), pos = 1)),
    rlang::call2("<-", call("$", global, quote(list)), rebinding),
    rlang::call2("<-", call("[[", global, "list"), rebinding),
    bquote({
      e <- environment(CALL)
      e$list <- .(rebinding)
    }),
    bquote({
      g <- CALL
      environment(g)$list <- .(rebinding)
    })
  )
  forms <- lapply(writes, function(write) {
    bquote((function(prior) {
      f <- function() sum(prior)
      .(write)
      h <- CALL(f)[[1]]
      rm(prior)
      h()
    })(11))
  })
  # A global CALL runs its body where the global writes land.
  callees <- list(
    function(x) list(function() 7),
    rlang::new_function(alist(x = ), quote(list(function() 7)), globalenv())
  )
  expressions <- unlist(lapply(callees, function(callee) {
    lapply(forms, function(form) {
      rlang::new_quosure(form, rlang::env(rlang::global_env(), CALL = callee))
    })
  }))
  # The call is built so that no undeclared package name is spelled in source.
  qualified_head <- call("::", as.name("pkg"), as.name("fresh"))
  qualified <- bquote((function(prior) {
    f <- function() sum(prior)
    assign("list", .(rebinding), envir = environment(`::`))
    h <- .(qualified_head)(f)[[1]]
    rm(prior)
    h()
  })(11))
  expressions <- c(expressions, list(rlang::new_quosure(qualified, rlang::env(
    `::` = function(package, name) function(x) list(function() 7)
  ))))
  expect_transfer_dependencies(data, expressions)
})
