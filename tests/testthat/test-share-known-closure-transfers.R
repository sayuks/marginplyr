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
    expect_converter_values(known_closure_transfer_data(), list(read), 7)
    expect_identical(probe$calls, 12L)
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
    expect_converter_values(data, list(read), 7)
    expect_identical(probe$calls, 12L)

    caller[[converter]] <- 0L
    expect_transfer_dependencies(data, list(read))
    expect_identical(probe$calls, 12L)
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
    expect_converter_values(known_closure_transfer_data(), list(read), 7)
    expect_identical(probe$calls, 12L)
    caller[[callee]] <- 0L
    expect_transfer_dependencies(known_closure_transfer_data(), list(read))
    expect_identical(probe$calls, 12L)
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
    expect_converter_values(known_closure_transfer_data(), list(read), 7)
    expect_identical(probe$calls, 12L)
    caller[["::"]] <- 0L
    expect_transfer_dependencies(known_closure_transfer_data(), list(read))
    expect_identical(probe$calls, 12L)
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
# it through a base selector, local() environment, stock callback or local
# setter spelled in a way the walk reads only as a set of possible closures
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
  lapply(forms, rlang::new_quosure, env = rlang::global_env())
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
