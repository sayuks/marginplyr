condition_wrapper_reads <- function(wrappers, retained = FALSE,
                                    independent = FALSE, body = NULL) {
  lapply(wrappers, function(wrapper) {
    if (is.null(body)) {
      body <- if (independent) quote(7) else quote(sum(prior))
    }
    remove <- if (retained) NULL else quote(base::rm(prior))
    rlang::expr((function(prior) {
      # The injected wrapper reads this binding.
      read <- function() !!body # nolint: object_usage_linter.
      copied <- !!wrapper
      !!remove
      copied()
    })(11))
  })
}

condition_wrapper_data <- function() {
  data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
}

condition_wrappers <- function() {
  list(
    quote(base::suppressWarnings(read)),
    quote(base::suppressMessages(read)),
    quote(base::tryCatch(read, error = function(e) 0)),
    quote(base::withCallingHandlers(read, warning = function(w) NULL)),
    quote(base::withRestarts(read)),
    quote(base::try(read, silent = TRUE)),
    quote(base::local(read)),
    quote(base::eval(base::quote(read), envir = base::environment())),
    quote(base::`attr<-`(read, "label", value = "a")),
    quote(base::I(read))
  )
}

test_that("base condition wrappers retain invocation-time dependencies", {
  data <- condition_wrapper_data()
  before <- serialize(data, NULL)
  expect_share_dependencies(data, condition_wrapper_reads(condition_wrappers()))
  expect_identical(serialize(data, NULL), before)
})

finite_condition_carriers <- function() {
  list(
    quote(base::Filter(base::is.function, base::list(read))[[1L]]),
    quote(base::lapply(base::list(read), base::identity)[[1L]]),
    quote(base::Map(base::identity, base::list(read))[[1L]]),
    quote(base::Reduce(base::identity, base::list(read)))
  )
}

test_that("finite stock callbacks retain invocation-time dependencies", {
  data <- condition_wrapper_data()
  before <- serialize(data, NULL)
  expect_share_dependencies(
    data, condition_wrapper_reads(finite_condition_carriers())
  )
  expect_identical(serialize(data, NULL), before)
})

expect_condition_values <- function(data, reads, expected) {
  materialize <- if (is.data.frame(data)) identity else dplyr::collect
  before <- serialize(data, NULL)
  for (kind in c("parent", "total")) {
    share <- rlang::call2(paste0("share_of_", kind), quote(value))
    for (read in reads) {
      forward <- materialize(summarize_with_margins(
        data, value = sum(.data$x), prior = !!share, observed = !!read,
        .grouping = rollup("g")
      ))
      reverse <- materialize(summarize_with_margins(
        data, prior = sum(.data$x), value = !!read, ratio = !!share,
        .grouping = rollup("g")
      ))
      expect_equal(forward$observed, rep(expected, 3L))
      expect_identical(forward$prior, c(0.25, 0.75, 1))
      expect_equal(reverse$value, rep(expected, 3L))
      expect_identical(reverse$ratio, c(1, 1, 1))
    }
  }
  expect_identical(serialize(data, NULL), before)
}

expect_condition_controls <- function(data) {
  wrappers <- c(condition_wrappers(), finite_condition_carriers())
  expect_condition_values(
    data, condition_wrapper_reads(wrappers, retained = TRUE), 11
  )
  expect_condition_values(
    data, condition_wrapper_reads(wrappers, independent = TRUE), 7
  )
}

test_that("condition wrappers retain legal lexical and independent values", {
  expect_condition_controls(condition_wrapper_data())
  expect_condition_controls(tibble::as_tibble(condition_wrapper_data()))
})

test_that("dtplyr condition wrapper guards run before input or caller code", {
  skip_if_suggest_absent("dtplyr")
  data <- data.table::as.data.table(condition_wrapper_data())
  before <- serialize(data, NULL)
  probe <- rlang::env(reads = 0L, calls = 0L)
  observe <- function(x) {
    probe$reads <- probe$reads + 1L
    x
  }
  input <- dtplyr::lazy_dt(data, immutable = TRUE)
  input <- dplyr::filter(input, observe(x) > 0)
  wrappers <- c(condition_wrappers(), finite_condition_carriers())
  reads <- condition_wrapper_reads(wrappers, body = rlang::expr({
    probe$calls <- probe$calls + 1L
    sum(prior)
  }))
  expect_share_dependencies(input, reads)
  expect_identical(probe$reads, 0L)
  expect_identical(probe$calls, 0L)
  expect_identical(serialize(data, NULL), before)
})

test_that("dtplyr legal condition wrappers execute only at collection", {
  skip_if_suggest_absent("dtplyr")
  data <- data.table::as.data.table(condition_wrapper_data())
  before <- serialize(data, NULL)
  input <- dtplyr::lazy_dt(data, immutable = TRUE)
  expect_condition_controls(input)
  expect_identical(serialize(data, NULL), before)
})

condition_lookup_controls <- function() {
  lapply(condition_wrappers(), function(wrapper) {
    name <- as.character(wrapper[[1L]][[3L]])
    wrapper[[1L]] <- rlang::sym(name)
    binding <- rlang::call2(
      "<-", rlang::sym(name), quote(function(...) function() 7)
    )
    rlang::expr((function(prior) {
      !!binding
      # The injected wrapper reads this binding.
      read <- function() sum(prior) # nolint: object_usage_linter.
      copied <- !!wrapper
      base::rm(prior)
      copied()
    })(11))
  })
}

test_that("condition wrappers preserve caller lookup and callable classes", {
  data <- condition_wrapper_data()
  expect_condition_values(data, condition_lookup_controls(), 7)
  local_read <- quote((function(prior) {
    copied <- base::local({
      prior <- 7
      function() sum(prior)
    })
    base::rm(prior)
    copied()
  })(11))
  expect_condition_values(data, list(local_read), 7)
})

# Caller code the walk does not model returns the copy, so which closure it
# holds is unknown after the removal and the source is refused (#130, #1666).
test_that("caller methods and callbacks returning a closure refuse", {
  reads <- alist(
    (function(prior) {
      # The converter resolves this caller method through S3 dispatch.
      as.function.AsIs <- # nolint: object_usage_linter.
        function(x, ...) function() 7
      copied <- base::as.function(base::I(function() sum(prior)))
      base::rm(prior)
      copied()
    })(11),
    (function(prior) {
      read <- function() sum(prior)
      identity <- function(x) function() 7
      copied <- base::lapply(base::list(read), identity)[[1L]]
      base::rm(prior)
      copied()
    })(11),
    (function(prior) {
      read <- function() sum(prior)
      identity <- function(x) function() 7
      copied <- base::Map(identity, base::list(read))[[1L]]
      base::rm(prior)
      copied()
    })(11)
  )
  expect_share_dependencies(condition_wrapper_data(), reads)
})

test_that("direct and previous closure carrier controls retain dependencies", {
  wrappers <- alist(
    read, base::identity(read), base::invisible(read), base::force(read),
    base::structure(read), base::as.function(read), base::Vectorize(read)
  )
  expect_share_dependencies(
    condition_wrapper_data(), condition_wrapper_reads(wrappers)
  )
})

test_that("dtplyr condition planning does not invoke caller wrappers", {
  skip_if_suggest_absent("dtplyr")
  probe <- rlang::env(calls = 0L)
  input <- dtplyr::lazy_dt(condition_wrapper_data(), immutable = TRUE)
  binding <- rlang::call2(
    "<-", rlang::sym("suppressWarnings"), rlang::expr(function(x) {
      probe$calls <- probe$calls + 1L
      function() 7
    })
  )
  read <- rlang::quo((function(prior) {
    !!binding
    copied <- suppressWarnings(function() sum(prior))
    base::rm(prior)
    copied()
  })(11))
  for (kind in c("parent", "total")) {
    share <- rlang::call2(paste0("share_of_", kind), quote(value))
    probe$calls <- 0L
    result <- summarize_with_margins(
      input, prior = sum(.data$x), value = !!read, ratio = !!share,
      .grouping = rollup("g")
    )
    expect_identical(probe$calls, 0L)
    value <- dplyr::collect(result)
    expect_equal(value$value, c(7, 7, 7))
    expect_identical(value$ratio, c(1, 1, 1))
    expect_gt(probe$calls, 0L)
  }
})

test_that("bare stock callbacks retain the same callable proofs", {
  wrappers <- alist(
    Filter(is.function, list(read))[[1L]],
    lapply(list(read), identity)[[1L]],
    Map(identity, list(read))[[1L]],
    Reduce(identity, list(read))
  )
  expect_share_dependencies(
    condition_wrapper_data(), condition_wrapper_reads(wrappers)
  )
})

test_that("I replaces an implicit function class before converter dispatch", {
  read <- quote((function(prior) {
    # S3 lookup reaches this method only for the explicit function class.
    as.function.function <- # nolint: object_usage_linter.
      function(x, ...) function() 7
    copied <- base::as.function(base::I(function() sum(prior)))
    base::rm(prior)
    copied()
  })(11))
  reads <- lapply(alist(NULL, base::class(read) <- NULL), function(reset) {
    rlang::expr((function(prior) {
      # The converter resolves this method only for an explicit function class.
      as.function.function <- # nolint: object_usage_linter.
        function(x, ...) function() 7
      read <- function() sum(prior)
      !!reset
      copied <- base::as.function(base::I(read))
      base::rm(prior)
      copied()
    })(11))
  })
  # The explicit class reaches the caller method, whose result the walk does
  # not model, so it refuses as well (#1666).
  explicit <- quote((function(prior) {
    # The converter resolves this method from the explicit class on copied.
    as.function.function <- # nolint: object_usage_linter.
      function(x, ...) function() 7
    read <- function() sum(prior)
    base::class(read) <- "function"
    copied <- base::as.function(base::I(read))
    base::rm(prior)
    copied()
  })(11))
  expect_share_dependencies(
    condition_wrapper_data(), c(list(read), reads, list(explicit))
  )
})
