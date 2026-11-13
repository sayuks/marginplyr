# A nested constructor's capture either runs against column names alone or
# needs a typed selection context (ADR 0008). A SQL input's generic proxy
# carries no R types, so it refuses the second, which makes it the input that
# shows which answer a capture received: a capture proven name-only behaves as
# it does on a local frame, and one that is not is refused before it runs.

capture_input <- function() data.frame(a = c("x", "y"), b = 1:2)

capture_lazy <- function() {
  dbplyr::tbl_lazy(capture_input(), con = dbplyr::simulate_postgres())
}

# The nested constructor arrives as a quosure, so its own injection operators
# reach the preflight unprocessed; written inline, the outer constructor's
# capture would already have run them.
inspect_nested <- function(data, nested) {
  inspect_grouping(
    data, .grouping = grouping_sets(!!nested, "b"), .format = "list"
  )
}

typed_context_refusal <- "Can't provide a faithful typed selection context"

test_that("a parenthesized specification value is read from names", {
  spec <- grouping_set(a)
  nested <- rlang::new_quosure(rlang::call2(quote(base::`(`), spec))
  expect_identical(
    inspect_nested(capture_lazy(), nested)$included, list("a", "b")
  )
})

test_that("an embraced operand the capture cannot prove is refused unread", {
  calls <- rlang::env(n = 0L)
  caller <- rlang::env(
    globalenv(),
    make = function() quote(a)
  )
  makeActiveBinding("act", function() {
    calls$n <- calls$n + 1L
    quote(a)
  }, caller)
  # `..2` of a one-argument call: a promise whose capture fails.
  overrun <- local({
    capture <- function(x) environment()
    forward <- function(...) capture(..2)
    forward(quote(a))
  })
  nested <- list(
    call = rlang::new_quosure(
      quote(marginplyr::grouping_set({{ make() }})), caller
    ),
    active = rlang::new_quosure(
      quote(marginplyr::grouping_set({{ act }})), caller
    ),
    uncaptured = rlang::new_quosure(
      quote(marginplyr::grouping_set({{ x }})), overrun
    ),
    injected = rlang::new_quosure(
      quote(marginplyr::grouping_set(!!x)), overrun
    )
  )
  for (name in names(nested)) {
    expect_error(
      inspect_nested(capture_lazy(), nested[[name]]),
      typed_context_refusal, fixed = TRUE, class = "marginplyr_error",
      info = name
    )
  }
  expect_identical(calls$n, 0L)
  expect_identical(
    inspect_nested(capture_input(), nested$active)$included, list("a", "b")
  )
  expect_identical(calls$n, 1L)
})

test_that("an embraced argument forwarding a quosure is read through it", {
  forwarded <- rlang::inject(local({
    capture <- function(x) environment()
    capture(!!rlang::quo(a))
  }))
  nested <- rlang::new_quosure(
    quote(marginplyr::grouping_set({{ x }})), forwarded
  )
  expect_identical(
    inspect_nested(capture_lazy(), nested)$included, list("a", "b")
  )
})

test_that("an injected quosure operand is read from names", {
  operand <- rlang::quo(quote(a))
  nested <- rlang::new_quosure(rlang::call2(
    quote(marginplyr::grouping_set),
    rlang::call2("!", rlang::call2("!", operand))
  ))
  expect_identical(
    inspect_nested(capture_lazy(), nested)$included, list("a", "b")
  )
})

# Each operand fails when it runs, and needs no column type to do so. A SQL
# input therefore reports the caller's own failure, as a local frame does,
# rather than the refusal of a typed context the operand never needed.
test_that("a failing name-only injection keeps the caller's own error", {
  absent <- local({
    capture <- function(x) environment()
    capture()
  })
  nested <- list(
    unbound = rlang::new_quosure(
      quote(marginplyr::grouping_set(!!nowhere)), globalenv()
    ),
    missing_splice = rlang::new_quosure(
      quote(marginplyr::grouping_set(!!!x)), absent
    ),
    # `list(quote(a), )` written as a splice operand, which R refuses for its
    # empty second argument.
    empty_element = rlang::new_quosure(rlang::call2(
      quote(marginplyr::grouping_set),
      rlang::call2("!", rlang::call2("!", rlang::call2(
        "!", rlang::call2("list", quote(quote(a)), rlang::missing_arg())
      )))
    ), globalenv())
  )
  expected <- c(
    unbound = "object 'nowhere' not found",
    missing_splice = "argument \"x\" is missing, with no default",
    empty_element = "argument 2 is empty"
  )
  for (name in names(nested)) {
    local_error <- expect_error(
      inspect_nested(capture_input(), nested[[name]]),
      expected[[name]], fixed = TRUE, info = name
    )
    lazy_error <- expect_error(
      inspect_nested(capture_lazy(), nested[[name]]),
      expected[[name]], fixed = TRUE, info = name
    )
    expect_identical(
      conditionMessage(lazy_error), conditionMessage(local_error), info = name
    )
  }
})

test_that("a selection callee bound to a missing argument keeps R's error", {
  data <- capture_input()
  select_with <- function(pick) {
    inspect_grouping(data, .grouping = grouping_set(pick()))
  }
  expect_error(
    select_with(), "argument \"pick\" is missing, with no default",
    fixed = TRUE
  )
})
