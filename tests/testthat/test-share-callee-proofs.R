test_that("scope proofs unwrap parenthesized callees without evaluating them", {
  scope <- rlang::env(rm = function(...) stop("Caller function ran"))
  lookup <- list(env = scope, rebound = character(), mask = character())

  callee <- rlang::call2("(", quote(rm))
  expect_true(scope_callee_is_shadowed(callee, "rm", character(), lookup))
  expect_false(scope_callee_is_shadowed(
    quote((base::rm)), "rm", character(), lookup
  ))
  expect_null(scope_mutation_call(quote(do.call()), character(), lookup))
})

test_that("scope proofs distinguish resolver ownership and binding certainty", {
  scope <- rlang::env(rm = function(...) stop("Caller function ran"))
  lookup <- list(env = scope, rebound = character(), mask = character())

  expect_false(scope_callee_is_shadowed(
    quote(factory("rm")), "rm", character(), lookup
  ))
  expect_false(scope_callee_is_shadowed(
    quote(get("rm")), "rm", "get", lookup
  ))
  rebound <- lookup
  rebound$rebound <- "get"
  expect_false(scope_callee_is_shadowed(
    quote(get("rm")), "rm", character(), rebound
  ))
  expect_true(scope_callee_is_shadowed(
    quote(base::get("rm")), "rm", character(), lookup
  ))
  expect_true(scope_callee_is_shadowed(
    quote(utils::get("rm")), "rm", character(), lookup
  ))
  scope$get <- function(...) stop("Caller resolver ran")
  expect_true(scope_callee_is_shadowed(
    quote(get("rm")), "rm", character(), lookup
  ))
  scope$get <- base::get
  scope$rm <- base::rm
  expect_false(scope_callee_is_shadowed(
    quote(base::get("rm")), "rm", character(), lookup
  ))
})

test_that("scope proofs never force an unresolved resolver binding", {
  for (mode in c("active", "lazy")) {
    calls <- 0L
    scope <- rlang::env()
    callback <- function() {
      calls <<- calls + 1L
      stop("Caller resolver binding ran")
    }
    if (mode == "active") {
      makeActiveBinding("get", callback, scope)
    } else {
      delayedAssign("get", callback(), assign.env = scope)
    }
    lookup <- list(env = scope, rebound = character(), mask = character())
    expect_false(scope_callee_is_shadowed(
      quote(get("rm")), "rm", character(), lookup
    ))
    expect_identical(calls, 0L)
  }
})
