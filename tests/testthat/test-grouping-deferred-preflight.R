# A nested constructor whose injection runs caller code is read only against
# the typed snapshot, so the preflight suspends there and the typed pass
# completes it (ADR 0008). These pin what the suspension must not change: the
# arguments read before it, the refusals the suspended argument earns, and the
# duplicate refusal that the arguments before it already settle (ADR 0005).

deferred_input <- function() {
  data.frame(a = c("x", "y"), b = 1:2, c = c(TRUE, FALSE))
}

# The nested constructor arrives as a quosure, so its injection reaches the
# preflight unprocessed; written inline, the outer constructor's capture would
# already have run it.
deferred_set <- function() {
  # Read only by the quoted injection below, which codetools cannot see.
  pick <- function() quote(c) # nolint: object_usage_linter.
  rlang::new_quosure(quote(marginplyr::grouping_set(!!pick())))
}

test_that("completing a suspended preflight keeps the arguments before it", {
  data <- deferred_input()
  actual <- inspect_grouping(
    data, .grouping = grouping_sets(b, grouping_spec(a, b), !!deferred_set())
  )
  expected <- inspect_grouping(
    data, .grouping = grouping_sets(b, grouping_spec(a, b), grouping_set(c))
  )
  expect_identical(actual, expected)
  expect_identical(
    inspect_grouping(data, .grouping = grouping_sets(
      b, grouping_spec(a, b), !!deferred_set()
    ), .format = "list")$included,
    list("b", c("b", "a"), "c")
  )
})

test_that("a suspended family inside a set or a rollup is refused as written", {
  data <- deferred_input()
  deferred <- deferred_set()
  cases <- list(
    list(
      deferred = rlang::quo(grouping_set(a, grouping_sets(!!deferred))),
      direct = rlang::quo(grouping_set(a, grouping_sets(c)))
    ),
    list(
      deferred = rlang::quo(rollup(a, grouping_sets(!!deferred))),
      direct = rlang::quo(rollup(a, grouping_sets(c)))
    ),
    list(
      deferred = rlang::quo(cube(a, grouping_sets(!!deferred))),
      direct = rlang::quo(cube(a, grouping_sets(c)))
    )
  )
  for (case in cases) {
    direct <- expect_error(
      inspect_grouping(data, .grouping = !!case$direct),
      class = "marginplyr_error"
    )
    deferred_error <- expect_error(
      inspect_grouping(data, .grouping = !!case$deferred),
      class = "marginplyr_error"
    )
    expect_identical(
      conditionMessage(deferred_error), conditionMessage(direct)
    )
  }
})

test_that("leading products settle a duplicate before a typed selection", {
  data <- deferred_input()
  calls <- rlang::env(n = 0L)
  counted <- function(x) {
    calls$n <- calls$n + 1L
    is.logical(x)
  }
  expect_error(
    inspect_grouping(data, .grouping = grouping_sets(
      grouping_spec(a, b), grouping_spec(b, a), where(counted)
    )),
    "Duplicate grouping sets were produced", class = "marginplyr_error"
  )
  expect_identical(calls$n, 0L)

  expect_identical(
    inspect_grouping(data, .grouping = grouping_sets(
      grouping_spec(a, b), grouping_spec(a, c), where(counted)
    ), .format = "list")$included,
    list(c("a", "b"), c("a", "c"), "c")
  )
  expect_gt(calls$n, 0L)
})
