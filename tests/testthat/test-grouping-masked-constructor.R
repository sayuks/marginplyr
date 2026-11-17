# A constructor name in `.grouping` that resolves to another package's
# function (#1700). The stand-in is a function enclosed by the stats
# namespace, so ordinary lookup finds a package's function under the
# constructor's name without attaching any package.

masked_constructor_data <- function() {
  data.frame(region = "East", store = "A", value = 1)
}

# Evaluates its first argument the way an S3 generic dispatching on it does,
# which is what makes data.table's `rollup()` fail on a bare column name.
masking_constructor <- function() {
  constructor <- function(x, ...) x
  environment(constructor) <- asNamespace("stats")
  constructor
}

expect_masked_refusal <- function(call, name, package) {
  error <- expect_error(call, class = "marginplyr_error")
  message <- conditionMessage(error)
  expect_match(
    message,
    paste0(
      "`", name, "()` in `.grouping` resolves to `", package, "::", name, "()`"
    ),
    fixed = TRUE
  )
  expect_match(message, paste0("`marginplyr::", name, "()`"), fixed = TRUE)
  invisible(error)
}

test_that("a masked constructor in .grouping is refused on every entry point", {
  data <- masked_constructor_data()
  rollup <- masking_constructor()
  cube <- masking_constructor()
  for (verb in names(forwarded_verbs)) {
    expect_masked_refusal(
      forwarded_verbs[[verb]](data, NULL, rollup(region, store)),
      "rollup", "stats"
    )
    expect_masked_refusal(
      forwarded_verbs[[verb]](data, NULL, cube(region, store)),
      "cube", "stats"
    )
  }
})

test_that("a masked constructor nested in a specification is refused", {
  data <- masked_constructor_data()
  rollup <- masking_constructor()
  cube <- masking_constructor()
  for (verb in names(forwarded_verbs)) {
    expect_masked_refusal(
      forwarded_verbs[[verb]](data, NULL, grouping_sets(region, rollup(store))),
      "rollup", "stats"
    )
    expect_masked_refusal(
      forwarded_verbs[[verb]](data, NULL, grouping_spec(region, cube(store))),
      "cube", "stats"
    )
  }
})

test_that("a masked constructor's non-specification value is refused", {
  data <- masked_constructor_data()
  rollup <- function(...) "region"
  environment(rollup) <- asNamespace("stats")
  expect_masked_refusal(
    inspect_grouping(data, .grouping = rollup(region)),
    "rollup", "stats"
  )
  expect_masked_refusal(
    inspect_grouping(data, .grouping = grouping_sets(store, rollup(region))),
    "rollup", "stats"
  )
})

# `tbl_lazy()` has no connection to send a query to, so a refusal reached by
# reading data would be a different error rather than this one.
test_that("a lazy input is refused before any query", {
  remote <- dbplyr::tbl_lazy(
    masked_constructor_data(),
    con = dbplyr::simulate_postgres()
  )
  rollup <- masking_constructor()
  expect_masked_refusal(
    summarize_with_margins(
      remote, total = sum(value), .grouping = rollup(region, store)
    ),
    "rollup", "stats"
  )
  expect_masked_refusal(
    inspect_grouping(remote, .grouping = grouping_sets(rollup(region))),
    "rollup", "stats"
  )
})

test_that("every accepted spelling reads as before under a mask", {
  data <- masked_constructor_data()
  expected <- inspect_grouping(data, .grouping = marginplyr::rollup(region))
  rollup <- masking_constructor()
  expect_identical(
    inspect_grouping(data, .grouping = marginplyr::rollup(region)),
    expected
  )
  expect_identical(
    inspect_grouping(
      data, .grouping = grouping_sets(marginplyr::rollup(region))
    )$included,
    inspect_grouping(
      data, .grouping = grouping_sets(grouping_set(region), grouping_set())
    )$included
  )
  # A name bound to a specification is a value, whatever the mask.
  spec <- marginplyr::rollup(region)
  expect_identical(inspect_grouping(data, .grouping = spec), expected)
})

# A caller's own function is enclosed by no package namespace, so what it
# raises or returns is the caller's to read (ADR 0019).
test_that("a caller's own constructor is read as before", {
  data <- masked_constructor_data()
  caller_env <- rlang::new_environment(parent = globalenv())
  rollup <- function(...) marginplyr::rollup(...)
  environment(rollup) <- caller_env
  expect_identical(
    inspect_grouping(data, .grouping = rollup(region)),
    inspect_grouping(data, .grouping = marginplyr::rollup(region))
  )

  rollup <- function(...) stop("builder failed")
  environment(rollup) <- caller_env
  failing <- list(quote(rollup(region)), quote(grouping_sets(rollup(region))))
  for (grouping in failing) {
    error <- expect_error(inspect_grouping(data, .grouping = !!grouping))
    expect_false(inherits(error, "marginplyr_error"))
    expect_identical(conditionMessage(error), "builder failed")
  }
})

test_that("data.table's rollup() and cube() are refused with the remedy", {
  skip_if_suggest_absent("data.table")
  data <- masked_constructor_data()
  rollup <- data.table::rollup
  cube <- data.table::cube
  expect_masked_refusal(
    summarize_with_margins(
      data, total = sum(value), .grouping = rollup(region, store)
    ),
    "rollup", "data.table"
  )
  expect_masked_refusal(
    summarize_with_margins(
      data, total = sum(value), .grouping = cube(region, store)
    ),
    "cube", "data.table"
  )
  expect_masked_refusal(
    summarize_with_margins(
      data, total = sum(value), .grouping = grouping_sets(rollup(region, store))
    ),
    "rollup", "data.table"
  )
})

test_that("the masked-constructor refusal names the call and its cause", {
  data <- masked_constructor_data()
  rollup <- masking_constructor()
  expect_snapshot(error = TRUE, {
    summarize_with_margins(data, n = dplyr::n(), .grouping = rollup(region))
    inspect_grouping(data, .grouping = grouping_sets(store, rollup(region)))
  })
})
