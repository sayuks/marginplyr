# A computed expression where `.by` or a constructor selects columns (#1703).
# tidyselect evaluates such a call without the columns in scope, so it failed
# with "object not found" for a column that exists, or -- for a column named
# like a function -- with the function's deparsed body as column names.

computed_selection_data <- function() {
  data.frame(
    region = c("a", "b"),
    day = as.Date(c("2025-01-01", "2026-02-01")),
    dt = 1:2,
    value = 1:2
  )
}

computed_selection_remedy <- paste0(
  "i Create the column with `dplyr::mutate()` first, then select it by name."
)

expect_computed_refusal <- function(call, main, computed) {
  error <- expect_error(call, class = "marginplyr_error")
  lines <- strsplit(conditionMessage(error), "\n", fixed = TRUE)[[1L]]
  expect_identical(lines, c(main, computed, computed_selection_remedy))
  expect_null(error$parent)
  invisible(error)
}

by_main <- "`.by` selects existing columns, not computed expressions."
grouping_main <- paste0(
  "Grouping specification arguments select existing columns, not computed ",
  "expressions."
)

test_that("a computed `.by` is sent to mutate() on every entry point", {
  data <- computed_selection_data()
  for (verb in names(forwarded_verbs)) {
    expect_computed_refusal(
      forwarded_verbs[[verb]](data, toupper(region), rollup(dt)),
      by_main,
      "i `toupper(region)` is computed from `region`."
    )
  }
})

test_that("a computed constructor argument is sent to mutate()", {
  data <- computed_selection_data()
  for (verb in names(forwarded_verbs)) {
    expect_computed_refusal(
      forwarded_verbs[[verb]](data, NULL, rollup(format(day, "%Y"))),
      grouping_main,
      "i `format(day, \"%Y\")` is computed from `day`."
    )
  }
  # Under a selection operator, and inside a nested constructor.
  expect_computed_refusal(
    inspect_grouping(data, .grouping = grouping_sets(c(region, toupper(dt)))),
    grouping_main,
    "i `toupper(dt)` is computed from `dt`."
  )
  expect_computed_refusal(
    inspect_grouping(
      data, .grouping = grouping_sets(rollup(region, format(day)))
    ),
    grouping_main,
    "i `format(day)` is computed from `day`."
  )
})

# `dt` is also `stats::dt()`, which tidyselect found and deparsed.
test_that("a column named like a function gets the same refusal", {
  data <- computed_selection_data()
  error <- expect_computed_refusal(
    inspect_grouping(data, .grouping = rollup(format(dt))),
    grouping_main,
    "i `format(dt)` is computed from `dt`."
  )
  expect_no_match(conditionMessage(error), "function", fixed = TRUE)
})

# `tbl_lazy()` has no connection, so a refusal that read data would fail
# differently.
test_that("a lazy input is refused from its metadata alone", {
  remote <- dbplyr::tbl_lazy(
    computed_selection_data(),
    con = dbplyr::simulate_postgres()
  )
  expect_computed_refusal(
    inspect_grouping(remote, .by = toupper(region), .grouping = rollup(dt)),
    by_main,
    "i `toupper(region)` is computed from `region`."
  )
  expect_computed_refusal(
    inspect_grouping(remote, .grouping = rollup(format(day, "%Y"))),
    grouping_main,
    "i `format(day, \"%Y\")` is computed from `day`."
  )
})

test_that("selection helpers and other failures are left as they were", {
  data <- computed_selection_data()
  expect_identical(
    inspect_grouping(data, .grouping = rollup(starts_with("reg")))$included,
    inspect_grouping(data, .grouping = rollup(region))$included
  )
  missing <- expect_error(
    inspect_grouping(data, .grouping = rollup(all_of("zzz")))
  )
  expect_false(inherits(missing, "marginplyr_error"))
  # A call naming no column of the input is not a computed column.
  unrelated <- expect_error(
    inspect_grouping(data, .grouping = rollup(toupper(nowhere)))
  )
  expect_false(inherits(unrelated, "marginplyr_error"))
})
