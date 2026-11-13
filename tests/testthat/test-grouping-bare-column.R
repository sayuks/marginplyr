# A column written where `.grouping` expects a Grouping specification (#1638).

bare_column_data <- function() {
  data.frame(region = "East", store = "A", value = 1)
}

bare_column_lines <- function(columns, written) {
  c(
    paste0(
      "`.grouping` must be created with `grouping_set()`, ",
      "`grouping_sets()`, `rollup()`, `cube()`, or `grouping_spec()`."
    ),
    paste0(
      "i ", columns, " must be wrapped in a constructor, for example ",
      "`rollup(", written, ")` or `grouping_sets(", written, ")`."
    ),
    "Caused by error:"
  )
}

expect_bare_column_refusal <- function(call, columns, written, missing) {
  error <- expect_error(call, class = "marginplyr_error")
  lines <- strsplit(conditionMessage(error), "\n", fixed = TRUE)[[1L]]
  expect_identical(lines[1:3], bare_column_lines(columns, written))
  expect_false(inherits(error$parent, "marginplyr_error"))
  expect_identical(
    conditionMessage(error$parent),
    paste0("object '", missing, "' not found")
  )
  invisible(error)
}

test_that("a bare column as .grouping is refused on every entry point", {
  data <- bare_column_data()
  for (verb in names(forwarded_verbs)) {
    expect_bare_column_refusal(
      forwarded_verbs[[verb]](data, NULL, region),
      "Column `region`", "region", "region"
    )
    expect_bare_column_refusal(
      forwarded_verbs[[verb]](data, NULL, c(region, store)),
      "Columns `region` and `store`", "region, store", "region"
    )
  }
})

test_that("the bare-column refusal names the call and its cause", {
  data <- bare_column_data()
  expect_snapshot(error = TRUE, {
    summarize_with_margins(data, n = dplyr::n(), .grouping = region)
    inspect_grouping(data, .grouping = c(region, store))
  })
})

# `tbl_lazy()` has no connection to send a query to, so a refusal reached by
# reading data would be a different error rather than this one. The nesting
# verbs refuse a lazy input before reading `.grouping`, so they are not asked.
test_that("a lazy input is refused from its metadata alone", {
  remote <- dbplyr::tbl_lazy(
    bare_column_data(),
    con = dbplyr::simulate_postgres()
  )
  lazy_verbs <- setdiff(
    names(forwarded_verbs), c("nest_with_margins", "nest_by_with_margins")
  )
  for (verb in lazy_verbs) {
    expect_bare_column_refusal(
      forwarded_verbs[[verb]](remote, NULL, c(region, store)),
      "Columns `region` and `store`", "region, store", "region"
    )
  }
})

test_that("a dtplyr input is refused the same way", {
  skip_if_suggest_absent("dtplyr")
  lazy <- dtplyr::lazy_dt(bare_column_data())
  for (verb in names(forwarded_verbs)) {
    expect_bare_column_refusal(
      forwarded_verbs[[verb]](lazy, NULL, region),
      "Column `region`", "region", "region"
    )
  }
})

test_that("an Arrow input is refused the same way", {
  skip_if_suggest_absent("arrow")
  table <- arrow::arrow_table(bare_column_data())
  for (verb in c("summarize_with_margins", "inspect_grouping")) {
    expect_bare_column_refusal(
      forwarded_verbs[[verb]](table, NULL, region),
      "Column `region`", "region", "region"
    )
  }
})

test_that("a specification bound to a column's name is still read", {
  data <- bare_column_data()
  region <- rollup(store)
  expect_identical(
    inspect_grouping(data, .grouping = region)$included,
    inspect_grouping(data, .grouping = rollup(store))$included
  )
})

test_that("other failures in .grouping propagate unchanged", {
  data <- bare_column_data()
  build <- function() stop("builder failed")
  error <- expect_error(inspect_grouping(data, .grouping = build()))
  expect_false(inherits(error, "marginplyr_error"))
  expect_identical(conditionMessage(error), "builder failed")

  # A name that is not a column, and a `c()` mixing one in, are not a column
  # written in place of a specification.
  for (written in list(quote(regoin), quote(c(region, regoin)))) {
    error <- expect_error(
      inspect_grouping(data, .grouping = !!written),
      "object '(regoin|region)' not found"
    )
    expect_false(inherits(error, "marginplyr_error"))
  }
})
