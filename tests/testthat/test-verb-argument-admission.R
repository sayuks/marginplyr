# Two admission rules that keep a mistake from becoming a plausible result.
#
# `...` accepts any named expression, so an argument name the verb does not
# have becomes a constant summary column instead of an error; and an input
# without dplyr methods reached `group_vars()` and reported an internal
# generic rather than the argument the caller supplied.

admission_data <- function() {
  data.frame(g = c("a", "a", "b"), v = c(1, 2, 3))
}

test_that("a removed option is reported instead of summarized", {
  expect_error(
    summarize_with_margins(
      admission_data(),
      s = sum(v),
      .grouping = rollup(g),
      .sort = TRUE
    ),
    class = "marginplyr_error"
  )
  expect_error(
    summarize_with_margins(
      admission_data(),
      s = sum(v),
      .grouping = rollup(g),
      .sort = TRUE
    ),
    "no `.sort` argument"
  )
})

test_that("a misspelled option names the argument it resembles", {
  expect_error(
    summarize_with_margins(
      admission_data(),
      s = sum(v),
      .grouping = rollup(g),
      .margin_labels = "ALL"
    ),
    "Did you mean `.margin_label`"
  )
  expect_error(
    summarize_with_margins(
      admission_data(),
      s = sum(v),
      .groupings = rollup(g)
    ),
    "Did you mean `.grouping`"
  )
  expect_error(
    summarize_with_margins(
      admission_data(),
      s = sum(v),
      .grouping = rollup(g),
      .duplicate = "drop"
    ),
    "Did you mean `.duplicates`"
  )
  expect_error(
    summarize_with_margins(
      admission_data(),
      s = sum(v),
      .grouping = rollup(g),
      .ids = "set"
    ),
    "Did you mean `.id`"
  )
})

test_that("the check is scoped to names that resemble an option", {
  # A leading dot is ordinary in an output name, and only an exact match or a
  # one-character difference is treated as a mistake.
  expect_named(
    summarize_with_margins(
      admission_data(),
      .n = dplyr::n(),
      .grouping = rollup(g)
    ),
    c("g", ".n")
  )
  expect_named(
    summarize_with_margins(
      admission_data(),
      .total_by_region = sum(v),
      .grouping = rollup(g)
    ),
    c("g", ".total_by_region")
  )
  expect_named(
    summarize_with_margins(
      admission_data(),
      s = sum(v),
      .grouping = rollup(g),
      .id = ".set"
    ),
    c("g", ".set", "s")
  )
})

test_that("the option check survives splicing", {
  options <- list(.sort = TRUE)

  expect_error(
    summarize_with_margins(
      admission_data(),
      s = sum(v),
      .grouping = rollup(g),
      !!!options
    ),
    class = "marginplyr_error"
  )
})

test_that("input that dplyr cannot group is rejected in the caller's terms", {
  for (input in list(as.matrix(admission_data()), as.list(admission_data()))) {
    expect_error(
      summarize_with_margins(input, s = sum(v), .grouping = rollup(g)),
      class = "marginplyr_error"
    )
    expect_error(
      summarize_with_margins(input, s = sum(v), .grouping = rollup(g)),
      "must be a data frame or a lazy table"
    )
  }

  expect_error(
    summarize_with_margins(NULL, s = sum(v), .grouping = rollup(g)),
    "`NULL` was supplied"
  )
})

test_that("every entry point admits input the same way", {
  input <- as.matrix(admission_data())

  expect_error(
    expand_with_margins(input, .grouping = rollup(g)),
    class = "marginplyr_error"
  )
  expect_error(
    nest_with_margins(input, .grouping = rollup(g)),
    class = "marginplyr_error"
  )
  expect_error(
    nest_by_with_margins(input, .grouping = rollup(g)),
    class = "marginplyr_error"
  )
  expect_error(
    inspect_grouping(input, .grouping = rollup(g)),
    class = "marginplyr_error"
  )
})

test_that("supported backends are still admitted", {
  expect_no_error(
    summarize_with_margins(
      tibble::as_tibble(admission_data()),
      s = sum(v),
      .grouping = rollup(g)
    )
  )
  expect_no_error(
    summarize_with_margins(
      dplyr::group_by(admission_data(), g),
      s = sum(v)
    )
  )

  skip_if_backend_absent("dtplyr")
  expect_no_error(
    dplyr::collect(summarize_with_margins(
      dtplyr::lazy_dt(admission_data()),
      s = sum(v),
      .grouping = rollup(g)
    ))
  )
})

test_that("arrow input is still admitted", {
  skip_if_backend_absent("arrow")

  expect_no_error(
    dplyr::collect(summarize_with_margins(
      arrow::as_arrow_table(admission_data()),
      s = sum(v),
      .grouping = rollup(g)
    ))
  )
})
