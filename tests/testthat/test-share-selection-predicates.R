test_that("Total-share across refuses an aliased predicate helper", {
  data <- data.frame(region = c("a", "a"), site = c("x", "y"), value = c(1, 3))
  predicate_helper <- tidyselect::where

  error <- expect_error(
    summarize_with_margins(
      data,
      total = sum(value),
      dplyr::across(
        predicate_helper(is.numeric),
        share_of_total,
        .names = "{.col}_share"
      ),
      .grouping = rollup(region, site)
    ),
    "only supports name-based tidyselect",
    class = "marginplyr_error"
  )
  expect_match(rlang::expr_text(error$call), "predicate_helper", fixed = TRUE)
})

test_that("Parent-share selections never apply composed predicates", {
  data <- data.frame(group = c("a", "b"), value = c(1, 3))
  observed <- new.env(parent = emptyenv())
  observed$calls <- 0L
  predicate <- tidyselect::where(function(x) {
    observed$calls <- observed$calls + 1L
    is.numeric(x)
  })
  summarize <- function(selection) {
    summarize_with_margins(
      data,
      total = sum(value),
      dplyr::across({{ selection }}, share_of_parent, .names = "{.col}_share"),
      .grouping = rollup(group)
    )
  }

  expect_error(
    summarize(function(x) is.numeric(x)),
    "only supports name-based tidyselect",
    class = "marginplyr_error"
  )
  error <- expect_error(
    summarize(!.env$predicate),
    "only supports name-based tidyselect",
    class = "marginplyr_error"
  )
  expect_match(rlang::expr_text(error$parent$call), "!.env$predicate",
               fixed = TRUE)
  expect_error(
    summarize(-.env$predicate),
    "only supports name-based tidyselect",
    class = "marginplyr_error"
  )
  expect_error(
    summarize(c(total, .env$predicate)),
    "only supports name-based tidyselect",
    class = "marginplyr_error"
  )
  expect_identical(observed$calls, 0L)
})

test_that("lazy Total-share across refuses an injected predicate", {
  skip_if_suggest_absent("dtplyr")
  data <- dtplyr::lazy_dt(data.frame(group = c("a", "b"), value = c(1, 3)))
  predicate <- tidyselect::where(is.numeric)

  expect_error(
    summarize_with_margins(
      data,
      total = sum(value),
      dplyr::across(!!predicate, share_of_total, .names = "{.col}_share"),
      .grouping = rollup(group)
    ),
    "only supports name-based tidyselect",
    class = "marginplyr_error"
  )
})

test_that("share selections preserve order, empty results, and ratios", {
  data <- data.frame(
    region = c("a", "a", "b"), site = c("x", "y", "z"),
    value = c(1, 3, 8), weight = c(3, 1, 4)
  )
  result <- summarize_with_margins(
    data,
    total = sum(value),
    mass = sum(weight),
    dplyr::across(
      c(dplyr::all_of("mass"), total),
      share_of_parent,
      .names = "{.col}_parent"
    ),
    dplyr::across(
      dplyr::everything() & !dplyr::starts_with("m"),
      share_of_total,
      .names = "{.col}_whole"
    ),
    dplyr::across(
      dplyr::any_of("absent"),
      share_of_total,
      .names = "{.col}_empty"
    ),
    .grouping = rollup(region, site)
  )

  expect_named(
    result,
    c("region", "site", "total", "mass", "mass_parent", "total_parent",
      "total_whole")
  )
  leaf <- result[result$site %in% c("x", "y", "z"), ]
  expect_equal(leaf$mass_parent, c(0.75, 0.25, 1))
  expect_equal(leaf$total_parent, c(0.25, 0.75, 1))
  expect_equal(leaf$total_whole, c(1 / 12, 0.25, 2 / 3))
})

test_that("ordinary across still supports predicates", {
  data <- data.frame(group = c("a", "b"), value = c(1, 3), label = c("x", "y"))
  result <- summarize_with_margins(
    data,
    dplyr::across(dplyr::where(is.numeric), sum),
    .grouping = rollup(group)
  )
  expect_named(result, c("group", "value"))
  expect_equal(result$value, c(1, 3, 4))
})
