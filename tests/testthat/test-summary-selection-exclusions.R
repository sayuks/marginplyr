summary_exclusion_data <- function() {
  tibble::tibble(x = c(1, 2), y = c(-3, -4), z = c(100, 101))
}

test_that("across keeps the order of parenthesized exclusions", {
  data <- summary_exclusion_data()
  y_suffix <- "y"
  x_prefix <- "x"
  selectors <- rlang::exprs(
    parenthesized = c(
      !tidyselect::ends_with(y_suffix), (-tidyselect::starts_with(x_prefix))
    ),
    bare = c(
      !tidyselect::ends_with(y_suffix), -tidyselect::starts_with(x_prefix)
    ),
    parenthesized_range = c(!y, (-x:-z)),
    bare_range = c(!y, -x:-z)
  )
  for (form in names(selectors)) {
    selection <- selectors[[form]]
    selected <- names(tidyselect::eval_select(selection, data))
    fixed <- inspect_grouping(data, .by = !!selection, .format = "list")
    grouped <- inspect_grouping(
      data, .grouping = grouping_set(!!selection), .format = "list"
    )
    expect_identical(fixed$fixed[[1L]], selected, info = form)
    expect_identical(grouped$included[[1L]], selected, info = form)
    expected <- dplyr::summarise(data, dplyr::across(
      tidyselect::all_of(selected), sum
    ))
    actual <- summarize_with_margins(data, dplyr::across(!!selection, sum))
    expect_identical(actual, expected, info = form)
  }
})

test_that("ordinary summaries preserve exclusions with an empty helper", {
  data <- summary_exclusion_data()
  selectors <- rlang::exprs(
    parenthesized = -c(tidyselect::any_of("absent"), (-x)),
    bare = -c(tidyselect::any_of("absent"), -x)
  )
  for (form in names(selectors)) {
    selection <- selectors[[form]]
    consumers <- rlang::exprs(
      across = dplyr::across(!!selection, sum),
      pick = sum(dplyr::pick(!!selection)),
      if_any = sum(dplyr::if_any(!!selection, ~ .x > 10)),
      if_all = sum(dplyr::if_all(!!selection, ~ .x > 0))
    )
    for (consumer in names(consumers)) {
      summary <- consumers[consumer]
      if (identical(consumer, "across")) names(summary) <- NULL
      for (prefix in list(list(), rlang::exprs(rows = dplyr::n()))) {
        expected <- dplyr::summarise(data, !!!prefix, !!!summary)
        actual <- summarize_with_margins(data, !!!prefix, !!!summary)
        expect_identical(actual, expected,
                         info = paste(form, consumer, length(prefix)))
      }
    }
  }
})

expect_lazy_summary_exclusions <- function(input, data, class) {
  selectors <- list(
    parenthesized = quote(c(!y, (-x))),
    bare = quote(c(!y, -x)),
    nested_empty = quote(-c(tidyselect::any_of("absent"), (-x))),
    bare_empty = quote(-c(tidyselect::any_of("absent"), -x))
  )
  prefixes <- list(list(), list(first = quote(sum(x, na.rm = TRUE))))
  for (form in names(selectors)) {
    selection <- selectors[[form]]
    selected <- names(tidyselect::eval_select(selection, data))
    for (prefix in prefixes) {
      expected <- dplyr::summarise(
        data, !!!prefix, dplyr::across(tidyselect::all_of(selected), sum)
      )
      query <- summarize_with_margins(
        input, !!!prefix, dplyr::across(!!selection, ~sum(.x, na.rm = TRUE))
      )
      expect_s3_class(query, class)
      expect_equal(dplyr::collect(query), expected,
                   info = paste(form, length(prefix)))
    }
  }
}

test_that("dtplyr keeps parenthesized summary exclusions lazy", {
  skip_if_suggest_absent("dtplyr")
  data <- summary_exclusion_data()
  expect_lazy_summary_exclusions(dtplyr::lazy_dt(data), data, "dtplyr_step")
})

test_that("SQLite keeps parenthesized summary exclusions lazy", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  data <- summary_exclusion_data()
  expect_lazy_summary_exclusions(dplyr::copy_to(con, data), data, "tbl_lazy")
})

test_that("DuckDB keeps parenthesized summary exclusions lazy", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  data <- summary_exclusion_data()
  expect_lazy_summary_exclusions(dplyr::copy_to(con, data), data, "tbl_lazy")
})

test_that("Arrow keeps parenthesized summary exclusions lazy", {
  skip_if_suggest_absent("arrow")
  data <- summary_exclusion_data()
  expect_lazy_summary_exclusions(arrow::Table$create(data), data,
                                 "arrow_dplyr_query")
})
