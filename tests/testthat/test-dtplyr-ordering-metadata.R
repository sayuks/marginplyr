test_that("filtered dtplyr ordering preserves nesting keys and membership", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(g = c("b", "a"), value = 1:2)
  before <- data.table::copy(source)
  step <- dtplyr::lazy_dt(source) |>
    dplyr::filter(value > 0L) |>
    dplyr::arrange(g)

  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    for (keep in c(FALSE, TRUE)) {
      for (sort in c("first", "last")) {
        result <- verb(
          step, .grouping = rollup(g), .keep = keep,
          .id = "set", .sort = sort
        )
        if (identical(verb, nest_with_margins)) {
          expect_s3_class(result, "dtplyr_step")
          result <- dplyr::collect(result)
        } else {
          expect_s3_class(result, "rowwise_df")
          expect_identical(dplyr::group_vars(result), c("g", "set"))
        }
        total <- if (identical(sort, "first")) 1L else 3L
        detail <- setdiff(1:3, total)
        expect_identical(result$g[total], "Total")
        expect_identical(result$g[detail], c("a", "b"))
        expect_identical(result$set[total], 2L)
        expect_identical(result$set[detail], c(1L, 1L))
        expect_identical(result$data[[total]]$value, 2:1)
        expect_identical(lapply(result$data[detail], function(x) x$value),
                         list(2L, 1L))
        expect_identical(names(result$data[[total]]),
                         if (keep) c("g", "value") else "value")
        if (keep) {
          expect_identical(result$data[[total]]$g, c("a", "b"))
        }
        identities <- inspect_grouping(step, .grouping = rollup(g))
        expect_identical(identities$grouping_id[result$set],
                         if (identical(sort, "first")) c(1L, 0L, 0L)
                         else c(0L, 0L, 1L))
        expect_identical(source, before)
      }
    }
  }
})

ordering_metadata_cases <- function(immutable, predicate) {
  source <- data.table::data.table(
    g = c("b", "a", "b", "a"), value = 1:4, discard = FALSE,
    day = as.Date("2026-01-01") + 0:3,
    category = factor(c("x", "y", "x", "y"),
                      levels = c("y", "x", "unused"))
  )
  left <- data.table::data.table(g = c("b", "a", "b", "a"), value = 1:4)
  right <- data.table::data.table(g = c("a", "b"), weight = 5:6)
  root <- dtplyr::lazy_dt(source, immutable = immutable)
  steps <- list(
    filter = root,
    select = dplyr::select(root, g, value, day, category),
    rename = dplyr::rename(root, renamed = category),
    relocate = dplyr::relocate(root, category),
    mutate = dplyr::mutate(root, doubled = value * 2L),
    join = dplyr::left_join(
      dtplyr::lazy_dt(left, immutable = immutable),
      dplyr::filter(dtplyr::lazy_dt(right, immutable = immutable),
                    predicate(weight)), by = "g"
    )
  )
  list(
    sources = list(source, left, right),
    steps = lapply(steps, function(x) dplyr::filter(x, predicate(value)))
  )
}

test_that("sorted derivations expose faithful typed metadata without row work", {
  skip_if_suggest_absent("dtplyr")
  seen <- integer()
  probe <- function(x) {
    seen <<- c(seen, length(x))
    x > 0L
  }
  controls <- ordering_metadata_cases(TRUE, function(x) x > 0L)
  predicates <- list(
    is.integer, is.factor, function(x) inherits(x, "Date"),
    function(x) identical(levels(x), c("y", "x", "unused"))
  )
  orders <- list(
    function(x) dplyr::arrange(x, g),
    function(x) dplyr::arrange(x, dplyr::desc(g), value)
  )

  for (immutable in c(TRUE, FALSE)) {
    cases <- ordering_metadata_cases(immutable, probe)
    before <- lapply(cases$sources, data.table::copy)
    for (name in names(cases$steps)) {
      for (order in orders) {
        step <- order(cases$steps[[name]])
        materialized <- dplyr::collect(order(controls$steps[[name]]))
        expect_identical(
          inspect_grouping(step, .grouping = rollup(g)),
          inspect_grouping(materialized, .grouping = rollup(g))
        )
        for (predicate in predicates) {
          expect_identical(
            inspect_grouping(step, .grouping = grouping_set(where(predicate))),
            inspect_grouping(materialized,
                             .grouping = grouping_set(where(predicate)))
          )
          expect_identical(
            inspect_grouping(step, .by = where(predicate)),
            inspect_grouping(materialized, .by = where(predicate))
          )
        }
        expect_identical(seen, integer())
        expect_identical(cases$sources, before)
      }
    }
  }
})

test_that("name-only inspection executes no sorted dtplyr step", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(g = c("b", "a"), value = 1:2)
  before <- data.table::copy(source)
  expected <- inspect_grouping(source, .grouping = rollup(g))
  testthat::local_mocked_bindings(
    dt_eval = function(...) stop("name-only inspection executed a step"),
    .package = "dtplyr"
  )

  for (immutable in c(TRUE, FALSE)) {
    root <- dtplyr::lazy_dt(source, immutable = immutable)
    for (step in list(root, dplyr::filter(root, value > 0L))) {
      step <- dplyr::arrange(step, dplyr::desc(g), value)
      expect_identical(inspect_grouping(step, .grouping = rollup(g)), expected)
      expect_identical(source, before)
    }
  }
})

test_that("sorted derivations share lazy Margin preparation", {
  skip_if_suggest_absent("dtplyr")
  seen <- integer()
  probe <- function(x) {
    seen <<- c(seen, length(x))
    x > 0L
  }
  cases <- ordering_metadata_cases(TRUE, probe)
  before <- lapply(cases$sources, data.table::copy)
  for (step in cases$steps) {
    step <- dplyr::arrange(step, dplyr::desc(g), value)
    query <- nest_with_margins(
      step, .grouping = rollup(g), .keep = TRUE, .sort = "last", .id = "set"
    )
    expansion <- expand_with_margins(step, .grouping = rollup(g), .id = "set")
    summary <- summarize_with_margins(
      step, total = sum(value), mask = grouping_id(g),
      .grouping = rollup(g), .id = "set", .sort = "last"
    )
    expect_s3_class(query, "dtplyr_step")
    expect_s3_class(expansion, "dtplyr_step")
    expect_s3_class(summary, "dtplyr_step")
    expect_identical(seen, integer())
    expect_identical(cases$sources, before)

    result <- dplyr::collect(query)
    expect_true(any(seen > 0L))
    expect_identical(result$g, c("a", "b", "Total"))
    expect_identical(result$set, c(1L, 1L, 2L))
    expect_identical(lapply(result$data, function(x) x$value),
                     list(c(2L, 4L), c(1L, 3L), c(1L, 3L, 2L, 4L)))
    expanded <- dplyr::collect(expansion)
    expect_identical(sort(expanded$value[expanded$set == 1L]), 1:4)
    expect_identical(sort(expanded$value[expanded$set == 2L]), 1:4)
    summarized <- dplyr::collect(summary)
    expect_identical(summarized$g, c("a", "b", "Total"))
    expect_identical(summarized$total, c(6L, 4L, 10L))
    expect_identical(summarized$mask, c(0L, 0L, 1L))

    seen <- integer()
    rowwise <- nest_by_with_margins(
      step, .grouping = rollup(g), .keep = TRUE, .sort = "last", .id = "set"
    )
    expect_s3_class(rowwise, "rowwise_df")
    expect_true(any(seen > 0L))
    expect_identical(dplyr::ungroup(rowwise), result)
    expect_identical(cases$sources, before)
    seen <- integer()
  }
})

test_that("ordering does not admit unsafe metadata or Mutable Margin inputs", {
  skip_if_suggest_absent("dtplyr")
  seen <- integer()
  probe <- function(x) {
    seen <<- c(seen, length(x))
    x
  }
  source <- data.table::data.table(g = c("b", "a"), value = 1:2)
  before <- data.table::copy(source)
  root <- dtplyr::lazy_dt(source)
  unsafe <- list(
    dplyr::mutate(root, value = probe(value)),
    dplyr::mutate(root, value = factor(value)),
    dplyr::mutate(root, value = if (dplyr::n() > 0L) "text" else 0L),
    dplyr::mutate(root, value = "text")
  )
  for (step in unsafe) {
    step <- dplyr::arrange(step, g)
    for (verb in forwarded_margin_verbs) {
      expect_error(
        verb(step, NULL, rollup(g)),
        "Can't safely determine column metadata", class = "marginplyr_error"
      )
    }
    expect_error(
      inspect_grouping(step, .grouping = rollup(where(is.numeric))),
      "Can't safely determine column metadata", class = "marginplyr_error"
    )
    expect_identical(nrow(inspect_grouping(step, .grouping = rollup(g))), 2L)
  }
  expect_identical(seen, integer())
  expect_identical(source, before)

  cases <- ordering_metadata_cases(FALSE, function(x) x > 0L)
  before <- lapply(cases$sources, data.table::copy)
  for (step in cases$steps) {
    step <- dplyr::arrange(step, dplyr::desc(g), value)
    for (verb in forwarded_margin_verbs) {
      expect_error(verb(step, NULL, rollup(g)),
                   "immutable = TRUE", class = "marginplyr_error")
    }
    expect_identical(cases$sources, before)
  }
})
