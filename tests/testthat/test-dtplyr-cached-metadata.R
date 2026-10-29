test_that("cached row predicates do not run during typed inspection", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(g = c("a", "b"), value = 1:2)
  before <- data.table::copy(source)
  step <- dtplyr::lazy_dt(source) |>
    dplyr::filter(value[[1L]] > 0L) |>
    dplyr::compute() |>
    dplyr::arrange(value)
  collected <- dplyr::collect(step)

  expect_identical(
    inspect_grouping(step, .grouping = rollup(where(is.character))),
    inspect_grouping(collected, .grouping = rollup(where(is.character)))
  )
  expect_identical(source, before)
})

test_that("cached minimum predicates emit no metadata warnings", {
  skip_if_suggest_absent("dtplyr")
  step <- dtplyr::lazy_dt(data.frame(g = c("a", "b"), value = 1:2)) |>
    dplyr::filter(min(value) > 0L) |>
    dplyr::compute() |>
    dplyr::arrange(value)

  expect_silent(inspect_grouping(
    step, .grouping = rollup(where(is.character))
  ))
  expect_silent(nest_with_margins(step, .grouping = rollup(g)))
  expect_silent(nest_by_with_margins(step, .grouping = rollup(g)))
})

test_that("cached predicate compositions preserve metadata and nested rows", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr imports data.table; only dtplyr is optional in these tests.
  normalize <- function(x) {
    x <- dplyr::ungroup(x)
    x$data <- lapply(x$data, tibble::as_tibble)
    x
  }
  predicates <- list(
    function(x) x[[1L]] > 0L,
    function(x) min(x) > 0L
  )

  for (rows in c(1L, 3L)) {
    source <- data.table::data.table(g = letters[seq_len(rows)],
                                     value = seq_len(rows))
    other <- data.table::data.table(g = letters[seq_len(rows)],
                                    weight = seq_len(rows))
    before <- lapply(list(source, other), data.table::copy)
    root <- dtplyr::lazy_dt(source)
    right <- dtplyr::lazy_dt(other)
    derivations <- list(
      function(x) dplyr::arrange(x, value),
      function(x) dplyr::rename(x, amount = value),
      function(x) dplyr::left_join(x, right, by = "g"),
      function(x) dplyr::union_all(x, x)
    )
    for (predicate in predicates) {
      seen <- integer()
      probe <- function(x) {
        seen <<- c(seen, length(x))
        predicate(x)
      }
      filtered <- dplyr::filter(root, probe(value))
      cached <- dplyr::compute(filtered)
      for (derive in derivations) {
        for (step in list(cached, derive(filtered), derive(cached))) {
          expect_silent(materialized <- dplyr::collect(step))
          expect_true(all(seen > 0L))
          seen <- integer()
          expect_silent(plan <- inspect_grouping(
            step, .grouping = rollup(where(is.character))
          ))
          expect_identical(plan, inspect_grouping(
            materialized, .grouping = rollup(where(is.character))
          ))
          # A single occurrence keeps this metadata regression independent of
          # the repeated cached-alias execution defect in #1003.
          spec <- grouping_set(g)
          expect_silent(query <- nest_with_margins(
            step, .grouping = spec, .keep = TRUE, .sort = "last"
          ))
          expect_silent(expansion <- expand_with_margins(
            step, .grouping = spec, .sort = "last"
          ))
          expect_silent(summary <- summarize_with_margins(
            step, n = dplyr::n(), .grouping = spec, .sort = "last"
          ))
          expect_s3_class(query, "dtplyr_step")
          expect_s3_class(expansion, "dtplyr_step")
          expect_s3_class(summary, "dtplyr_step")
          expect_identical(seen, integer())
          expect_identical(list(source, other), before)

          expected <- normalize(nest_with_margins(
            materialized, .grouping = spec, .keep = TRUE, .sort = "last"
          ))
          expect_identical(normalize(dplyr::collect(query)), expected)
          expect_identical(dplyr::collect(expansion), expand_with_margins(
            materialized, .grouping = spec, .sort = "last"
          ))
          expect_identical(dplyr::collect(summary), summarize_with_margins(
            materialized, n = dplyr::n(), .grouping = spec, .sort = "last"
          ))
          seen <- integer()
          expect_silent(rowwise <- nest_by_with_margins(
            step, .grouping = spec, .keep = TRUE, .sort = "last"
          ))
          expect_s3_class(rowwise, "rowwise_df")
          expect_true(length(seen) > 0L && all(seen == rows))
          expect_identical(normalize(rowwise), expected)
          expect_identical(list(source, other), before)
          seen <- integer()
        }
      }
    }
  }
})

test_that("cached ordered filters nest every rollup cell faithfully", {
  skip_if_suggest_absent("dtplyr")
  step <- dtplyr::lazy_dt(data.frame(g = c("b", "a", "b"), value = 1:3)) |>
    dplyr::filter(value[[1L]] > 0L) |>
    dplyr::compute() |>
    dplyr::arrange(value)

  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    result <- verb(step, .grouping = rollup(g), .sort = "last", .keep = TRUE)
    if (identical(verb, nest_with_margins)) {
      result <- dplyr::collect(result)
    }
    expect_identical(result$g, c("a", "b", "Total"))
    expect_identical(lapply(result$data, function(x) x$value),
                     list(2L, c(1L, 3L), 1:3))
    expect_identical(lapply(result$data, function(x) x$g),
                     list("a", c("b", "b"), c("b", "a", "b")))
  }
})

test_that("cached filters retain faithful factor and date metadata", {
  skip_if_suggest_absent("dtplyr")
  source <- data.frame(
    g = factor(c("b", "a"), levels = c("a", "b", "unused")),
    day = as.Date("2026-01-01") + 0:1, value = 1:2
  )
  step <- dtplyr::lazy_dt(source) |>
    dplyr::filter(value[[1L]] > 0L) |>
    dplyr::compute() |>
    dplyr::rename(category = g) |>
    dplyr::arrange(value)
  predicates <- list(is.factor, function(x) inherits(x, "Date"), function(x) {
    identical(levels(x), c("a", "b", "unused"))
  })
  for (predicate in predicates) {
    expect_identical(
      inspect_grouping(step, .grouping = grouping_set(where(predicate))),
      inspect_grouping(dplyr::collect(step),
                       .grouping = grouping_set(where(predicate)))
    )
  }
})

test_that("cached graphs retain unsafe-schema and Mutable-step refusals", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(g = c("a", "b"), value = 1:2)
  before <- data.table::copy(source)
  probe <- function(x) stop("source rows evaluated")
  root <- dtplyr::lazy_dt(source)
  unsafe <- list(
    dplyr::mutate(root, value = probe(value)),
    dplyr::mutate(root, value = factor(value)),
    dplyr::mutate(root, value = if (dplyr::n() > 0L) "text" else 0L)
  )
  for (step in unsafe) {
    step <- dplyr::arrange(dplyr::compute(step), g)
    expect_error(
      inspect_grouping(step, .grouping = rollup(where(is.character))),
      "Can't safely determine column metadata", class = "marginplyr_error"
    )
    for (verb in forwarded_margin_verbs) {
      expect_error(verb(step, NULL, rollup(g)),
                   "Can't safely determine column metadata",
                   class = "marginplyr_error")
    }
  }

  mutable <- dtplyr::lazy_dt(source, immutable = FALSE) |>
    dplyr::filter(probe(value)) |>
    dplyr::compute() |>
    dplyr::arrange(g)
  expect_identical(
    inspect_grouping(mutable, .grouping = rollup(where(is.character))),
    inspect_grouping(tibble::as_tibble(before), .grouping = rollup(g))
  )
  for (verb in forwarded_margin_verbs) {
    expect_error(verb(mutable, NULL, rollup(g)),
                 "immutable = TRUE", class = "marginplyr_error")
  }
  expect_identical(source, before)
})
