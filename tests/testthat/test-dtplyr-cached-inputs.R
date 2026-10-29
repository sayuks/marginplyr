test_that("immutable cached renames survive multiple nesting branches", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(g = c("a", "b", "a"), value = 1:3)
  before <- data.table::copy(source)
  step <- dtplyr::lazy_dt(source) |>
    dplyr::filter(TRUE) |>
    dplyr::compute() |>
    dplyr::rename(amount = value)
  expected_input <- tibble::tibble(g = c("a", "b", "a"), amount = 1:3)
  expect_identical(dplyr::collect(step), expected_input)

  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    for (keep in c(FALSE, TRUE)) {
      for (iteration in 1:2) {
        result <- verb(step, .grouping = rollup(g), .sort = "last",
                       .id = "set", .keep = keep)
        if (identical(verb, nest_with_margins)) {
          expect_s3_class(result, "dtplyr_step")
          expect_identical(source, before)
          query <- result
          result <- dplyr::collect(query)
          expect_identical(dplyr::collect(query), result)
        } else {
          expect_s3_class(result, "rowwise_df")
          expect_identical(dplyr::group_vars(result), c("g", "set"))
        }
        expect_identical(result$g, c("a", "b", "Total"))
        expect_identical(result$set, c(1L, 1L, 2L))
        expect_identical(lapply(result$data, function(x) x$amount),
                         list(c(1L, 3L), 2L, 1:3))
        expect_true(all(vapply(result$data, function(x) {
          identical(names(x), if (keep) c("g", "amount") else "amount")
        }, logical(1))))
        if (keep) {
          expect_identical(lapply(result$data, function(x) x$g),
                           list(c("a", "a"), "b", c("a", "b", "a")))
        }
        expect_identical(source, before)
        expect_identical(dplyr::collect(step), expected_input)
        expect_identical(source, before)
      }
    }
  }
})

cached_margin_result <- function(result) {
  if (inherits(result, "dtplyr_step")) result <- dplyr::collect(result)
  result <- dplyr::ungroup(result)
  if ("data" %in% names(result)) {
    result$data <- lapply(result$data, tibble::as_tibble)
  }
  tibble::as_tibble(result)
}

cached_margin_consumers <- list(
  function(x, spec) {
    nest_with_margins(
      x, .grouping = spec, .sort = "last", .id = "set", .keep = TRUE
    )
  },
  function(x, spec) {
    nest_by_with_margins(
      x, .grouping = spec, .sort = "last", .id = "set", .keep = TRUE
    )
  },
  function(x, spec) {
    expand_with_margins(x, .grouping = spec, .sort = "last", .id = "set")
  },
  function(x, spec) {
    summarize_with_margins(
      x, n = dplyr::n(), total = sum(amount),
      .grouping = spec, .sort = "last", .id = "set"
    )
  }
)

test_that("cached renames preserve rollup, single-set, and recached controls", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(g = c("a", "b", "a"), value = 1:3)
  before <- data.table::copy(source)
  step <- dtplyr::lazy_dt(source) |>
    dplyr::filter(TRUE) |>
    dplyr::compute() |>
    dplyr::rename(amount = value)
  materialized <- tibble::tibble(g = c("a", "b", "a"), amount = 1:3)
  for (spec in list(grouping_set(g), rollup(g),
                    grouping_sets(rollup(g), grouping_set(amount)))) {
    for (consume in cached_margin_consumers) {
      expected <- cached_margin_result(consume(materialized, spec))
      for (input in list(step, dplyr::compute(step), materialized)) {
        result <- consume(input, spec)
        expect_identical(source, before)
        expect_identical(cached_margin_result(result), expected)
        expect_identical(source, before)
      }
    }
  }
})

test_that("cached expansion and portable summary retain repeated occurrences", {
  skip_if_suggest_absent("dtplyr")
  step <- dtplyr::lazy_dt(data.frame(g = c("a", "b", "a"), value = 1:3)) |>
    dplyr::filter(TRUE) |>
    dplyr::compute() |>
    dplyr::rename(amount = value)
  spec <- grouping_sets(rollup(g), rollup(g))
  expansion <- dplyr::collect(expand_with_margins(
    step, .grouping = spec, .duplicates = "keep", .id = "set", .sort = "last"
  ))
  for (set in 1:4) {
    expect_identical(sort(expansion$amount[expansion$set == set]), 1:3)
    expect_identical(expansion$g[expansion$set == set],
                     if (set %% 2L == 1L) c("a", "a", "b")
                     else rep("Total", 3L))
  }
  summary <- dplyr::collect(summarize_with_margins(
    step, n = dplyr::n(), total = sum(amount),
    .grouping = spec, .duplicates = "keep", .id = "set", .sort = "last"
  ))
  expect_identical(summary$g, c("a", "a", "b", "b", "Total", "Total"))
  expect_identical(summary$set, c(1L, 3L, 1L, 3L, 2L, 4L))
  expect_identical(summary$n, c(2L, 2L, 1L, 1L, 3L, 3L))
  expect_identical(summary$total, c(4L, 4L, 2L, 2L, 6L, 6L))
})

test_that("cached graph isolation stays deferred and protects saved bindings", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(g = c("a", "b", "a"), value = 1:3)
  before <- data.table::copy(source)
  seen <- integer()
  probe <- function(x) {
    seen <<- c(seen, length(x))
    min(x) > 0L
  }
  cached <- dtplyr::lazy_dt(source) |>
    dplyr::filter(probe(value)) |>
    dplyr::compute()
  renamed <- dplyr::rename(cached, amount = value)
  ordered <- dplyr::arrange(renamed, dplyr::desc(amount))
  nested <- dplyr::arrange(dplyr::compute(ordered), g)
  materialized <- tibble::tibble(g = c("a", "b", "a"), amount = 1:3)
  cases <- list(
    list(renamed, materialized),
    list(ordered, dplyr::arrange(materialized, dplyr::desc(amount))),
    list(nested, dplyr::arrange(
      dplyr::arrange(materialized, dplyr::desc(amount)), g
    )),
    list(dplyr::left_join(renamed, renamed, by = c("g", "amount")),
         materialized),
    list(dplyr::union_all(renamed, renamed),
         dplyr::union_all(materialized, materialized))
  )
  for (case in cases) {
    for (consume in cached_margin_consumers) {
      seen <- integer()
      expect_silent(result <- consume(case[[1L]], rollup(g)))
      if (inherits(result, "dtplyr_step")) {
        expect_identical(seen, integer())
      } else {
        expect_s3_class(result, "rowwise_df")
        expect_true(length(seen) > 0L && all(seen == 3L))
      }
      expect_identical(source, before)
      expect_identical(cached_margin_result(result),
                       cached_margin_result(consume(case[[2L]], rollup(g))))
      expect_true(length(seen) > 0L && all(seen == 3L))
      expect_identical(source, before)
    }
  }
})

test_that("cached rename isolation does not admit explicit Mutable steps", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(g = c("a", "b", "a"), value = 1:3)
  before <- data.table::copy(source)
  mutable <- dtplyr::lazy_dt(source, immutable = FALSE) |>
    dplyr::filter(TRUE) |>
    dplyr::compute() |>
    dplyr::rename(amount = value)
  for (consume in cached_margin_consumers) {
    expect_error(consume(mutable, rollup(g)),
                 "immutable = TRUE", class = "marginplyr_error")
    expect_identical(source, before)
  }
  expect_identical(
    inspect_grouping(mutable, .grouping = rollup(where(is.character))),
    inspect_grouping(tibble::as_tibble(before), .grouping = rollup(g))
  )
  expect_identical(source, before)
})
