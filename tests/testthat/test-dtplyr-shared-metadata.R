test_that("cached shared-source joins preserve independently typed branches", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(g = 1:3, v = 4:6)
  before <- data.table::copy(source)
  root <- dtplyr::lazy_dt(source)
  left <- dplyr::compute(dplyr::mutate(root, new = v + 1L))
  right <- dplyr::compute(dplyr::mutate(root, new = "a"))
  step <- dplyr::inner_join(left, right, by = "g")
  collected <- dplyr::collect(step)
  expect_identical(
    inspect_grouping(step, .by = where(is.character)),
    inspect_grouping(collected, .by = where(is.character))
  )
  expect_identical(source, before)
})

test_that("all cached join types agree with collected typed selections", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(g = 1:3, v = 4:6)
  before <- data.table::copy(source)
  root <- dtplyr::lazy_dt(source)
  values <- list(TRUE, 1L, 1.5, "a")
  for (left_value in values) {
    for (right_value in values) {
      left <- dplyr::compute(dplyr::mutate(root, new = !!left_value))
      right <- dplyr::compute(dplyr::mutate(root, new = !!right_value))
      for (join in list(dplyr::left_join, dplyr::right_join,
                        dplyr::inner_join, dplyr::full_join)) {
        step <- join(left, right, by = "g")
        collected <- dplyr::collect(step)
        for (predicate in list(is.logical, is.integer, is.double,
                               is.character)) {
          expect_silent(plan <- inspect_grouping(
            step, .by = where(predicate)
          ))
          expect_identical(plan, inspect_grouping(
            collected, .by = where(predicate)
          ))
          expect_silent(plan <- inspect_grouping(
            step, .grouping = grouping_set(where(predicate))
          ))
          expect_identical(plan, inspect_grouping(
            collected, .grouping = grouping_set(where(predicate))
          ))
        }
      }
    }
  }
  expect_identical(source, before)
})

test_that("shared cached joins preserve nested keys payloads and expansion", {
  skip_if_suggest_absent("dtplyr")
  normalize <- function(x) {
    x <- dplyr::ungroup(x)
    if ("data" %in% names(x)) {
      x$data <- lapply(x$data, tibble::as_tibble)
    }
    x
  }
  source <- data.table::data.table(g = 1:3, v = 4:6)
  before <- data.table::copy(source)
  root <- dtplyr::lazy_dt(source)
  left_inputs <- list(dplyr::mutate(root, new = v + 1L),
                      dplyr::mutate(root, new = 1L))
  for (left in left_inputs) {
    left <- dplyr::compute(dplyr::filter(left, g < 3L))
    right <- dplyr::compute(dplyr::filter(
      dplyr::mutate(root, new = "a"), g > 1L
    ))
    for (join in list(dplyr::left_join, dplyr::right_join,
                      dplyr::inner_join, dplyr::full_join)) {
      step <- join(left, right, by = "g")
      collected <- dplyr::collect(step)
      saved <- utils::capture.output(dplyr::show_query(step))
      for (iteration in 1:2) {
        expect_identical(
          inspect_grouping(step, .grouping = rollup(where(is.character))),
          inspect_grouping(collected,
                           .grouping = rollup(where(is.character)))
        )
        for (spec in list(NULL, rollup(where(is.character)))) {
          for (keep in c(FALSE, TRUE)) {
            for (verb in list(nest_with_margins, nest_by_with_margins)) {
              expect_silent(actual <- verb(
                step, .by = if (is.null(spec)) where(is.character) else NULL,
                .grouping = spec, .keep = keep, .sort = "last"
              ))
              expected <- verb(
                collected,
                .by = if (is.null(spec)) where(is.character) else NULL,
                .grouping = spec, .keep = keep, .sort = "last"
              )
              expect_identical(normalize(dplyr::collect(actual)),
                               normalize(expected))
            }
          }
        }
        expect_identical(dplyr::collect(expand_with_margins(
          step, .grouping = rollup(where(is.character)),
          .sort = "last", .id = "set"
        )), expand_with_margins(
          collected, .grouping = rollup(where(is.character)),
          .sort = "last", .id = "set"
        ))
        expect_identical(dplyr::collect(step), collected)
        expect_identical(utils::capture.output(dplyr::show_query(step)), saved)
        expect_identical(source, before)
      }
    }
  }
})

test_that("cached rename and arithmetic graphs need no repair warnings", {
  skip_if_suggest_absent("dtplyr")
  old <- options(warn = 2L)
  on.exit(options(old), add = TRUE)
  normalize <- function(x) {
    x <- dplyr::ungroup(x)
    x$data <- lapply(x$data, tibble::as_tibble)
    x
  }
  source <- data.table::data.table(g = c("a", "b"), v = 1:2)
  before <- data.table::copy(source)
  root <- dtplyr::lazy_dt(source)
  for (derived in list(dplyr::rename(root, value = v),
                       dplyr::mutate(root, new = v + 1L))) {
    cached <- dplyr::compute(derived)
    for (combine in list(dplyr::union_all, dplyr::union, dplyr::intersect,
                         dplyr::setdiff, function(x, y) {
                           dplyr::semi_join(x, y, by = "g")
                         })) {
      step <- combine(cached, cached)
      expected_input <- dplyr::collect(step)
      for (iteration in 1:2) {
        expect_silent(plan <- inspect_grouping(step, .by = where(is.character)))
        expect_identical(plan, inspect_grouping(
          expected_input, .by = where(is.character)
        ))
        for (verb in list(nest_with_margins, nest_by_with_margins)) {
          expect_silent(actual <- verb(
            step, .grouping = rollup(g), .sort = "last", .keep = TRUE
          ))
          expected <- verb(expected_input, .grouping = rollup(g),
                           .sort = "last", .keep = TRUE)
          expect_identical(normalize(dplyr::collect(actual)),
                           normalize(expected))
        }
        expect_identical(dplyr::collect(step), expected_input)
        expect_identical(source, before)
      }
    }
  }
})

test_that("shared graph controls preserve metadata without processing rows", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(g = 1:3, v = 4:6)
  before <- data.table::copy(source)
  seen <- integer()
  probe <- function(x) {
    seen <<- c(seen, length(x))
    x > 0L
  }
  root <- dtplyr::lazy_dt(source)
  left <- dplyr::mutate(root, new = v + 1L)
  right <- dplyr::mutate(root, new = "a")
  cached <- dplyr::compute(dplyr::filter(right, probe(v)))
  independent <- dtplyr::lazy_dt(data.table::copy(source)) |>
    dplyr::mutate(new = "a") |>
    dplyr::compute()
  inputs <- list(
    dplyr::left_join(left, right, by = "g"), cached,
    dplyr::left_join(dplyr::compute(left), independent, by = "g"),
    dplyr::left_join(dplyr::compute(left), cached, by = "g")
  )
  controls <- lapply(inputs, dplyr::collect)
  seen <- integer()
  root_sizes <- integer()
  # Observe the backend evaluator: public results cannot reveal a hidden read.
  dt_eval <- getFromNamespace("dt_eval", "dtplyr")
  dt_sources <- getFromNamespace("dt_sources", "dtplyr")
  testthat::local_mocked_bindings(
    dt_eval = function(x) {
      root_sizes <<- c(root_sizes, vapply(dt_sources(x), nrow, integer(1)))
      dt_eval(x)
    },
    .package = "dtplyr"
  )
  for (i in seq_along(inputs)) {
    step <- inputs[[i]]
    collected <- controls[[i]]
    for (iteration in 1:2) {
      expect_identical(
        inspect_grouping(step, .by = where(is.character)),
        inspect_grouping(collected, .by = where(is.character))
      )
      expect_silent(query <- nest_with_margins(
        step, .grouping = rollup(where(is.character)), .sort = "last"
      ))
      expect_s3_class(query, "dtplyr_step")
      expect_s3_class(expand_with_margins(
        step, .grouping = rollup(where(is.character))
      ), "dtplyr_step")
      expect_identical(seen, integer())
      expect_true(length(root_sizes) > 0L && all(root_sizes == 0L))
      expect_identical(source, before)
    }
  }
  root_sizes <- integer()
  expect_s3_class(nest_by_with_margins(
    inputs[[4L]], .by = where(is.character)
  ), "rowwise_df")
  expect_true(any(root_sizes > 0L))
  expect_true(length(seen) > 0L && all(seen == 3L))
  expect_identical(source, before)
})

test_that("schema isolation preserves External warnings", {
  skip_if_suggest_absent("dtplyr")
  root <- dtplyr::lazy_dt(data.frame(g = 1:2, v = 3:4))
  cached <- dplyr::compute(dplyr::mutate(root, new = v + 1L))
  step <- dplyr::union_all(cached, cached)
  external <- structure(
    list(message = "external metadata warning", call = NULL),
    class = c("metadata_warning", "warning", "condition")
  )
  dt_eval <- getFromNamespace("dt_eval", "dtplyr")
  # Inject an External condition where the backend evaluates the schema graph.
  testthat::local_mocked_bindings(
    dt_eval = function(x) {
      warning(external)
      dt_eval(x)
    },
    .package = "dtplyr"
  )
  warnings <- list()
  withCallingHandlers({
    inspect_grouping(step, .by = where(is.integer))
    nest_with_margins(step)
    expand_with_margins(step)
    nest_by_with_margins(step)
  }, warning = function(w) {
    warnings[[length(warnings) + 1L]] <<- w
    invokeRestart("muffleWarning")
  })
  expect_true(length(warnings) > 0L)
  expect_true(all(vapply(warnings, identical, logical(1), y = external)))
})
