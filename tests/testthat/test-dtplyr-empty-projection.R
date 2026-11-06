test_that("literal-zero dtplyr projections expand as an empty schema", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr imports data.table; only dtplyr is an optional backend here.
  source <- data.table::data.table(g = c("a", "b"), value = 1:2)
  before <- serialize(source, NULL)
  step <- dplyr::select(dtplyr::lazy_dt(source))
  query <- expand_with_margins(step, .id = "set")
  expect_s3_class(query, "dtplyr_step")
  expect_identical(dplyr::collect(query), tibble::tibble(set = integer()))
  expect_identical(serialize(source, NULL), before)
})

test_that("equivalent empty dtplyr projections preserve expansion and plans", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr imports data.table; only dtplyr is an optional backend here.
  selectors <- list(
    function(x) dplyr::select(x),
    function(x) dplyr::select(x, -dplyr::everything()),
    function(x) dplyr::select(x, dplyr::any_of("absent"))
  )
  for (rows in c(0L, 2L)) {
    source <- data.table::data.table(g = rep("a", rows), value = seq_len(rows))
    before <- serialize(source, NULL)
    source_calls <- 0L
    probe <- function(x) {
      source_calls <<- source_calls + 1L
      x > 0L
    }
    root <- dtplyr::lazy_dt(source)
    filtered <- dplyr::filter(root, probe(value))
    steps <- list(
      direct = root, filtered = filtered,
      arranged = dplyr::arrange(filtered, value),
      cached = dplyr::compute(filtered, name = "empty_projection_cache")
    )
    expect_identical(source_calls, 0L)
    for (shape in names(steps)) {
      for (selector in selectors) {
        step <- selector(steps[[shape]])
        if (shape %in% c("direct", "filtered")) {
          expect_match(paste(deparse(dplyr::show_query(step)), collapse = " "),
                       ", 0L]", fixed = TRUE)
        } else {
          expect_match(paste(deparse(dplyr::show_query(step)), collapse = " "),
                       ":=", fixed = TRUE)
        }
        eager <- dplyr::collect(step)
        expect_identical(eager, tibble::tibble())
        source_calls <- 0L
        column_calls <- 0L
        predicate <- function(x) {
          column_calls <<- column_calls + 1L
          is.character(x)
        }
        for (repeated in c(FALSE, TRUE)) {
          spec <- grouping_set()
          typed <- grouping_set(where(predicate))
          if (repeated) {
            spec <- grouping_sets(grouping_set(), grouping_set())
            typed <- grouping_sets(grouping_set(where(predicate)),
                                   grouping_set(where(predicate)))
          }
          plan <- inspect_grouping(
            eager, .grouping = spec, .duplicates = "keep"
          )
          expect_identical(plan$included, rep("()", if (repeated) 2L else 1L))
          expect_identical(inspect_grouping(
            step, .grouping = typed, .duplicates = "keep"
          ), plan)
          expect_identical(inspect_grouping(
            step, .by = where(predicate), .grouping = spec,
            .duplicates = "keep"
          ), plan)
          expect_identical(inspect_grouping(
            step, .grouping = spec, .duplicates = "keep"
          ), plan)
          expect_identical(column_calls, 0L)
          expect_identical(source_calls, 0L)
          for (id in list(NULL, "set")) {
            expected <- if (is.null(id)) {
              tibble::tibble()
            } else {
              tibble::tibble(set = integer())
            }
            query <- expand_with_margins(
              step, .grouping = spec, .duplicates = "keep", .id = id
            )
            expect_s3_class(query, "dtplyr_step")
            expect_identical(source_calls, 0L)
            expect_identical(serialize(source, NULL), before)
            expect_identical(dplyr::collect(query), expected)
            expect_identical(expand_with_margins(
              eager, .grouping = spec, .duplicates = "keep", .id = id
            ), expected)
            expect_identical(dplyr::collect(expand_with_margins(
              dtplyr::lazy_dt(data.frame()), .grouping = spec,
              .duplicates = "keep", .id = id
            )), expected)
            expect_identical(serialize(source, NULL), before)
            source_calls <- 0L
          }
        }
      }
    }
  }
})

test_that("empty projections do not admit unknown or value-dependent parents", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(g = c("a", "b"), value = 1:2)
  before <- serialize(source, NULL)
  calls <- 0L
  probe <- function(x) {
    calls <<- calls + 1L
    x
  }
  root <- dtplyr::lazy_dt(source)
  unsafe <- list(
    dplyr::distinct(root),
    dplyr::mutate(root, value = probe(value)),
    dplyr::mutate(root, value = factor(value)),
    dplyr::mutate(root, value = if (dplyr::n() > 0L) "text" else 0L)
  )
  for (parent in unsafe) {
    for (step in list(dplyr::select(parent),
                      dplyr::select(dplyr::arrange(parent, value)))) {
      expect_error(expand_with_margins(step, .id = "set"),
                   "Can't safely determine column metadata", fixed = TRUE,
                   class = "marginplyr_error")
      expect_error(inspect_grouping(
        step, .grouping = grouping_set(where(is.character))
      ), "Can't safely determine column metadata", fixed = TRUE,
      class = "marginplyr_error")
      expect_identical(inspect_grouping(step)$included, "()")
      expect_identical(calls, 0L)
      expect_identical(serialize(source, NULL), before)
    }
  }
})

test_that("empty projections preserve Mutable-step inspection and refusal", {
  skip_if_suggest_absent("dtplyr")
  for (rows in c(0L, 2L)) {
    source <- data.table::data.table(g = rep("a", rows), value = seq_len(rows))
    before <- serialize(source, NULL)
    calls <- 0L
    probe <- function(x) {
      calls <<- calls + 1L
      x > 0L
    }
    predicate <- function(x) stop("empty column predicate evaluated")
    root <- dtplyr::lazy_dt(source, immutable = FALSE)
    filtered <- dplyr::filter(root, probe(value))
    for (parent in list(root, filtered, dplyr::arrange(filtered, value))) {
      step <- dplyr::select(parent)
      expect_identical(inspect_grouping(
        step, .grouping = grouping_set(where(predicate))
      ), inspect_grouping(tibble::tibble()))
      expect_identical(inspect_grouping(step, .by = where(predicate)),
                       inspect_grouping(tibble::tibble()))
      expect_error(expand_with_margins(step, .id = "set"),
                   "immutable = FALSE", fixed = TRUE,
                   class = "marginplyr_error")
      expect_identical(calls, 0L)
      expect_identical(serialize(source, NULL), before)
    }
  }
})

test_that("empty projections do not bypass caller bracket safety", {
  skip_if_suggest_absent("dtplyr")
  for (binding in c("[", "[.data.table")) {
    source <- data.table::data.table(g = c("a", "b"), value = 1:2)
    before <- serialize(source, NULL)
    calls <- 0L
    caller <- new.env(parent = environment())
    caller[[binding]] <- function(...) {
      calls <<- calls + 1L
      stop("caller bracket evaluated")
    }
    root <- eval(quote(dtplyr::lazy_dt(source)), caller)
    for (parent in list(root, dplyr::filter(root, value > 0L),
                        dplyr::arrange(root, value))) {
      step <- dplyr::select(parent)
      expect_identical(inspect_grouping(step)$included, "()")
      expect_error(inspect_grouping(step, .by = where(is.character)),
                   "Can't safely determine column metadata", fixed = TRUE,
                   class = "marginplyr_error")
      expect_error(expand_with_margins(step), "dplyr::collect()", fixed = TRUE,
                   class = "marginplyr_error")
      expect_identical(calls, 0L)
      expect_identical(serialize(source, NULL), before)
    }
  }
})
