# jarl-ignore missing_argument: Quoted empty positions are factory input data.
test_that("quoted factories agree inline and stored at every public boundary", {
  data <- data.frame(a = 1L, b = 2L)
  drop_last <- function(expr) {
    expr[[length(expr)]] <- NULL
    eval(expr)
  }
  empty <- drop_last(quote(grouping_set(, a)))
  retained <- drop_last(quote(grouping_set(a, , b)))
  expect_identical(inspect_grouping(data, .grouping = empty)$included, "()")
  expect_identical(inspect_grouping(data, .grouping = retained)$included, "(a)")
  plain_capture <- function(x) rlang::eval_tidy(rlang::enquo(x))
  plain <- plain_capture(drop_last(quote(grouping_set(, a))))
  expect_identical(inspect_grouping(data, .grouping = plain),
                   inspect_grouping(data, .grouping = empty))
  plain <- plain_capture(drop_last(quote(grouping_set(a, , b))))
  expect_identical(inspect_grouping(data, .grouping = plain),
                   inspect_grouping(data, .grouping = retained))
  for (consume in list(inspect_grouping, summarize_with_margins,
                       expand_with_margins, nest_with_margins,
                       nest_by_with_margins)) {
    actual <- tryCatch(
      consume(data, .grouping = drop_last(quote(grouping_set(, a)))),
      error = identity
    )
    expect_identical(actual, consume(data, .grouping = empty))
    actual <- tryCatch(
      consume(data, .grouping = drop_last(quote(grouping_set(a, , b)))),
      error = identity
    )
    expect_identical(actual, consume(data, .grouping = retained))
  }
})

# jarl-ignore missing_argument: Editing the quoted call leaves a trailing comma.
test_that("quoted factories preserve dtplyr metadata and source ownership", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr imports data.table, so dtplyr is the only optional backend here.
  source <- data.table::data.table(a = 1:2, b = c("x", "y"))
  before <- serialize(source, NULL)
  step <- dplyr::select(dtplyr::lazy_dt(source, immutable = FALSE), b, a)
  counter <- local_dtplyr_proxy_counter()
  drop_last <- function(expr) {
    expr[[length(expr)]] <- NULL
    eval(expr)
  }
  stored <- drop_last(quote(grouping_set(where(is.numeric), , b)))
  expected <- inspect_grouping(step, .grouping = stored)
  counter$n <- 0L
  counter$source_evaluations <- 0L
  actual <- inspect_grouping(
    step, .grouping = drop_last(quote(grouping_set(where(is.numeric), , b)))
  )
  expect_identical(actual, expected)
  expect_identical(actual$included, "(a)")
  expect_identical(counter$n, 1L)
  expect_identical(counter$source_evaluations, 0L)
  expect_identical(serialize(source, NULL), before)
})

# jarl-ignore missing_argument: The quoted empty position becomes trailing.
test_that("quoted factories leave an inspected Arrow reader unconsumed", {
  skip_if_suggest_absent("arrow")
  data <- data.frame(a = 1:2, b = c("x", "y"))
  reader <- arrow::RecordBatchReader$create(arrow::record_batch(data))
  drop_last <- function(expr) {
    expr[[length(expr)]] <- NULL
    eval(expr)
  }
  stored <- drop_last(quote(grouping_set(where(is.numeric), , b)))
  expected <- inspect_grouping(arrow::Table$create(data), .grouping = stored)
  actual <- inspect_grouping(
    reader, .grouping = drop_last(quote(grouping_set(where(is.numeric), , b)))
  )
  expect_identical(actual, expected)
  expect_identical(actual$included, "(a)")
  expect_identical(as.data.frame(arrow::as_arrow_table(reader)), data)
})

# jarl-ignore missing_argument: Template equality observes unmodified language.
test_that("quoted and expression templates keep their selected column", {
  data <- data.frame(a = 1L, b = 2L)
  counter <- new.env(parent = emptyenv())
  counter$reads <- 0L
  choose <- function(expr) {
    counter$reads <- counter$reads + 1L
    if (identical(expr, quote(grouping_set(, a)))) {
      grouping_set(a)
    } else {
      grouping_set(b)
    }
  }
  calls <- list(
    function(consume) {
      consume(data, .grouping = choose(quote(grouping_set(, a))))
    },
    function(consume) {
      consume(data, .grouping = choose(base::quote(grouping_set(, a))))
    },
    function(consume) {
      consume(data, .grouping = choose((quote)(grouping_set(, a))))
    },
    function(consume) {
      consume(data, .grouping = choose((base::quote)(grouping_set(, a))))
    },
    function(consume) {
      consume(data, .grouping = choose(expression(grouping_set(, a))[[1L]]))
    },
    function(consume) {
      consume(data, .grouping = choose(
        base::expression(grouping_set(, a))[[1L]]
      ))
    }
  )
  stored <- choose(quote(grouping_set(, a)))
  for (consume in list(inspect_grouping, summarize_with_margins,
                       expand_with_margins, nest_with_margins,
                       nest_by_with_margins)) {
    expected <- consume(data, .grouping = stored)
    for (call in calls) {
      counter$reads <- 0L
      actual <- call(consume)
      expect_identical(actual, expected)
      expect_identical(counter$reads, 1L)
    }
  }
})

# jarl-ignore missing_argument: The factory inspects the function body's syntax.
test_that("a supplied function body remains ordinary factory input", {
  data <- data.frame(a = 1L, b = 2L)
  choose <- function(fn) {
    if (identical(body(fn), quote(grouping_set(, a)))) {
      grouping_set(a)
    } else {
      grouping_set(b)
    }
  }
  stored <- choose(function() grouping_set(, a))
  expect_identical(inspect_grouping(data, .grouping = stored)$included, "(a)")
  for (consume in list(inspect_grouping, summarize_with_margins,
                       expand_with_margins, nest_with_margins,
                       nest_by_with_margins)) {
    actual <- consume(data, .grouping = choose(function() grouping_set(, a)))
    expect_identical(actual, consume(data, .grouping = stored))
  }
})
