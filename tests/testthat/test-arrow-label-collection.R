# Observe both entry and completion: an error after collection must not hide
# the rows Arrow already read. Tracing the generic also sees automatic collect.
observe_arrow_label_collection <- function(expr) {
  counter <- new.env(parent = emptyenv())
  counter$entries <- 0L
  counter$rows <- integer()
  on.exit(suppressMessages(untrace(
    "collect", where = asNamespace("dplyr")
  )), add = TRUE)
  suppressMessages(trace(
    "collect", where = asNamespace("dplyr"),
    tracer = function() {
      counter$entries <- counter$entries + 1L
    },
    exit = function() {
      frame <- parent.frame()
      counter$rows <- c(counter$rows, nrow(evalq(returnValue(), frame)))
    },
    print = FALSE
  ))
  result <- tryCatch(suppressWarnings(force(expr)), error = identity)
  list(result = result, entries = counter$entries, rows = counter$rows)
}

# Both public verbs reach the generated label conversion with the same keys.
arrow_label_result <- function(source, dimensions, summary) {
  grouping <- grouping_set(dplyr::all_of(dimensions))
  if (summary) {
    return(summarize_with_margins(
      source, n = dplyr::n(), .grouping = grouping
    ))
  }
  expand_with_margins(source, .grouping = grouping)
}

test_that("generated Arrow labels refuse automatic collection before entry", {
  skip_if_suggest_absent("arrow")
  inputs <- absorbing_arrow_inputs(data.frame(.data = "x", b = "y"))
  control <- observe_arrow_label_collection(dplyr::collect(inputs$table))
  expect_identical(control$entries, 1L)
  expect_identical(control$rows, 1L)
  expect_equal(control$result, tibble::tibble(.data = "x", b = "y"))

  for (shape in names(inputs)) {
    for (summary in c(FALSE, TRUE)) {
      info <- paste(shape, summary)
      observed <- observe_arrow_label_collection(arrow_label_result(
        inputs[[shape]], c(".data", "b"), summary
      ))
      expect_identical(observed$entries, 0L, info = info)
      expect_identical(observed$rows, integer(), info = info)
      expect_true(inherits(observed$result, "marginplyr_error"), info = info)
      verb <- if (summary) "summarize_with_margins" else "expand_with_margins"
      expect_identical(rlang::call_name(conditionCall(observed$result)), verb,
                       info = info)
      text <- conditionMessage(observed$result)
      expect_match(text, "Arrow cannot evaluate", fixed = TRUE, info = info)
      expect_match(text, "Margin label", fixed = TRUE, info = info)
      expect_match(text, "Collect the Arrow input first", fixed = TRUE,
                   info = info)
      expect_match(text, "Select the columns you need before collecting",
                   fixed = TRUE, info = info)
      expect_false(grepl("conversions", text, fixed = TRUE), info = info)
    }
  }
})

test_that("supported Arrow labels stay lazy and collect only on request", {
  skip_if_suggest_absent("arrow")
  inputs <- absorbing_arrow_inputs(data.frame(.data = "x", b = "y"))
  for (shape in names(inputs)) {
    for (summary in c(FALSE, TRUE)) {
      info <- paste(shape, summary)
      observed <- observe_arrow_label_collection(arrow_label_result(
        inputs[[shape]], c("b", ".data"), summary
      ))
      expect_identical(observed$entries, 0L, info = info)
      expect_identical(observed$rows, integer(), info = info)
      expect_true(inherits(observed$result, "arrow_dplyr_query"), info = info)
      collected <- observe_arrow_label_collection(dplyr::collect(
        observed$result
      ))
      expect_identical(collected$entries, 1L, info = info)
      expect_identical(collected$rows, 1L, info = info)
      expected <- tibble::tibble(b = "y", .data = "x")
      if (summary) expected$n <- 1L
      expect_equal(collected$result, expected, info = info)
    }
  }
})

test_that("Arrow labels preserve included and omitted dimensions", {
  skip_if_suggest_absent("arrow")
  inputs <- absorbing_arrow_inputs(data.frame(.data = "x", b = "y"))
  for (shape in names(inputs)) {
    for (summary in c(FALSE, TRUE)) {
      for (missing in c(FALSE, TRUE)) {
        info <- paste(shape, summary, missing)
        dimensions <- c("b", ".data")
        grouping <- rollup(dplyr::all_of(dimensions))
        label <- if (missing) NULL else "Total"
        observed <- observe_arrow_label_collection(if (summary) {
          summarize_with_margins(
            inputs[[shape]], n = dplyr::n(), .grouping = grouping,
            .margin_label = label, .id = "sid"
          )
        } else {
          expand_with_margins(
            inputs[[shape]], .grouping = grouping,
            .margin_label = label, .id = "sid"
          )
        })
        expect_identical(observed$entries, 0L, info = info)
        expect_identical(observed$rows, integer(), info = info)
        expect_true(inherits(observed$result, "arrow_dplyr_query"), info = info)
        result <- dplyr::collect(observed$result)
        result <- result[order(result$sid), ]
        omitted <- if (missing) NA_character_ else "Total"
        expected <- tibble::tibble(b = c("y", "y", omitted),
                                   .data = c("x", omitted, omitted), sid = 1:3)
        if (summary) expected$n <- rep(1L, 3L)
        expect_equal(result, expected, info = info)
      }
    }
  }
})

test_that("Dataset labels retain Arrow's refusal without collection", {
  skip_if_suggest_absent("arrow")
  inputs <- refusing_arrow_inputs(data.frame(.data = "x", b = "y"))
  for (shape in names(inputs)) {
    for (summary in c(FALSE, TRUE)) {
      info <- paste(shape, summary)
      observed <- observe_arrow_label_collection(arrow_label_result(
        inputs[[shape]], c(".data", "b"), summary
      ))
      expect_identical(observed$entries, 0L, info = info)
      expect_identical(observed$rows, integer(), info = info)
      expect_true(inherits(observed$result, "arrow_not_supported"), info = info)
      expect_false(inherits(observed$result, "marginplyr_error"), info = info)
    }
  }
})

test_that("reader label construction refuses before consuming any batch", {
  skip_if_suggest_absent("arrow")
  for (query in c(FALSE, TRUE)) {
    for (summary in c(FALSE, TRUE)) {
      reader <- arrow::RecordBatchReader$create(
        arrow::record_batch(.data = "x", b = "y")
      )
      source <- if (query) {
        dplyr::select(reader, dplyr::everything())
      } else {
        reader
      }
      observed <- observe_arrow_label_collection(arrow_label_result(
        source, c(".data", "b"), summary
      ))
      expect_identical(observed$entries, 0L)
      expect_identical(observed$rows, integer())
      expect_s3_class(observed$result, "marginplyr_error")
      expect_match(conditionMessage(observed$result), "RecordBatchReader",
                   fixed = TRUE)
      expect_equal(dplyr::collect(arrow::as_arrow_table(reader)),
                   tibble::tibble(.data = "x", b = "y"))
    }
  }
})

test_that("the label guard refuses a local result even without a warning", {
  skip_if_suggest_absent("arrow")
  # Simulate backend drift at the dplyr boundary: a returned frame cannot
  # otherwise exercise the backstop while Arrow still emits its warning.
  original_mutate <- dplyr::mutate
  testthat::local_mocked_bindings(
    mutate = function(.data, ...) {
      dplyr::collect(original_mutate(.data, ...))
    },
    .package = "dplyr"
  )
  raised <- expect_error(expand_with_margins(
    arrow::Table$create(data.frame(k = "x")), .grouping = grouping_set(k)
  ), class = "marginplyr_error")
  expect_match(conditionMessage(raised), "Margin label transformation",
               fixed = TRUE)
})
