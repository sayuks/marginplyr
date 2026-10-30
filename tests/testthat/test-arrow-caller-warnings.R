test_that("supported Arrow summaries preserve caller warning conditions", {
  skip_if_suggest_absent("arrow")
  data <- data.frame(k = "E", x = 1)
  inputs <- c(absorbing_arrow_inputs(data), refusing_arrow_inputs(data))
  messages <- c(
    "pulling data into R",
    "Pulling data into R",
    "the caller is not pulling data into R"
  )

  for (input in inputs) {
    for (message in messages) {
      for (custom in c(FALSE, TRUE)) {
        cnd <- if (custom) {
          rlang::warning_cnd(
            "caller_warning", message = message,
            parent = simpleWarning("caller cause"), token = new.env()
          )
        } else {
          simpleWarning(message)
        }
        offset <- function() {
          warning(cnd)
          1
        }
        observed <- list()
        capture <- function(warning) {
          observed[[length(observed) + 1L]] <<- warning
          invokeRestart("muffleWarning")
        }

        control <- withCallingHandlers(
          dplyr::summarize(input, total = sum(x) + offset()),
          warning = capture
        )
        expect_s3_class(control, "arrow_dplyr_query")
        expect_equal(dplyr::collect(control)$total, 2)
        expect_identical(observed, list(cnd))

        observed <- list()
        result <- withCallingHandlers(
          summarize_with_margins(
            input, total = sum(x) + offset(), .grouping = grouping_set()
          ),
          warning = capture
        )
        expect_s3_class(result, "arrow_dplyr_query")
        expect_identical(observed, list(cnd))
        expect_equal(dplyr::collect(result)$total, 2)
      }
    }
  }
})

test_that("Arrow caller warnings keep ordinary repetition and class identity", {
  skip_if_suggest_absent("arrow")
  input <- arrow::Table$create(data.frame(k = "E", x = 1))
  first_cnd <- rlang::warning_cnd(
    "first_caller_warning", message = "pulling data into R",
    parent = simpleWarning("caller cause"), token = new.env()
  )
  second_cnd <- rlang::warning_cnd(
    "second_caller_warning", message = "pulling data into R"
  )
  offset <- function() {
    warning(first_cnd)
    warning(second_cnd)
    1
  }
  observed <- list()
  result <- withCallingHandlers(
    summarize_with_margins(
      input, total = sum(x) + offset(), .grouping = rollup(k)
    ),
    warning = function(cnd) {
      observed[[length(observed) + 1L]] <<- cnd
      invokeRestart("muffleWarning")
    }
  )

  expect_s3_class(result, "arrow_dplyr_query")
  expect_equal(dplyr::collect(result)$total, c(2, 2))
  expect_length(observed, 2L)
  for (i in seq_along(observed)) {
    original <- list(first_cnd, second_cnd)[[i]]
    expect_identical(class(observed[[i]]), class(original))
    expect_identical(observed[[i]]$parent, original$parent)
    expect_identical(observed[[i]]$token, original$token)
    expect_match(conditionMessage(observed[[i]]), "pulling data into R")
    expect_match(
      conditionMessage(observed[[i]]),
      "1 further grouping set raised this warning.", fixed = TRUE
    )
  }
})

test_that("a caller handler can stop on an Arrow caller's rlang warning", {
  skip_if_suggest_absent("arrow")
  input <- arrow::Table$create(data.frame(x = 1))
  emitted <- NULL
  received <- NULL
  offset <- function() {
    withCallingHandlers(
      rlang::warn("Pulling data into R", class = "caller_warning"),
      warning = function(cnd) {
        emitted <<- cnd
      }
    )
    1
  }
  sentinel <- errorCondition("caller handler stopped", class = "caller_error")
  error <- tryCatch(
    withCallingHandlers(
      summarize_with_margins(
        input, total = sum(x) + offset(), .grouping = grouping_set()
      ),
      warning = function(cnd) {
        received <<- cnd
        stop(sentinel)
      }
    ),
    error = identity
  )

  expect_identical(received, emitted)
  expect_s3_class(received, "caller_warning")
  expect_identical(error, sentinel)
})
