# Exercise the caller's binding through each verb with a fresh read counter.
run_external_selector <- function(data, expression, named, preceding,
                                  read_once, margin) {
  reads <- 0L
  makeActiveBinding("cols", function() {
    reads <<- reads + 1L
    if (read_once && reads > 1L) stop("selector read twice")
    if (reads == 1L) "x" else "y"
  }, environment())
  dots <- list(rlang::new_quosure(expression, rlang::env(environment())))
  if (named) names(dots) <- "value"
  if (preceding) dots <- c(rlang::quos(ordinary = sum(y)), dots)
  warnings <- character()
  error <- NULL
  result <- tryCatch(withCallingHandlers(if (margin) {
    rlang::inject(summarize_with_margins(
      data, !!!dots, .grouping = grouping_set(g)
    ))
  } else {
    rlang::inject(dplyr::summarise(data, !!!dots, .by = g))
  }, warning = function(cnd) {
    warnings <<- c(warnings, conditionMessage(cnd))
    invokeRestart("muffleWarning")
  }), error = function(cnd) {
    error <<- cnd
    NULL
  })
  list(result = result, reads = reads, error = error, warnings = warnings)
}

# The selectors and independent-summary control differ only at the public call.
expect_external_selectors <- function(data) {
  withr::local_options(lifecycle_verbosity = "warning")
  expressions <- rlang::exprs(
    dplyr::across(cols, sum),
    dplyr::across(dplyr::all_of(cols), sum),
    dplyr::across((cols), sum),
    dplyr::across(.env$cols, sum),
    sum(dplyr::pick(cols))
  )
  for (expression in expressions) {
    for (named in c(FALSE, TRUE)) {
      for (preceding in c(FALSE, TRUE)) {
        for (read_once in c(FALSE, TRUE)) {
          info <- paste(rlang::as_label(expression), named, preceding, read_once)
          expected <- run_external_selector(
            data, expression, named, preceding, read_once, margin = FALSE
          )
          actual <- run_external_selector(
            data, expression, named, preceding, read_once, margin = TRUE
          )
          expect_identical(actual$reads, expected$reads, info = info)
          expect_equal(actual$result, expected$result, info = info)
          if (is.null(expected$error)) {
            expect_null(actual$error, info = info)
          } else {
            expect_match(conditionMessage(expected$error), "selector read twice")
            expect_s3_class(actual$error, class(expected$error)[[1L]])
            expect_match(conditionMessage(actual$error), "selector read twice")
          }
          deprecation <- "Using an external vector in selections was deprecated"
          expect_identical(
            grepl(deprecation, actual$warnings, fixed = TRUE),
            grepl(deprecation, expected$warnings, fixed = TRUE), info = info
          )
        }
      }
    }
  }
}

test_that("first local selections preserve external binding evaluation", {
  withr::local_options(lifecycle_verbosity = "warning")
  data <- data.frame(g = c("a", "a", "b"), x = 1:3, y = 5:7)
  first <- run_external_selector(
    data, quote(dplyr::across(cols, sum)), named = FALSE, preceding = FALSE,
    read_once = TRUE, margin = TRUE
  )
  expect_identical(first$reads, 1L)
  expect_identical(first$result$x, c(3L, 3L))
  expect_match(first$warnings, "Using an external vector", fixed = TRUE)
  for (input in list(data, tibble::as_tibble(data))) {
    expect_external_selectors(input)
  }
})

test_that("data.table selections preserve external binding evaluation", {
  skip_if_suggest_absent("data.table")
  data <- data.table::data.table(g = c("a", "a", "b"), x = 1:3, y = 5:7)
  expect_external_selectors(data)
})

test_that("external selectors are not forced before Margin-label validation", {
  data <- data.frame(first = "a", second = "b", x = 1L)
  reads <- 0L
  makeActiveBinding("cols", function() {
    reads <<- reads + 1L
    stop("selector evaluated")
  }, environment())
  expect_error(summarize_with_margins(
    data, dplyr::across(cols, sum), .grouping = rollup(first, second),
    .margin_label = c(first = "All first")
  ), "must name every Margin dimension", class = "marginplyr_error")
  expect_identical(reads, 0L)
})

test_that("static selection errors retain precedence over Margin-label errors", {
  data <- data.frame(first = "a", second = "b", x = 1L)
  empty_pair <- rlang::call2("(", rlang::missing_arg())
  for (empty in c(FALSE, TRUE)) {
    selection <- if (empty) empty_pair else quote(unknown)
    expect_error(summarize_with_margins(
      data, dplyr::across(!!selection, sum),
      .grouping = rollup(first, second), .margin_label = c(first = "All first")
    ), class = if (empty) "simpleError" else "vctrs_error_subscript_oob")
  }
})
