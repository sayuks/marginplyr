test_that("external column selections keep their typed-pass binding value", {
  data <- data.frame(a = c("x", "y"), b = c(1, 2))
  reads <- 0L
  selected <- 0L
  counted <- function(x) {
    selected <<- selected + 1L
    x
  }
  env <- rlang::env(rlang::current_env())
  makeActiveBinding("cols", function() {
    reads <<- reads + 1L
    if (reads %% 2L == 1L) "a" else "b"
  }, env)
  measure <- function(expr) {
    reads <<- 0L
    selected <<- 0L
    result <- eval(expr, envir = env)
    list(result = result, reads = reads, selected = selected)
  }
  rlang::local_options(lifecycle_verbosity = "quiet")

  probe <- measure(quote(c(cols, cols)))
  expect_identical(probe$result, c("a", "b"))
  expect_identical(probe$reads, 2L)
  direct <- measure(quote(tidyselect::eval_select(quote(cols), data, env)))
  expect_identical(direct$result, c(a = 1L))
  expect_identical(direct$reads, 1L)
  fixed <- measure(quote(inspect_grouping(data, .by = cols)))
  expect_identical(fixed$result$fixed, "(a)")
  expect_identical(fixed$reads, 1L)

  constructors <- c(
    "grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec"
  )
  selections <- list(
    quote(cols),
    rlang::call2("(", rlang::call2("(", quote(cols))),
    quote(tidyselect::all_of(cols))
  )
  for (constructor in constructors) {
    for (selection in selections) {
      nested <- rlang::call2(constructor, selection)
      call <- rlang::expr(inspect_grouping(
        data,
        .grouping = grouping_sets(
          !!nested,
          tidyselect::all_of(counted("a"))
        ),
        .duplicates = "keep"
      ))
      observed <- measure(call)
      expected <- if (constructor %in% c("rollup", "cube")) {
        c("(a)", "()", "(a)")
      } else {
        c("(a)", "(a)")
      }
      expect_identical(observed$result$included, expected, info = constructor)
      expect_identical(observed$reads, 1L, info = constructor)
      # This selection runs in every compilation pass; one proves that a
      # discarded names-only pass does not account for a second binding read.
      expect_identical(observed$selected, 1L, info = constructor)
    }
  }

  names_only <- measure(quote(inspect_grouping(
    data,
    .grouping = grouping_set(tidyselect::all_of(counted("a")))
  )))
  expect_identical(names_only$result$included, "(a)")
  expect_identical(names_only$reads, 0L)
  expect_identical(names_only$selected, 2L)

  repeated <- measure(quote(inspect_grouping(
    data, .grouping = grouping_sets(cols, cols)
  )))
  expect_identical(repeated$result$included, c("(a)", "(b)"))
  expect_identical(repeated$reads, 2L)

  spec <- eval(quote(grouping_set(cols)), envir = env)
  reads <- 0L
  first <- inspect_grouping(data, .grouping = spec)
  second <- inspect_grouping(data, .grouping = spec)
  expect_identical(first$included, "(a)")
  expect_identical(second$included, "(b)")
  expect_identical(reads, 2L)
})

test_that("retained external selections keep tidyselect diagnostics", {
  data <- data.frame(a = c("x", "y"), b = c(1, 2))
  reads <- 0L
  env <- rlang::env(rlang::current_env())
  makeActiveBinding("cols", function() {
    reads <<- reads + 1L
    if (reads == 1L) "a" else "b"
  }, env)
  rlang::local_options(lifecycle_verbosity = "warning")
  warning <- expect_warning(
    result <- eval(quote(inspect_grouping(
      data, .grouping = grouping_set(cols)
    )), envir = env),
    class = "lifecycle_warning_deprecated"
  )
  expect_match(conditionMessage(warning), "all_of(cols)", fixed = TRUE)
  expect_identical(result$included, "(a)")
  expect_identical(reads, 1L)

  invalid_reads <- 0L
  makeActiveBinding("invalid_cols", function() {
    invalid_reads <<- invalid_reads + 1L
    if (invalid_reads == 1L) 3L else 1L
  }, env)
  rlang::local_options(lifecycle_verbosity = "quiet")
  expect_error(eval(quote(inspect_grouping(
    data, .grouping = grouping_set(invalid_cols)
  )), envir = env), class = "vctrs_error_subscript_oob")
  expect_identical(invalid_reads, 1L)
})
