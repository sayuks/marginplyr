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

# These public consumers share the selection contract, including the source
# partitions each nesting verb returns.
grouping_selection_consumers <- function(data) {
  list(
    function(s) inspect_grouping(data, .grouping = s),
    function(s) summarize_with_margins(data, n = dplyr::n(), .grouping = s),
    function(s) expand_with_margins(data, .grouping = s),
    function(s) nest_with_margins(data, .grouping = s),
    function(s) nest_by_with_margins(data, .grouping = s)
  )
}

# Redundant call and head parentheses are transparent to the spelling gate.
constructor_call_spellings <- function(constructor) {
  call <- rlang::call2(constructor)
  wrapped_head <- rlang::call2(rlang::call2("(", rlang::sym(constructor)))
  list(call, rlang::call2("(", call), wrapped_head,
       rlang::call2("(", rlang::call2("(", wrapped_head)))
}

test_that("constructor-shaped selections keep their first answer", {
  data <- data.frame(a = c("x", "y"), b = c(1L, 1L))
  reads <- 0L
  choose <- function() {
    reads <<- reads + 1L
    if (reads == 1L) "a" else "b"
  }
  env <- rlang::env(rlang::current_env(), choose = choose)
  control <- eval(quote(marginplyr::grouping_sets(choose())), env)
  expected <- grouping_sets(a)
  constructors <- c("grouping_set", "grouping_sets", "rollup", "cube",
                    "grouping_spec")
  for (constructor in constructors) {
    assign(constructor, choose, envir = env)
    for (expr in constructor_call_spellings(constructor)) {
      spec <- eval(rlang::expr(marginplyr::grouping_sets(!!expr)), env)
      original <- rlang::duplicate(spec)
      for (consume in grouping_selection_consumers(data)) {
        reads <- 0L
        baseline <- consume(control)
        expect_identical(reads, 1L)
        expect_identical(baseline, consume(expected))
        reads <- 0L
        expect_identical(consume(spec), baseline, info = constructor)
        expect_identical(reads, 1L, info = constructor)
        expect_identical(spec, original)
        expect_identical(get(constructor, env), choose)
      }
    }
  }
})

test_that("constructor-shaped selections signal caller conditions once", {
  data <- data.frame(a = c("x", "y"), b = c(1L, 1L))
  reads <- 0L
  warning <- rlang::warning_cnd("selection_warning", message = "first answer")
  failure <- rlang::error_cnd("selection_error", message = "first failure")
  env <- rlang::env(rlang::current_env())
  warn_once <- function() {
    reads <<- reads + 1L
    if (reads > 1L) stop("second invocation")
    warning(warning)
    "a"
  }
  fail_once <- function() {
    reads <<- reads + 1L
    stop(failure)
  }
  constructors <- c("grouping_set", "grouping_sets", "rollup", "cube",
                    "grouping_spec")
  for (constructor in constructors) {
    for (expr in constructor_call_spellings(constructor)) {
      spec <- eval(rlang::expr(marginplyr::grouping_sets(!!expr)), env)
      for (consume in grouping_selection_consumers(data)) {
        assign(constructor, warn_once, envir = env)
        reads <- 0L
        seen <- list()
        result <- withCallingHandlers(consume(spec), warning = function(cnd) {
          seen[[length(seen) + 1L]] <<- cnd
          invokeRestart("muffleWarning")
        })
        expect_identical(result, consume(grouping_sets(a)))
        expect_identical(reads, 1L)
        expect_identical(seen, list(warning))

        assign(constructor, fail_once, envir = env)
        reads <- 0L
        error <- expect_error(consume(spec), class = "selection_error")
        expect_identical(conditionMessage(error), conditionMessage(failure))
        expect_identical(error$parent, failure$parent)
        expect_identical(reads, 1L)
      }
    }
  }
})

test_that("constructor selections keep occurrences and calls independent", {
  data <- data.frame(a = c("x", "y"), b = c(1L, 1L))
  reads <- 0L
  arguments <- 0L
  env <- rlang::env(rlang::current_env(), rollup = function(x) {
    reads <<- reads + 1L
    force(x)
    if (reads %% 2L == 1L) "a" else "b"
  }, argument = function() {
    arguments <<- arguments + 1L
  })
  single <- eval(quote(marginplyr::grouping_sets(rollup(argument()))), env)
  repeated <- eval(quote(marginplyr::grouping_sets(
    rollup(argument()), rollup(argument())
  )), env)
  for (consume in grouping_selection_consumers(data)) {
    reads <- 0L
    arguments <- 0L
    expect_identical(consume(single), consume(grouping_sets(a)))
    expect_identical(reads, 1L)
    expect_identical(consume(single), consume(grouping_sets(b)))
    expect_identical(reads, 2L)
    expect_identical(arguments, 2L)

    reads <- 0L
    arguments <- 0L
    expect_identical(consume(repeated), consume(grouping_sets(a, b)))
    expect_identical(reads, 2L)
    expect_identical(arguments, 2L)
  }
})

test_that("retained constructor selections keep selection diagnostics", {
  data <- data.frame(a = c("x", "y"), b = c(1L, 1L))
  reads <- 0L
  env <- rlang::env(rlang::current_env(), rollup = function() {
    reads <<- reads + 1L
    if (reads == 1L) 3L else 1L
  })
  for (expr in constructor_call_spellings("rollup")) {
    spec <- eval(rlang::expr(marginplyr::grouping_sets(!!expr)), env)
    reads <- 0L
    error <- expect_error(inspect_grouping(data, .grouping = spec),
                          class = "vctrs_error_subscript_oob")
    expect_false(inherits(error, "marginplyr_error"))
    expect_match(conditionMessage(error), "Location 3", fixed = TRUE)
    expect_identical(reads, 1L)

    env$rollup <- function() list("a")
    invalid <- expect_error(inspect_grouping(data, .grouping = spec),
                            class = "vctrs_error_subscript_type")
    direct <- expect_error(tidyselect::eval_select(
      rlang::new_quosure(expr, env), data
    ), class = "vctrs_error_subscript_type")
    expect_identical(invalid$subscript_arg, direct$subscript_arg)
    expect_identical(conditionMessage(invalid), conditionMessage(direct))
    env$rollup <- function() {
      reads <<- reads + 1L
      if (reads == 1L) 3L else 1L
    }
  }

  # Call results are values: a language object is not evaluated as new syntax,
  # and a returned function remains a selection predicate.
  for (value in list(quote(a), is.character, NULL, c(renamed = "a"))) {
    env$rollup <- function() value
    env$choose <- env$rollup
    spec <- eval(quote(marginplyr::grouping_sets(rollup())), env)
    control <- eval(quote(marginplyr::grouping_sets(choose())), env)
    observe <- function(s) {
      tryCatch(inspect_grouping(data, .grouping = s), error = identity)
    }
    result <- observe(spec)
    expected <- observe(control)
    if (inherits(expected, "error")) {
      expect_identical(class(result), class(expected))
    } else {
      expect_identical(result, expected)
    }
  }
})
