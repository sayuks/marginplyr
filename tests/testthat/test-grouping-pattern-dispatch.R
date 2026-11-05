test_that("pattern consumption methods receive typed local selection data", {
  skip_if_suggest_absent("stringr")

  data <- data.frame(a = "x", b = 1L)
  pattern <- stringr::fixed("a")
  reads <- logical()
  method <- function(x) {
    typed <- is.character(tidyselect::peek_data()$a)
    reads <<- c(reads, typed)
    if (typed) 1L else 2L
  }
  rlang::local_bindings(
    length.stringr_fixed = method,
    .env = asNamespace("base")$.__S3MethodsTable__.
  )
  selection <- rlang::quo(matches(pattern))
  expect_identical(names(tidyselect::eval_select(selection, data)), "a")
  reads <- logical()
  expect_identical(
    inspect_grouping(data, .by = !!selection, .format = "list")$fixed,
    list("a")
  )
  expect_true(length(reads) > 0L)
  expect_true(all(reads))
})

# Direct tidyselect and an opaque wrapper provide typed public controls.
check_pattern_consumers <- function(data, local) {
  run <- function(modifier, level) {
    counter <- rlang::env(reads = logical())
    method <- function(x) {
      typed <- is.character(tidyselect::peek_data()$a)
      counter$reads <- c(counter$reads, typed)
      if (typed) 1L else 2L
    }
    method_class <- if (level == "exact") {
      paste0("stringr_", modifier)
    } else {
      "stringr_pattern"
    }
    rlang::local_bindings(
      !!!setNames(list(method), paste0("length.", method_class)),
      .env = asNamespace("base")$.__S3MethodsTable__.
    )
    pattern <- getExportedValue("stringr", modifier)("a")
    selections <- list(
      rlang::quo(tidyselect::matches(pattern)),
      rlang::new_quosure(
        rlang::call2("matches", rlang::call2(modifier, "a", .ns = "stringr")),
        environment()
      )
    )
    for (selection in selections) {
      counter$reads <- logical()
      expect_identical(names(tidyselect::eval_select(selection, local)), "a")
      budget <- length(counter$reads)
      counter$reads <- logical()
      expected <- inspect_grouping(
        data, .by = tidyselect::matches(identity(pattern)), .format = "list"
      )
      counter$reads <- logical()
      expect_identical(
        inspect_grouping(data, .by = !!selection, .format = "list"), expected
      )
      expect_length(counter$reads, budget)
      expect_true(all(counter$reads))
      for (constructor in c("grouping_set", "grouping_sets", "rollup", "cube",
                            "grouping_spec")) {
        spec <- rlang::eval_tidy(rlang::call2(constructor, selection))
        original <- spec
        counter$reads <- logical()
        plan <- inspect_grouping(data, .grouping = spec, .format = "list")
        expect_identical(plan$included[[1L]], "a")
        expect_identical(spec, original)
        expect_length(counter$reads, budget)
        expect_true(all(counter$reads))
      }
      for (verb in c("summarize_with_margins", "expand_with_margins",
                     "nest_with_margins", "nest_by_with_margins")) {
        args <- list(data, .by = selection)
        if (verb == "summarize_with_margins") {
          args$n <- rlang::quo(dplyr::n())
        }
        counter$reads <- logical()
        actual <- dplyr::ungroup(dplyr::collect(
          rlang::eval_tidy(rlang::call2(verb, !!!args))
        ))
        expect_identical(actual$a, "x")
        expect_length(counter$reads, budget)
        expect_true(all(counter$reads))
      }
      expect_true(length(counter$reads) > 0L)
      expect_true(all(counter$reads))
    }
  }
  for (modifier in c("fixed", "regex", "coll")) {
    for (level in c("exact", "inherited")) {
      run(modifier, level)
    }
  }
}

test_that("all local pattern consumers preserve typed pattern dispatch", {
  skip_if_suggest_absent("stringr")
  data <- data.frame(a = "x", b = 1L)
  check_pattern_consumers(data, data)
})

test_that("safe dtplyr pattern consumers preserve typed pattern dispatch", {
  skip_if_suggest_absent("dtplyr", "stringr")
  data <- data.frame(a = "x", b = 1L)
  step <- dtplyr::lazy_dt(data)
  check_pattern_consumers(step, data)
  expect_identical(data, data.frame(a = "x", b = 1L))
})

test_that("pattern methods defer before unsafe dtplyr metadata without reads", {
  skip_if_suggest_absent("dtplyr", "stringr")
  calls <- 0L
  method_calls <- 0L
  method <- function(x) {
    method_calls <<- method_calls + 1L
    1L
  }
  rlang::local_bindings(
    length.stringr_pattern = method,
    .env = asNamespace("base")$.__S3MethodsTable__.
  )
  source <- data.frame(a = "x", b = 1L)
  transform <- function(x) {
    calls <<- calls + 1L
    paste0(x, "!")
  }
  step <- dplyr::mutate(dtplyr::lazy_dt(source), added = transform(a))
  pattern <- stringr::regex("a")
  selections <- rlang::quos(matches(pattern), matches(stringr::regex("a")))
  for (selection in selections) {
    expect_error(
      inspect_grouping(step, .by = !!selection),
      "Can't safely determine column metadata", class = "marginplyr_error"
    )
    expect_error(
      inspect_grouping(step, .grouping = grouping_set(!!selection)),
      "Can't safely determine column metadata", class = "marginplyr_error"
    )
  }
  expect_identical(method_calls, 0L)
  expect_identical(calls, 0L)
  expect_identical(source, data.frame(a = "x", b = 1L))
})

test_that("pattern classification leaves deferred method bindings unforced", {
  skip_if_suggest_absent("dtplyr", "stringr")
  calls <- 0L
  env <- rlang::env(pattern = stringr::fixed("a"))
  makeActiveBinding("length.stringr_fixed", function() {
    calls <<- calls + 1L
    stop("method binding forced")
  }, env)
  delayedAssign(
    "length.stringr_pattern", stop("promise forced"), assign.env = env
  )
  source <- data.frame(a = "x")
  step <- dplyr::mutate(dtplyr::lazy_dt(source), b = paste0(a, "!"))
  selection <- rlang::new_quosure(quote(matches(pattern)), env)
  expect_error(
    inspect_grouping(step, .by = !!selection),
    "Can't safely determine column metadata", class = "marginplyr_error"
  )
  expect_identical(calls, 0L)
  expect_true(rlang::env_binding_are_lazy(env, "length.stringr_pattern")[[1L]])
})

test_that("global pattern methods remain visible to isolated caller scopes", {
  skip_if_suggest_absent("stringr")
  reads <- logical()
  method <- function(x) {
    typed <- is.character(tidyselect::peek_data()$a)
    reads <<- c(reads, typed)
    if (typed) 1L else 2L
  }
  rlang::local_bindings(length.stringr_regex = method, .env = globalenv())
  env <- rlang::env(baseenv(), pattern = stringr::regex("a"))
  selection <- rlang::new_quosure(quote(tidyselect::matches(pattern)), env)
  data <- data.frame(a = "x", b = 1L)
  expect_identical(names(tidyselect::eval_select(selection, data)), "a")
  reads <- logical()
  expect_identical(inspect_grouping(data, .by = !!selection)$fixed, "(a)")
  expect_true(length(reads) > 0L)
  expect_true(all(reads))
})

test_that("unrelated pattern methods preserve ordinary names-only matching", {
  skip_if_suggest_absent("dtplyr", "stringr")
  calls <- 0L
  method <- function(x) {
    calls <<- calls + 1L
    stop("unrelated method ran")
  }
  rlang::local_bindings(length.unrelated_pattern = method, .env = globalenv())
  source <- data.frame(a = "x", b = 1L)
  step <- dplyr::mutate(dtplyr::lazy_dt(source), added = paste0(a, "!"))
  for (modifier in c("regex", "fixed", "coll")) {
    pattern <- getExportedValue("stringr", modifier)("A", ignore_case = TRUE)
    expect_identical(
      inspect_grouping(step, .by = matches(pattern), .format = "list")$fixed,
      list(c("a", "added"))
    )
    expect_identical(
      inspect_grouping(step, .by = !matches(pattern), .format = "list")$fixed,
      list("b")
    )
  }
  expect_identical(calls, 0L)
})
