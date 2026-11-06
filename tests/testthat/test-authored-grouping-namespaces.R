test_that("authored wrappers retain an injected specification's first answer", {
  data <- data.frame(a = c("east", "west"), b = c(1L, 1L))
  observed <- rlang::env(calls = 0L)
  caller <- rlang::env(`::` = function(...) {
    observed$calls <- observed$calls + 1L
    if (observed$calls > 1L) stop("second namespace evaluation")
    function(x) "b"
  })
  expression <- rlang::call2(quote(base::`(`), grouping_set(a))
  selection <- rlang::new_quosure(expression, caller)
  expect_identical(
    inspect_grouping(
      data, .grouping = grouping_sets(!!selection), .format = "list"
    )$included,
    list("b")
  )
  expect_identical(observed$calls, 1L)
})

# Each input runs separately so no test requires two optional backends.
check_authored_namespaces <- function(input, data) {
  constructors <- c(
    "grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec"
  )
  for (constructor in constructors) {
    for (operator in c("::", ":::")) {
      observed <- rlang::env(calls = 0L)
      caller <- rlang::env(s = grouping_set("a"))
      caller[[operator]] <- function(...) {
        observed$calls <- observed$calls + 1L
        if (observed$calls > 1L) stop("second namespace evaluation")
        function(x) "b"
      }
      head <- rlang::call2(operator, quote(base), quote(`(`))
      wrap <- function(x) rlang::call2(head, x)
      bound <- wrap(quote(s))
      selection <- rlang::new_quosure(bound, caller)
      selected <- names(tidyselect::eval_select(selection, data))
      expect_identical(selected, "b")
      expect_identical(observed$calls, 1L)
      embrace <- eval(rlang::call2(
        "function", pairlist(x = rlang::missing_arg()),
        rlang::call2(constructor, wrap(quote({{ x }})))
      ), caller)
      numbered <- eval(rlang::call2(
        "function", pairlist(... = rlang::missing_arg()),
        rlang::call2(constructor, wrap(quote(..1)))
      ), caller)
      specifications <- list(
        eval(rlang::call2(constructor, bound), caller),
        eval(rlang::call2(constructor, wrap(quote(grouping_set(a)))), caller),
        eval(rlang::call2(constructor, selection), caller),
        eval(rlang::call2(embrace, quote(s)), caller),
        eval(rlang::call2(numbered, quote(s)), caller)
      )
      expected <- eval(rlang::call2(constructor, "b"))
      consumers <- list(
        function(s) inspect_grouping(input, .grouping = s, .format = "list"),
        function(s) {
          dplyr::collect(nest_with_margins(
            input, .grouping = s, .id = "set", .margin_label = NULL
          ))
        },
        function(s) {
          dplyr::collect(nest_by_with_margins(
            input, .grouping = s, .id = "set", .margin_label = NULL
          ))
        }
      )
      for (specification in specifications) {
        original <- rlang::duplicate(specification)
        for (consume in consumers) {
          observed <- rlang::env(calls = 0L)
          expect_identical(consume(specification), consume(expected))
          expect_identical(observed$calls, 1L)
        }
        expect_identical(specification, original)
      }
    }
  }
}

test_that("authored wrappers agree with typed selection on local input", {
  data <- data.frame(a = c("east", "west"), b = c(1L, 1L), value = 11:12)
  check_authored_namespaces(data, data)
})

test_that("authored wrappers agree with typed selection on dtplyr input", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(a = c("east", "west"), b = c(1L, 1L), value = 11:12)
  check_authored_namespaces(dtplyr::lazy_dt(data), data)
})

test_that("an authored wrapper can return a different nested specification", {
  data <- data.frame(a = c("east", "west"), b = c(1L, 1L), value = 11:12)
  observed <- rlang::env(calls = c(namespace = 0L, constructor = 0L))
  nested <- grouping_set(b)
  caller <- rlang::env(s = grouping_set("a"))
  caller[["::"]] <- function(...) {
    observed$calls[["namespace"]] <- observed$calls[["namespace"]] + 1L
    function(x) {
      force(x)
      nested
    }
  }
  caller$grouping_set <- function(...) {
    observed$calls[["constructor"]] <- observed$calls[["constructor"]] + 1L
    marginplyr::grouping_set(a)
  }
  for (payload in list(quote(s), quote(grouping_set(a)))) {
    selection <- rlang::new_quosure(
      rlang::call2(quote(base::`(`), payload), caller
    )
    observed$calls[] <- 0L
    expect_identical(rlang::eval_tidy(selection), nested)
    expect_identical(observed$calls[["namespace"]], 1L)
    for (constructor in c("grouping_sets", "rollup", "cube", "grouping_spec")) {
      spec <- eval(rlang::call2(constructor, selection), caller)
      expected <- eval(rlang::call2(constructor, nested))
      verbs <- list(inspect_grouping, nest_with_margins, nest_by_with_margins)
      for (verb in verbs) {
        observed$calls[] <- 0L
        expect_identical(
          verb(data, .grouping = spec), verb(data, .grouping = expected)
        )
        expect_identical(observed$calls, c(
          namespace = 1L, constructor = as.integer(is.call(payload))
        ))
      }
    }
  }
})

test_that("each authored namespace occurrence retains its own first answer", {
  data <- data.frame(a = c("east", "west"), b = c(1L, 1L))
  observed <- rlang::env(calls = 0L)
  caller <- rlang::env(s = grouping_set("a"), `::` = function(...) {
    observed$calls <- observed$calls + 1L
    answer <- if (observed$calls == 1L) "b" else "a"
    function(x) answer
  })
  selection <- rlang::new_quosure(quote(base::`(`(s)), caller)
  expect_identical(
    inspect_grouping(
      data, .grouping = grouping_sets(!!selection, !!selection),
      .format = "list"
    )$included,
    list("b", "a")
  )
  expect_identical(observed$calls, 2L)
  spec <- grouping_sets(!!selection)
  observed <- rlang::env(calls = 0L)
  expect_identical(
    inspect_grouping(data, .grouping = spec, .format = "list")$included,
    list("b")
  )
  expect_identical(
    inspect_grouping(data, .grouping = spec, .format = "list")$included,
    list("a")
  )
  expect_identical(observed$calls, 2L)
})

test_that("ordinary authored wrappers retain their typed selection context", {
  data <- data.frame(a = c("east", "west"), b = c(1L, 1L))
  observed <- rlang::env(calls = 0L)
  caller <- rlang::env(`::` = function(...) {
    observed$calls <- observed$calls + 1L
    function(x) {
      if (is.character(tidyselect::peek_data()$a)) "b" else "a"
    }
  })
  selection <- rlang::new_quosure(quote(base::`(`("a")), caller)
  selected <- names(tidyselect::eval_select(selection, data))
  expect_identical(selected, "b")
  observed <- rlang::env(calls = 0L)
  expect_identical(
    inspect_grouping(
      data, .grouping = grouping_sets(!!selection), .format = "list"
    )$included,
    list(selected)
  )
  expect_identical(observed$calls, 1L)
})

test_that("authored evaluation preserves dtplyr read and error boundaries", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(a = c("east", "west"), b = c(1L, 1L), s = 11:12)
  input <- dtplyr::lazy_dt(data)
  counter <- local_dtplyr_proxy_counter()
  observed <- rlang::env(calls = 0L)
  caller <- rlang::env(s = grouping_set("a"), `::` = function(...) {
    observed$calls <- observed$calls + 1L
    function(x) "b"
  })
  qualified <- rlang::new_quosure(quote(base::`(`(s)), caller)
  expect_identical(
    inspect_grouping(
      input, .grouping = grouping_sets(!!qualified), .format = "list"
    )$included,
    list("b")
  )
  expect_identical(observed$calls, 1L)
  expect_identical(counter$source_evaluations, 0L)

  caller[["("]] <- function(...) stop("syntactic wrapper evaluated")
  ambiguous <- rlang::new_quosure(quote((s)), caller)
  invalid <- rollup()
  caller[["::"]] <- function(...) {
    observed$calls <- observed$calls + 1L
    function(x) invalid
  }
  before <- counter$n
  verbs <- list(inspect_grouping, nest_with_margins, nest_by_with_margins)
  for (verb in verbs) {
    observed <- rlang::env(calls = 0L)
    expect_error(
      verb(input, .grouping = grouping_sets(!!ambiguous, !!qualified)),
      "both a column", class = "marginplyr_error"
    )
    expect_identical(observed$calls, 0L)
    expect_error(
      verb(input, .grouping = grouping_sets(!!qualified, where(is.character))),
      "requires at least one dimension", class = "marginplyr_error"
    )
    expect_identical(observed$calls, 1L)
  }
  expect_identical(counter$n, before)
  expect_identical(counter$source_evaluations, 0L)
})
