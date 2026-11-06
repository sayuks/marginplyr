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
        if (!is.character(tidyselect::peek_data()$a)) {
          stop("namespace operation saw the wrong column type")
        }
        function(x) {
          if (is.character(tidyselect::peek_data()$a)) "b" else "a"
        }
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

test_that("authored wrappers around bound specs see typed selections", {
  data <- data.frame(a = c("east", "west"), b = c(1L, 1L))
  observed <- rlang::env(calls = 0L)
  caller <- rlang::env(s = grouping_set(a), `::` = function(...) {
    observed$calls <- observed$calls + 1L
    function(x) {
      if (is.character(tidyselect::peek_data()$a)) "b" else "a"
    }
  })
  selection <- rlang::new_quosure(quote(base::`(`(s)), caller)
  selected <- names(tidyselect::eval_select(selection, data))
  expect_identical(selected, "b")
  expect_identical(observed$calls, 1L)
  observed$calls <- 0L
  expect_identical(
    inspect_grouping(
      data, .grouping = grouping_sets(!!selection), .format = "list"
    )$included,
    list(selected)
  )
  expect_identical(observed$calls, 1L)
})

test_that("genuine identity wrappers retain deferred payload context", {
  data <- data.frame(a = c("east", "west"), b = c(1L, 1L), value = 11:12)
  observed <- rlang::env(calls = 0L)
  typed_selection <- function(...) {
    observed$calls <- observed$calls + 1L
    if (is.character(tidyselect::peek_data()$a)) "b" else "a"
  }
  caller <- rlang::env(grouping_set = typed_selection)
  makeActiveBinding("s", typed_selection, caller)
  for (payload in list(quote(s), quote(grouping_set(a)))) {
    selection <- rlang::new_quosure(
      rlang::call2(quote(base::`(`), payload), caller
    )
    observed$calls <- 0L
    expect_identical(names(tidyselect::eval_select(selection, data)), "b")
    expect_identical(observed$calls, 1L)
    verbs <- list(inspect_grouping, nest_with_margins, nest_by_with_margins)
    for (verb in verbs) {
      observed$calls <- 0L
      expect_identical(
        verb(data, .grouping = grouping_sets(!!selection)),
        verb(data, .grouping = grouping_sets(b))
      )
      expect_identical(observed$calls, 1L)
    }
  }
})

test_that("stock constructor injection keeps its typed captured environment", {
  data <- data.frame(a = c("east", "west"), b = c(1L, 1L), value = 11:12)
  observed <- rlang::env(calls = 0L, spec = NULL)
  caller <- rlang::env(choose = function() {
    observed$calls <- observed$calls + 1L
    if (is.character(tidyselect::peek_data()$a)) {
      rlang::sym("b")
    } else {
      rlang::sym("a")
    }
  })
  expression <- quote(base::`(`(marginplyr::grouping_set(!!choose())))
  selection <- rlang::new_quosure(expression, caller)
  capture <- function(value) {
    observed$spec <- value
    integer()
  }
  tidyselect::eval_select(
    rlang::new_quosure(rlang::call2(capture, expression), caller), data
  )
  expect_identical(observed$calls, 1L)
  expect_identical(
    inspect_grouping(
      data, .grouping = observed$spec, .format = "list"
    )$included,
    list("b")
  )
  verbs <- list(inspect_grouping, nest_with_margins, nest_by_with_margins)
  for (verb in verbs) {
    observed$calls <- 0L
    actual <- verb(data, .grouping = grouping_sets(!!selection))
    expect_identical(
      actual,
      verb(data, .grouping = grouping_sets(grouping_set(b)))
    )
    expect_identical(observed$calls, 1L)
    observed$calls <- 0L
    wrapped <- verb(data, .grouping = grouping_sets(base::`(`(!!selection)))
    expect_identical(wrapped, actual)
    expect_identical(observed$calls, 1L)
  }
})

test_that("stock constructor embracing retains a deferred caller promise", {
  data <- data.frame(a = c("east", "west"), b = c(1L, 1L), value = 11:12)
  observed <- rlang::env(calls = 0L)
  choose <- function() {
    observed$calls <- observed$calls + 1L
    if (is.character(tidyselect::peek_data()$a)) "b" else "a"
  }
  capture <- function(x) {
    rlang::new_quosure(
      quote(base::`(`(marginplyr::grouping_set({{ x }}))), environment()
    )
  }
  selection <- capture(choose())
  verbs <- list(inspect_grouping, nest_with_margins, nest_by_with_margins)
  for (verb in verbs) {
    observed$calls <- 0L
    actual <- verb(data, .grouping = grouping_sets(!!selection))
    expect_identical(
      actual, verb(data, .grouping = grouping_sets(grouping_set(b)))
    )
    expect_identical(observed$calls, 1L)
  }
})

test_that("proving forwarded injection does not force an active binding", {
  data <- data.frame(a = c("east", "west"), b = c(1L, 1L))
  observed <- rlang::env(calls = 0L)
  caller <- new.env(parent = baseenv())
  makeActiveBinding("selected", function() {
    observed$calls <- observed$calls + 1L
    if (is.character(tidyselect::peek_data()$a)) "b" else "a"
  }, caller)
  forward <- function(x) {
    rlang::new_quosure(
      quote(base::`(`(marginplyr::grouping_set(!!x))), environment()
    )
  }
  verbs <- list(inspect_grouping, nest_with_margins, nest_by_with_margins)
  for (verb in verbs) {
    observed$calls <- 0L
    selection <- rlang::eval_bare(
      rlang::call2(forward, quote(selected)), caller
    )
    expect_identical(observed$calls, 0L)
    actual <- verb(data, .grouping = grouping_sets(!!selection))
    expect_identical(
      actual, verb(data, .grouping = grouping_sets(grouping_set(b)))
    )
    expect_identical(observed$calls, 1L)
  }
})

test_that("known injections retain name-only derived dtplyr inspection", {
  skip_if_suggest_absent("dtplyr")
  observed <- rlang::env(rows = 0L)
  touch <- function(x) {
    observed$rows <- observed$rows + length(x)
    x
  }
  data <- data.frame(a = c("east", "west"), b = c(1L, 1L))
  input <- dplyr::mutate(dtplyr::lazy_dt(data), a = touch(a))
  counter <- local_dtplyr_proxy_counter()
  caller <- rlang::env(
    columns = "a", pieces = list(rlang::new_quosure(quote(a), emptyenv()))
  )
  caller$quosures <- rlang::new_quosures(caller$pieces)
  caller$column <- caller$pieces[[1L]]
  expressions <- list(
    quote(marginplyr::grouping_set(!!"a")),
    quote(marginplyr::grouping_set(!!quote(a))),
    quote(marginplyr::grouping_set(!!columns)),
    quote(marginplyr::grouping_set(!!!list("a"))),
    quote(marginplyr::grouping_set(!!!pieces)),
    quote(marginplyr::grouping_set(!!!quosures)),
    quote(marginplyr::grouping_set(!!column))
  )
  selections <- lapply(expressions, function(expr) {
    rlang::new_quosure(rlang::call2(quote(base::`(`), expr), caller)
  })
  embrace <- function(x) {
    rlang::new_quosure(
      quote(base::`(`(marginplyr::grouping_set({{ x }}))), environment()
    )
  }
  forward <- function(x) {
    rlang::new_quosure(
      quote(base::`(`(marginplyr::grouping_set(!!x))), environment()
    )
  }
  column <- caller$column
  selections <- c(selections, list(embrace(a), forward(column)))
  reference_error <- tryCatch(
    inspect_grouping(input, .grouping = grouping_set(grouping_set(a))),
    error = identity
  )
  for (selection in selections) {
    for (constructor in c("grouping_sets", "rollup", "cube", "grouping_spec")) {
      specification <- eval(rlang::call2(constructor, selection))
      expected <- eval(rlang::call2(constructor, grouping_set(a)))
      reference <- inspect_grouping(
        input, .grouping = expected, .format = "list"
      )
      actual <- inspect_grouping(
        input, .grouping = specification, .format = "list"
      )
      expect_identical(actual, reference)
    }
    error <- tryCatch(
      inspect_grouping(input, .grouping = grouping_set(!!selection)),
      error = identity
    )
    expect_s3_class(error, "marginplyr_error")
    expect_identical(conditionMessage(error), conditionMessage(reference_error))
  }
  expect_identical(counter$n, 0L)
  expect_identical(counter$source_evaluations, 0L)
  expect_identical(observed$rows, 0L)
})

test_that("forwarded quosure bodies do not change early grammar refusal", {
  skip_if_suggest_absent("dtplyr")
  observed <- rlang::env(rows = 0L)
  touch <- function(x) {
    observed$rows <- observed$rows + length(x)
    x
  }
  data <- data.frame(a = c("east", "west"), b = c(1L, 1L))
  input <- dplyr::mutate(dtplyr::lazy_dt(data), a = touch(a))
  counter <- local_dtplyr_proxy_counter()
  typed <- rlang::new_quosure(quote(where(is.character)), baseenv())
  forward <- function(x) {
    rlang::new_quosure(
      quote(base::`(`(marginplyr::grouping_set(!!x))), environment()
    )
  }
  selection <- rlang::eval_bare(rlang::call2(forward, typed))
  error <- tryCatch(
    inspect_grouping(input, .grouping = grouping_set(!!selection)),
    error = identity
  )
  expect_s3_class(error, "marginplyr_error")
  expect_match(conditionMessage(error), "can contain columns")
  expect_identical(counter$n, 0L)
  expect_identical(counter$source_evaluations, 0L)
  expect_identical(observed$rows, 0L)
})

test_that("injection helpers retain the complete names selection context", {
  data <- data.frame(a = c("east", "west"), b = c(1L, 1L))
  caller <- new.env(parent = baseenv())
  operands <- list(
    quote(everything()), quote(tidyselect::peek_vars()),
    quote(names(tidyselect::peek_data()))
  )
  for (operand in operands) {
    constructor <- rlang::call2(
      quote(marginplyr::grouping_set),
      rlang::call2("!", rlang::call2("!", operand))
    )
    selection <- rlang::new_quosure(
      rlang::call2(quote(base::`(`), constructor), caller
    )
    verbs <- list(inspect_grouping, nest_with_margins, nest_by_with_margins)
    for (verb in verbs) {
      actual <- verb(data, .grouping = grouping_sets(!!selection))
      expect_identical(
        actual, verb(data, .grouping = grouping_sets(grouping_set(a, b)))
      )
    }
  }
})

test_that("unproven injection callee parentheses retain typed selection", {
  data <- data.frame(a = c("east", "west"), b = c(1L, 1L))
  observed <- rlang::env(calls = 0L)
  caller <- rlang::env(baseenv(), `(` = function(x) {
    observed$calls <- observed$calls + 1L
    selected <- if (is.character(tidyselect::peek_data()$a)) "b" else "a"
    function(...) selected
  })
  selection <- rlang::new_quosure(
    quote(base::`(`(marginplyr::grouping_set(!!(list)()))), caller
  )
  verbs <- list(inspect_grouping, nest_with_margins, nest_by_with_margins)
  for (verb in verbs) {
    observed$calls <- 0L
    actual <- verb(data, .grouping = grouping_sets(!!selection))
    expect_identical(
      actual, verb(data, .grouping = grouping_sets(grouping_set(b)))
    )
    expect_identical(observed$calls, 1L)
  }
})

test_that("typed authored failures retain the caller's condition once", {
  data <- data.frame(a = c("east", "west"), b = c(1L, 1L))
  observed <- rlang::env(calls = 0L)
  failure <- rlang::error_cnd(
    "authored_selection_error", message = "caller failure"
  )
  caller <- rlang::env(s = grouping_set(a), `::` = function(...) {
    observed$calls <- observed$calls + 1L
    function(x) stop(failure)
  })
  selection <- rlang::new_quosure(quote(base::`(`(s)), caller)
  verbs <- list(inspect_grouping, nest_with_margins, nest_by_with_margins)
  for (verb in verbs) {
    observed$calls <- 0L
    actual <- expect_error(
      verb(data, .grouping = grouping_sets(!!selection)),
      class = "authored_selection_error"
    )
    expect_identical(class(actual), class(failure))
    expect_identical(conditionMessage(actual), conditionMessage(failure))
    expect_identical(actual$parent, failure$parent)
    expect_identical(actual$call, failure$call)
    expect_identical(observed$calls, 1L)
  }
})

test_that("typed continuation preserves caller order and warnings", {
  data <- data.frame(a = c("east", "west"), b = c(1L, 1L),
                     value = 11:12, fixed = "all")
  observed <- rlang::env(order = character())
  warning <- rlang::warning_cnd(
    "authored_selection_warning", message = "caller warning"
  )
  first <- rlang::new_quosure(quote(grouping_set()), rlang::env(
    grouping_set = function(...) {
      observed$order <- c(observed$order, "first")
      marginplyr::grouping_set(a)
    }
  ))
  last <- rlang::new_quosure(quote(grouping_set()), rlang::env(
    grouping_set = function(...) {
      observed$order <- c(observed$order, "last")
      marginplyr::grouping_set(value)
    }
  ))
  caller <- rlang::env(s = grouping_set(a), `::` = function(...) {
    observed$order <- c(observed$order, "namespace")
    warning(warning)
    function(x) {
      if (is.character(tidyselect::peek_data()$a)) "b" else "a"
    }
  })
  selection <- rlang::new_quosure(quote(base::`(`(s)), caller)
  by <- rlang::quo(identity({
    observed$order <- c(observed$order, "by")
    "fixed"
  }))
  verbs <- list(inspect_grouping, nest_with_margins, nest_by_with_margins)
  for (verb in verbs) {
    observed$order <- character()
    seen <- list()
    actual <- withCallingHandlers(
      verb(
        data, .grouping = grouping_sets(!!first, !!selection, !!last),
        .by = !!by
      ),
      warning = function(cnd) {
        seen[[length(seen) + 1L]] <<- cnd
        invokeRestart("muffleWarning")
      }
    )
    expect_identical(
      actual, verb(data, .grouping = grouping_sets(a, b, value), .by = fixed)
    )
    expect_identical(observed$order, c("first", "namespace", "last", "by"))
    expect_identical(seen, list(warning))
  }
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
  expect_identical(counter$n, 1L)
  expect_identical(counter$source_evaluations, 0L)

  caller[["("]] <- function(...) stop("syntactic wrapper evaluated")
  ambiguous <- rlang::new_quosure(quote((s)), caller)
  invalid <- rollup()
  caller[["::"]] <- function(...) {
    observed$calls <- observed$calls + 1L
    function(x) invalid
  }
  verbs <- list(inspect_grouping, nest_with_margins, nest_by_with_margins)
  for (verb in verbs) {
    observed <- rlang::env(calls = 0L)
    before <- counter$n
    expect_error(
      verb(input, .grouping = grouping_sets(!!ambiguous, !!qualified)),
      "both a column", class = "marginplyr_error"
    )
    expect_identical(observed$calls, 0L)
    expect_identical(counter$n, before)
    identity <- rlang::new_quosure(
      quote(base::`(`(rollup())), rlang::env()
    )
    expect_error(
      verb(input, .grouping = grouping_sets(!!identity, !!qualified)),
      "requires at least one dimension", class = "marginplyr_error"
    )
    expect_identical(observed$calls, 0L)
    expect_identical(counter$n, before)
    expect_error(
      verb(input, .grouping = grouping_sets(!!qualified, where(is.character))),
      "requires at least one dimension", class = "marginplyr_error"
    )
    expect_identical(observed$calls, 1L)
    expect_identical(counter$n, before + 1L)
  }
  expect_identical(counter$source_evaluations, 0L)
})

test_that("leading errors precede an authored call's typed metadata", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(a = c("east", "west"), b = c(1L, 1L))
  input <- dtplyr::lazy_dt(data)
  counter <- local_dtplyr_proxy_counter()
  observed <- rlang::env(calls = 0L)
  caller <- rlang::env(s = grouping_set(a), `::` = function(...) {
    observed$calls <- observed$calls + 1L
    if (!is.character(tidyselect::peek_data()$a)) stop("wrong type")
    function(x) "b"
  })
  selection <- rlang::new_quosure(quote(base::`(`(s)), caller)
  specifications <- list(
    grouping_set(missing_column, !!selection),
    grouping_sets(missing_column, !!selection),
    rollup(missing_column, !!selection),
    cube(missing_column, !!selection),
    grouping_spec(missing_column, !!selection),
    grouping_spec(grouping_sets(missing_column, !!selection), a)
  )
  for (specification in specifications) {
    verbs <- list(inspect_grouping, nest_with_margins, nest_by_with_margins)
    for (verb in verbs) {
      expect_error(verb(input, .grouping = specification), "missing_column")
      expect_identical(observed$calls, 0L)
      expect_identical(counter$n, 0L)
      expect_identical(counter$source_evaluations, 0L)
    }
  }
})

test_that("pending authored families do not prove duplicate positions", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(a = c("east", "west"), b = c(1L, 1L))
  input <- dtplyr::lazy_dt(data)
  counter <- local_dtplyr_proxy_counter()
  observed <- rlang::env(calls = 0L)
  caller <- rlang::env(s = grouping_set(a), `::` = function(...) {
    observed$calls <- observed$calls + 1L
    if (!is.character(tidyselect::peek_data()$a)) stop("wrong type")
    function(x) grouping_sets(b, grouping_set())
  })
  selection <- rlang::new_quosure(quote(base::`(`(s)), caller)
  specification <- grouping_spec(grouping_sets(a, a), !!selection)
  expected <- grouping_spec(
    grouping_sets(a, a), grouping_sets(b, grouping_set())
  )
  reference <- tryCatch(
    inspect_grouping(data, .grouping = expected), error = identity
  )
  actual <- tryCatch(
    inspect_grouping(input, .grouping = specification), error = identity
  )
  expect_s3_class(reference, "marginplyr_error")
  expect_s3_class(actual, "marginplyr_error")
  expect_identical(conditionMessage(actual), conditionMessage(reference))
  expect_identical(observed$calls, 1L)
  expect_identical(counter$n, 1L)
  expect_identical(counter$source_evaluations, 0L)
})
