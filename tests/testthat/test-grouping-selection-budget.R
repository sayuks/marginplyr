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
constructor_call_spellings <- function(constructor, operator = NULL) {
  head <- rlang::sym(constructor)
  if (!is.null(operator)) {
    head <- rlang::call2(operator, quote(marginplyr), head)
  }
  selection_call_spellings(head)
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


test_that("qualified constructor selections retain their first answer", {
  data <- data.frame(region = c("east", "west"), amount = 1:2)
  reads <- 0L
  choose <- function(...) {
    reads <<- reads + 1L
    if (reads == 1L) "region" else "amount"
  }
  operator_fn <- function(package, name) choose
  env <- rlang::env(rlang::current_env())
  for (operator in c("::", ":::")) {
    assign(operator, operator_fn, env)
    for (constructor in c("grouping_set", "grouping_sets", "rollup", "cube",
                          "grouping_spec")) {
      for (expr in constructor_call_spellings(constructor, operator)) {
        selection <- rlang::new_quosure(expr, env)
        spec <- grouping_sets(!!selection)
        original <- rlang::duplicate(spec)
        for (consume in grouping_selection_consumers(data)) {
          reads <- 0L
          expect_identical(consume(spec), consume(grouping_sets(region)))
          expect_identical(reads, 1L)
          expect_identical(spec, original)
          expect_identical(get(operator, env), operator_fn)
        }
      }
    }
  }
})

test_that("qualified constructor results keep conditions and diagnostics", {
  data <- data.frame(a = c("x", "y"), b = 1:2)
  reads <- 0L
  arguments <- 0L
  warning <- rlang::warning_cnd("selection_warning", message = "first answer")
  failure <- rlang::error_cnd("selection_error", message = "first failure")
  warn_once <- function(x = NULL) {
    reads <<- reads + 1L
    force(x)
    if (reads > 1L) stop("second invocation")
    warning(warning)
    "a"
  }
  env <- rlang::env(rlang::current_env(), argument = function() {
    arguments <<- arguments + 1L
  })
  for (operator in c("::", ":::")) {
    choose <- warn_once
    assign(operator, function(package, name) choose, env)
    for (constructor in c("grouping_set", "grouping_sets", "rollup", "cube",
                          "grouping_spec")) {
      for (expr in constructor_call_spellings(constructor, operator)) {
        selection <- rlang::new_quosure(expr, env)
        spec <- grouping_sets(!!selection)
        for (consume in grouping_selection_consumers(data)) {
          reads <- 0L
          seen <- list()
          result <- withCallingHandlers(consume(spec), warning = function(cnd) {
            seen[[length(seen) + 1L]] <<- cnd
            invokeRestart("muffleWarning")
          })
          expect_identical(result, consume(grouping_sets(a)))
          expect_identical(seen, list(warning))
          expect_identical(reads, 1L)
        }
      }
    }

    head <- rlang::call2(operator, quote(marginplyr), quote(rollup))
    expr <- rlang::call2(head, quote(argument()))
    selection <- rlang::new_quosure(expr, env)
    spec <- grouping_sets(!!selection)
    for (consume in grouping_selection_consumers(data)) {
      choose <- function(x = NULL) {
        reads <<- reads + 1L
        force(x)
        if (reads %% 2L == 1L) "a" else "b"
      }
      reads <- 0L
      arguments <- 0L
      expect_identical(consume(spec), consume(grouping_sets(a)))
      expect_identical(consume(spec), consume(grouping_sets(b)))
      expect_identical(reads, 2L)
      expect_identical(arguments, 2L)
      reads <- 0L
      arguments <- 0L
      expect_identical(consume(grouping_sets(!!selection, !!selection)),
                       consume(grouping_sets(a, b)))
      expect_identical(reads, 2L)
      expect_identical(arguments, 2L)

      choose <- function(...) {
        reads <<- reads + 1L
        stop(failure)
      }
      reads <- 0L
      expect_identical(tryCatch(consume(spec), error = identity), failure)
      expect_identical(reads, 1L)
    }
    for (expr in constructor_call_spellings("rollup", operator)) {
      selection <- rlang::new_quosure(expr, env)
      spec <- grouping_sets(!!selection)
      choose <- function(...) list("a")
      actual <- expect_error(inspect_grouping(data, .grouping = spec),
                             class = "vctrs_error_subscript_type")
      direct <- expect_error(tidyselect::eval_select(selection, data),
                             class = "vctrs_error_subscript_type")
      expect_identical(conditionMessage(actual), conditionMessage(direct))
      expect_identical(actual$subscript_arg, direct$subscript_arg)
    }
  }
})

test_that("qualified heads retain nested specifications and selection values", {
  data <- data.frame(a = c("x", "y"), b = 1:2)
  env <- rlang::env(rlang::current_env())
  for (operator in c("::", ":::")) {
    assign(operator, function(package, name) choose, env)
    for (constructor in c("grouping_set", "grouping_sets", "rollup", "cube",
                          "grouping_spec")) {
      construct <- getExportedValue("marginplyr", constructor)
      calls <- 0L
      choose <- function(...) {
        calls <<- calls + 1L
        construct(a)
      }
      for (expr in constructor_call_spellings(constructor, operator)) {
        selection <- rlang::new_quosure(expr, env)
        calls <- 0L
        expect_identical(inspect_grouping(data, .grouping = grouping_sets(
          !!selection
        )), inspect_grouping(data, .grouping = grouping_sets(!!construct(a))))
        expect_identical(calls, 1L)
      }
    }
    for (value in list(quote(a), is.character, NULL, c(renamed = "a"), 3L)) {
      choose <- function(...) value
      selection <- rlang::new_quosure(
        rlang::call2(rlang::call2(operator, quote(marginplyr), quote(rollup))),
        env
      )
      control <- grouping_sets(choose())
      actual <- tryCatch(inspect_grouping(
        data, .grouping = grouping_sets(!!selection)
      ), error = identity)
      expected <- tryCatch(inspect_grouping(data, .grouping = control),
                           error = identity)
      if (inherits(expected, "error")) {
        expect_identical(class(actual), class(expected))
      } else {
        expect_identical(actual, expected)
      }
    }
  }
})

test_that("recognized caller constructors see actual local columns once", {
  data <- data.frame(a = c(1L, 2L), b = c("x", "y"), c = c(TRUE, FALSE))
  original_data <- rlang::duplicate(data)
  calls <- 0L
  nested <- FALSE
  choose <- function(...) {
    calls <<- calls + 1L
    columns <- tidyselect::peek_data()
    if (!identical(columns, data)) stop("invented selection context")
    selected <- if (calls == 1L) {
      names(columns)[vapply(columns, is.integer, logical(1))]
    } else {
      "b"
    }
    if (nested) marginplyr::grouping_set(!!selected) else selected
  }
  caller <- rlang::env(rlang::current_env())
  for (constructor in c("grouping_set", "grouping_sets", "rollup", "cube",
                        "grouping_spec")) {
    caller[[constructor]] <- choose
    for (operator in list(NULL, "::", ":::")) {
      if (!is.null(operator)) caller[[operator]] <- function(...) choose
      for (expr in constructor_call_spellings(constructor, operator)) {
        selection <- rlang::new_quosure(expr, caller)
        specification <- grouping_sets(!!selection)
        original <- rlang::duplicate(specification)
        for (nested_value in c(FALSE, TRUE)) {
          nested <- nested_value
          for (consume in grouping_selection_consumers(data)) {
            calls <- 0L
            expect_identical(consume(specification), consume(grouping_sets(a)))
            expect_identical(calls, 1L)
          }
        }
        expect_identical(specification, original)
        expect_identical(caller[[constructor]], choose)
      }
    }
  }
  expect_identical(data, original_data)
})

test_that("unknown constructor callbacks refuse invented SQL column types", {
  calls <- 0L
  choose <- function(...) {
    calls <<- calls + 1L
    columns <- tidyselect::peek_data()
    if (all(vapply(columns, is.logical, logical(1)))) return("a")
    "b"
  }
  caller <- rlang::env(rollup = choose)
  for (con in list(dbplyr::simulate_dbi(), dbplyr::simulate_postgres())) {
    input <- dbplyr::lazy_frame(a = 1L, b = "x", con = con)
    for (operator in list(NULL, "::", ":::")) {
      if (!is.null(operator)) caller[[operator]] <- function(...) choose
      for (expr in constructor_call_spellings("rollup", operator)) {
        selection <- rlang::new_quosure(expr, caller)
        specification <- grouping_sets(!!selection)
        # SQL nesting is refused at verb entry, before grouping is read.
        for (consume in grouping_selection_consumers(input)[1:3]) {
          calls <- 0L
          expect_error(consume(specification), "faithful.*selection context",
                       class = "marginplyr_error")
          expect_identical(calls, 0L)
        }
      }
    }
    expect_identical(
      inspect_grouping(
        input, .grouping = grouping_sets(grouping_set(a))
      )$included,
      "(a)"
    )
  }
})

test_that("caller constructor answers retain each parent's existing grammar", {
  data <- data.frame(a = 1L, b = "x")
  calls <- 0L
  nested <- FALSE
  choose <- function(...) {
    calls <<- calls + 1L
    selected <- names(tidyselect::peek_data())[
      vapply(tidyselect::peek_data(), is.integer, logical(1))
    ]
    if (nested) marginplyr::grouping_set(!!selected) else selected
  }
  caller <- rlang::env()
  constructors <- c("grouping_set", "grouping_sets", "rollup", "cube",
                    "grouping_spec")
  for (constructor in constructors) {
    caller[[constructor]] <- choose
    selection <- rlang::new_quosure(rlang::call2(constructor), caller)
    for (parent in constructors) {
      construct <- getExportedValue("marginplyr", parent)
      for (nested_value in c(FALSE, TRUE)) {
        nested <- nested_value
        specification <- construct(!!selection)
        calls <- 0L
        if (nested && parent == "grouping_set") {
          expect_error(inspect_grouping(data, .grouping = specification),
                       "not another grouping family",
                       class = "marginplyr_error")
        } else {
          expected <- if (nested) {
            construct(!!grouping_set(a))
          } else {
            construct(a)
          }
          expect_identical(inspect_grouping(data, .grouping = specification),
                           inspect_grouping(data, .grouping = expected))
        }
        expect_identical(calls, 1L)
      }
    }
  }
})

test_that("forwarded caller constructors keep their captured typed context", {
  data <- data.frame(a = 1L, b = "x")
  calls <- 0L
  caller <- rlang::env(rollup = function(...) {
    calls <<- calls + 1L
    if (!is.integer(tidyselect::peek_data()$a)) stop("invented column type")
    "a"
  })
  caller$embrace <- function(x) grouping_sets({{ x }})
  caller$dots <- function(...) grouping_sets(...)
  caller$numbered <- function(...) grouping_sets(..1)
  selection <- rlang::new_quosure(quote(rollup()), caller)
  specifications <- list(
    grouping_sets(!!selection),
    grouping_sets(!!!list(selection)),
    eval(quote(embrace(rollup())), caller),
    eval(quote(dots(rollup())), caller),
    eval(quote(numbered(rollup())), caller)
  )
  for (specification in specifications) {
    for (consume in grouping_selection_consumers(data)) {
      calls <- 0L
      expect_identical(consume(specification), consume(grouping_sets(a)))
      expect_identical(calls, 1L)
    }
  }
})

test_that("dtplyr caller constructors share one faithful zero-row snapshot", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(a = 1:2, b = c("x", "y"),
                     f = factor(c("u", "u"), levels = c("u", "unused")))
  input <- dtplyr::lazy_dt(data)
  original <- rlang::duplicate(input)
  counter <- local_dtplyr_proxy_counter()
  calls <- 0L
  choose <- function(...) {
    calls <<- calls + 1L
    columns <- tidyselect::peek_data()
    if (nrow(columns) != 0L || !is.integer(columns$a) ||
          !is.character(columns$b) ||
          !identical(levels(columns$f), c("u", "unused"))) {
      stop("unfaithful zero-row context")
    }
    if (calls == 1L) "a" else "b"
  }
  caller <- rlang::env(rollup = choose)
  for (operator in list(NULL, "::", ":::")) {
    if (!is.null(operator)) caller[[operator]] <- function(...) choose
    selection <- rlang::new_quosure(
      constructor_call_spellings("rollup", operator)[[1L]], caller
    )
    specification <- grouping_sets(!!selection)
    for (consume in grouping_selection_consumers(input)) {
      calls <- 0L
      before <- counter$n
      source_before <- counter$source_evaluations
      actual <- consume(specification)
      expect_identical(counter$n, before + 1L)
      expect_identical(calls, 1L)
      # The row-wise nesting verb explicitly collects its result.
      if (!inherits(actual, "rowwise_df")) {
        expect_identical(counter$source_evaluations, source_before)
      }
      expect_identical(dplyr::collect(actual),
                       dplyr::collect(consume(grouping_sets(a))))
    }
  }
  expect_identical(input, original)
})

test_that("unknown constructors suspend before unsafe dtplyr work", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(a = 1:2, b = c("x", "y"))
  rows <- 0L
  input <- dtplyr::lazy_dt(data) |> dplyr::mutate(a = {
    rows <<- rows + length(a)
    as.character(a)
  })
  counter <- local_dtplyr_proxy_counter()
  calls <- 0L
  later <- 0L
  choose <- function(...) {
    calls <<- calls + 1L
    marginplyr::grouping_set(a)
  }
  for (binding in c("ordinary", "active", "promise")) {
    caller <- rlang::env(last = function(...) {
      later <<- later + 1L
      "b"
    })
    if (binding == "active") {
      makeActiveBinding("rollup", function() {
        calls <<- calls + 1L
        choose
      }, caller)
    } else if (binding == "promise") {
      delayedAssign("rollup", {
        calls <<- calls + 1L
        choose
      }, assign.env = caller)
    } else {
      caller$rollup <- choose
    }
    specification <- eval(quote(marginplyr::grouping_sets(rollup(), last())),
                          caller)
    for (consume in grouping_selection_consumers(input)) {
      expect_error(consume(specification), "source rows",
                   class = "marginplyr_error")
      expect_identical(calls, 0L)
      expect_identical(later, 0L)
      expect_identical(rows, 0L)
      expect_identical(counter$n, 0L)
      expect_identical(counter$source_evaluations, 0L)
    }
    if (binding == "active") expect_true(bindingIsActive("rollup", caller))
    if (binding == "promise") {
      expect_true(rlang::env_binding_are_lazy(caller, "rollup")[[1L]])
    }
  }
})

test_that("bare caller deferral preserves prefix and resumed preflight order", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(a = 1:2, b = c("x", "y"), fixed = "all", s = 1L)
  input <- dtplyr::lazy_dt(data)
  unsafe <- dplyr::mutate(input, a = as.character(a))
  counter <- local_dtplyr_proxy_counter()
  events <- character()
  caller <- rlang::env(s = grouping_set(a), rollup = function(...) {
    events <<- c(events, "callback")
    "a"
  }, cube = function(...) {
    events <<- c(events, "later")
    "b"
  })
  pending <- rlang::new_quosure(quote(rollup()), caller)
  later <- rlang::new_quosure(quote(cube()), caller)
  for (prefix in list(quote(all_of("absent")), quote(s))) {
    first <- rlang::new_quosure(prefix, caller)
    expect_error(inspect_grouping(
      unsafe, .grouping = grouping_sets(!!first, !!pending, !!later)
    ), if (identical(prefix, quote(s))) "both a column" else "absent")
    expect_identical(events, character())
    expect_identical(counter$n, 0L)
  }
  expect_error(inspect_grouping(
    unsafe, .grouping = grouping_sets(!!pending, !!later)
  ), "source rows", class = "marginplyr_error")
  expect_identical(events, character())
  expect_identical(counter$n, 0L)

  by <- rlang::quo(identity({
    events <<- c(events, "by")
    "fixed"
  }))
  before <- counter$n
  result <- inspect_grouping(
    input, .grouping = grouping_sets(!!pending, !!later), .by = !!by
  )
  expect_identical(result$included, c("(a)", "(b)"))
  expect_identical(events, c("callback", "later", "by"))
  expect_identical(counter$n, before + 1L)
  expect_identical(counter$source_evaluations, 0L)

  # A later ambiguous binding is reachable only after the deferred callback.
  events <- character()
  ambiguous <- rlang::new_quosure(quote(s), caller)
  before <- counter$n
  expect_error(inspect_grouping(
    input, .grouping = grouping_sets(!!pending, !!ambiguous), .by = !!by
  ), "both a column", class = "marginplyr_error")
  expect_identical(events, "callback")
  expect_identical(counter$n, before + 1L)
})

test_that("bare stock capture proves operands without forcing unknown work", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(a = 1:2, b = c("x", "y"))
  input <- dtplyr::lazy_dt(data)
  unsafe <- dplyr::mutate(input, a = as.character(a))
  counter <- local_dtplyr_proxy_counter()
  calls <- 0L
  choose <- function() {
    calls <<- calls + 1L
    if (!is.integer(tidyselect::peek_data()$a)) stop("invented column type")
    "a"
  }
  caller <- rlang::env(rlang::current_env(), columns = "a", choose = choose)
  for (expr in list(quote(grouping_set(!!"a")),
                    quote(grouping_set(!!columns)),
                    quote(grouping_set(!!!list("a"))),
                    quote(grouping_set(a)))) {
    selection <- rlang::new_quosure(expr, caller)
    expect_identical(inspect_grouping(
      unsafe, .grouping = grouping_sets(!!selection)
    )$included, "(a)")
    expect_identical(counter$n, 0L)
  }
  selection <- rlang::new_quosure(quote(grouping_set(!!choose())), caller)
  expect_error(inspect_grouping(
    unsafe, .grouping = grouping_sets(!!selection)
  ), "source rows", class = "marginplyr_error")
  expect_identical(calls, 0L)
  expect_identical(counter$n, 0L)
  expect_identical(inspect_grouping(
    input, .grouping = grouping_sets(!!selection)
  )$included, "(a)")
  expect_identical(calls, 1L)
  expect_identical(counter$n, 1L)
  expect_identical(counter$source_evaluations, 0L)
  for (binding in c("active", "promise")) {
    caller <- rlang::env(rlang::current_env())
    if (binding == "active") {
      makeActiveBinding("columns", choose, caller)
    } else {
      delayedAssign("columns", choose(), assign.env = caller)
    }
    selection <- rlang::new_quosure(quote(grouping_set(!!columns)), caller)
    calls <- 0L
    before <- counter$n
    expect_error(inspect_grouping(
      unsafe, .grouping = grouping_sets(!!selection)
    ), "source rows", class = "marginplyr_error")
    expect_identical(calls, 0L)
    expect_identical(counter$n, before)
    expect_identical(inspect_grouping(
      input, .grouping = grouping_sets(!!selection)
    )$included, "(a)")
    expect_identical(calls, 1L)
    expect_identical(counter$n, before + 1L)
  }
})

test_that("caller context preserves the nested constructor spelling gate", {
  data <- data.frame(a = 1L, b = "x")
  calls <- 0L
  choose <- function(...) {
    calls <<- calls + 1L
    marginplyr::grouping_set(a)
  }
  caller <- rlang::env(alias = choose, rollup = choose,
                       `::` = function(...) choose)
  for (expr in list(quote(alias()), quote(base::rollup()),
                    quote(get("rollup")()), quote(identity(rollup())))) {
    selection <- rlang::new_quosure(expr, caller)
    calls <- 0L
    expect_error(inspect_grouping(data, .grouping = grouping_sets(!!selection)),
                 class = "marginplyr_error")
    expect_identical(calls, 1L)
  }
  # Top-level `.grouping` remains ordinary expression evaluation.
  caller$rollup <- function(...) {
    calls <<- calls + 1L
    tidyselect::peek_data()
  }
  calls <- 0L
  call <- quote(inspect_grouping(data, .grouping = rollup()))
  expect_error(eval(call, caller),
               "selecting", fixed = TRUE)
  expect_identical(calls, 1L)
})

test_that("DuckDB caller constructors require one safe faithful snapshot", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  data <- data.frame(a = 1:2, b = c("x", "y"))
  DBI::dbWriteTable(con, "caller_context", data)
  input <- dplyr::tbl(con, "caller_context")
  calls <- 0L
  choose <- function(...) {
    calls <<- calls + 1L
    columns <- tidyselect::peek_data()
    if (nrow(columns) != 0L || !is.integer(columns$a) ||
          !is.character(columns$b)) stop("invented selection context")
    "a"
  }
  caller <- rlang::env(rollup = choose)
  rlang::local_options(marginplyr.audit_sql = TRUE)
  for (operator in list(NULL, "::", ":::")) {
    if (!is.null(operator)) caller[[operator]] <- function(...) choose
    selection <- rlang::new_quosure(
      constructor_call_spellings("rollup", operator)[[1L]], caller
    )
    for (consume in grouping_selection_consumers(input)[1:3]) {
      calls <- 0L
      result <- consume(grouping_sets(!!selection))
      expect_identical(calls, 1L)
      audit <- last_sent_queries(details = TRUE)
      expect_identical(sum(audit$purpose == "selection_proxy"), 1L)
      ordered <- function(x) {
        dplyr::arrange(dplyr::collect(x), dplyr::across(dplyr::everything()))
      }
      expect_identical(ordered(result), ordered(consume(grouping_sets(a))))
    }
  }
  # Explicit names avoid binding the file before marginplyr's admission check.
  path <- tempfile(fileext = ".csv")
  unsafe <- dplyr::tbl(con, dbplyr::sql(paste(
    "SELECT * FROM read_csv_auto(", DBI::dbQuoteString(con, path), ")"
  )), vars = c("a", "b"))
  selection <- rlang::new_quosure(quote(rollup()), caller)
  for (consume in grouping_selection_consumers(unsafe)[1:3]) {
    calls <- 0L
    expect_error(consume(grouping_sets(!!selection)), "source rows",
                 class = "marginplyr_error")
    expect_identical(calls, 0L)
    expect_identical(nrow(last_sent_queries()), 0L)
  }
})

test_that("Arrow caller constructors see the available schema", {
  skip_if_suggest_absent("arrow")
  data <- data.frame(a = 1:2, b = c("x", "y"))
  input <- arrow::Table$create(data)
  calls <- 0L
  caller <- rlang::env(rollup = function(...) {
    calls <<- calls + 1L
    columns <- tidyselect::peek_data()
    if (nrow(columns) != 0L || !is.integer(columns$a) ||
          !is.character(columns$b)) stop("invented schema context")
    "a"
  })
  selection <- rlang::new_quosure(quote(rollup()), caller)
  for (consume in grouping_selection_consumers(input)[1:3]) {
    calls <- 0L
    actual <- consume(grouping_sets(!!selection))
    expect_identical(calls, 1L)
    expect_identical(dplyr::collect(actual),
                     dplyr::collect(consume(grouping_sets(a))))
  }
  expect_identical(as.data.frame(input), data)
})
