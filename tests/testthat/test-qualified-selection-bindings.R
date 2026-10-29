# Qualified calls and their transparent head/call parentheses share one
# selection contract. The arguments stay unevaluated until selection runs.
qualified_selection_spellings <- function(operator, namespace, name, ...) {
  head <- rlang::call2(operator, rlang::sym(namespace), rlang::sym(name))
  call <- rlang::call2(head, ...)
  wrapped <- rlang::call2(rlang::call2("(", head), ...)
  list(call, rlang::call2("(", call), wrapped,
       rlang::call2("(", rlang::call2("(", wrapped)))
}

test_that("caller-qualified selections use the input's column types", {
  data <- data.frame(region = c("east", "west"), amount = 1:2)
  calls <- 0L
  resolutions <- 0L
  typed <- function(...) {
    calls <<- calls + 1L
    proxy <- tidyselect::peek_data()
    names(proxy)[vapply(proxy, is.character, logical(1))]
  }
  operator_fn <- function(package, name) {
    resolutions <<- resolutions + 1L
    typed
  }
  env <- rlang::env(rlang::current_env())
  for (operator in c("::", ":::")) {
    assign(operator, operator_fn, env)
    for (shape in list(c("dplyr", "all_of"), c("base", "names"))) {
      for (expr in qualified_selection_spellings(
        operator, shape[[1L]], shape[[2L]], "amount"
      )) {
        if (shape[[1L]] == "base") expr <- rlang::call2("all_of", expr)
        selection <- rlang::new_quosure(expr, env)
        expect_identical(names(tidyselect::eval_select(selection, data)),
                         "region")
        for (verb in list(inspect_grouping, expand_with_margins,
                          summarize_with_margins, nest_with_margins,
                          nest_by_with_margins)) {
          calls <- 0L
          resolutions <- 0L
          actual <- verb(data, .by = !!selection)
          expect_identical(actual, verb(data, .by = region))
          expect_identical(calls, 1L)
          expect_identical(resolutions, 1L)
        }
        for (constructor in c("grouping_set", "grouping_sets", "rollup",
                              "cube", "grouping_spec")) {
          construct <- getExportedValue("marginplyr", constructor)
          spec <- construct(!!selection)
          expected <- construct(region)
          for (verb in list(inspect_grouping, expand_with_margins,
                            summarize_with_margins, nest_with_margins,
                            nest_by_with_margins)) {
            calls <- 0L
            resolutions <- 0L
            expect_identical(verb(data, .grouping = spec),
                             verb(data, .grouping = expected))
            expect_identical(calls, 1L)
            expect_identical(resolutions, 1L)
          }
        }
      }
    }
  }
})

test_that("caller namespace bindings are forced only during selection", {
  data <- data.frame(region = c("east", "west"), amount = 1:2)
  resolutions <- 0L
  binding_reads <- 0L
  typed <- function(...) {
    proxy <- tidyselect::peek_data()
    names(proxy)[vapply(proxy, is.character, logical(1))]
  }
  operator_fn <- function(package, name) {
    resolutions <<- resolutions + 1L
    typed
  }
  for (operator in c("::", ":::")) {
    for (binding in c("active", "promise")) {
      for (nested in c(FALSE, TRUE)) {
        env <- rlang::env(rlang::current_env())
        read_operator <- function() {
          binding_reads <<- binding_reads + 1L
          # This context exists only while tidyselect evaluates the selection.
          expect_named(tidyselect::peek_data(), c("region", "amount"))
          operator_fn
        }
        if (binding == "active") {
          makeActiveBinding(operator, read_operator, env)
        } else {
          delayedAssign(operator, read_operator(), assign.env = env)
        }
        expr <- qualified_selection_spellings(
          operator, "dplyr", "all_of", "amount"
        )[[3L]]
        if (nested) expr <- rlang::call2("all_of", expr)
        selection <- rlang::new_quosure(expr, env)
        resolutions <- 0L
        binding_reads <- 0L
        actual <- inspect_grouping(data, .by = !!selection)
        expect_identical(actual$fixed, "(region)")
        expect_identical(resolutions, 1L)
        expect_identical(binding_reads, 1L)
      }
    }

    env <- rlang::env(rlang::current_env())
    body <- qualified_selection_spellings(
      operator, "dplyr", "all_of", "amount"
    )[[1L]]
    wrapper <- rlang::new_function(
      stats::setNames(list(operator_fn), operator),
      body,
      env
    )
    env$wrapper <- wrapper
    selection <- rlang::new_quosure(quote(all_of(wrapper())), env)
    resolutions <- 0L
    expect_identical(inspect_grouping(data, .by = !!selection)$fixed,
                     "(region)")
    expect_identical(resolutions, 1L)
  }
})

test_that("typed caller-qualified selections preserve their conditions", {
  data <- data.frame(region = c("east", "west"), amount = 1:2)
  calls <- 0L
  warning <- rlang::warning_cnd("selection_warning", message = "typed answer")
  failure <- rlang::error_cnd("selection_error", message = "typed failure")
  typed <- NULL
  operator_fn <- function(package, name) typed
  env <- rlang::env(rlang::current_env())
  for (operator in c("::", ":::")) {
    assign(operator, operator_fn, env)
    for (nested in c(FALSE, TRUE)) {
      expr <- qualified_selection_spellings(
        operator, "dplyr", "all_of", "amount"
      )[[4L]]
      if (nested) expr <- rlang::call2("all_of", expr)
      selection <- rlang::new_quosure(expr, env)
      for (role in c("fixed", "dimension")) {
        consume <- function() {
          if (role == "fixed") {
            inspect_grouping(data, .by = !!selection)
          } else {
            inspect_grouping(data, .grouping = rollup(!!selection))
          }
        }
        typed <- function(...) {
          calls <<- calls + 1L
          warning(warning)
          proxy <- tidyselect::peek_data()
          names(proxy)[vapply(proxy, is.character, logical(1))]
        }
        calls <- 0L
        seen <- list()
        result <- withCallingHandlers(consume(), warning = function(cnd) {
          seen[[length(seen) + 1L]] <<- cnd
          invokeRestart("muffleWarning")
        })
        expected <- if (role == "fixed") {
          inspect_grouping(data, .by = region)
        } else {
          inspect_grouping(data, .grouping = rollup(region))
        }
        expect_identical(result, expected)
        expect_identical(seen, list(warning))
        expect_identical(calls, 1L)
        typed <- function(...) {
          calls <<- calls + 1L
          stop(failure)
        }
        calls <- 0L
        actual <- expect_error(consume(), class = "rlang_error")
        expect_identical(calls, 1L)
        direct <- expect_error(tidyselect::eval_select(selection, data),
                               class = "rlang_error")
        expect_identical(class(actual), class(direct))
        expect_identical(conditionMessage(actual), conditionMessage(direct))
        expect_identical(actual$parent, failure)
      }
    }
  }
})

test_that("an absent namespace operator fails before typed metadata", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(region = c("east", "west"), amount = 1:2)
  calls <- 0L
  caller <- function(x) {
    calls <<- calls + 1L
    x
  }
  source <- dplyr::mutate(dtplyr::lazy_dt(data), derived = caller(region))
  for (operator in c("::", ":::")) {
    selection <- rlang::new_quosure(rlang::call2(
      rlang::call2(operator, quote(tidyselect), quote(all_of)), "region"
    ), rlang::new_environment())
    for (role in c("fixed", "dimension")) {
      actual <- if (role == "fixed") {
        expect_error(inspect_grouping(source, .by = !!selection))
      } else {
        expect_error(inspect_grouping(
          source, .grouping = rollup(!!selection)
        ))
      }
      expect_false(inherits(actual, "marginplyr_error"))
      expect_match(conditionMessage(actual), "could not find function",
                   fixed = TRUE)
      expect_match(conditionMessage(actual), operator, fixed = TRUE)
      expect_identical(calls, 0L)
    }
  }
})
