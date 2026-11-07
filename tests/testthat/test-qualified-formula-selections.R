# A formula-shaped qualified callee is ordinary caller code. Its namespace
# operation and selector both run in the captured selection environment.
qualified_formula_selection <- function(operator, spelling = 1L) {
  state <- rlang::env(calls = 0L, resolutions = 0L, types = character())
  selector <- function(...) {
    state$calls <- state$calls + 1L
    column <- tidyselect::peek_data()$a
    state$types <- c(state$types, typeof(column))
    if (is.character(column)) "a" else "b"
  }
  namespace <- function(package, name) {
    if (identical(substitute(package), quote(base)) &&
          identical(substitute(name), rlang::sym("~"))) {
      state$resolutions <- state$resolutions + 1L
      return(selector)
    }
    package <- as.character(substitute(package))
    name <- as.character(substitute(name))
    if (operator == "::") getExportedValue(package, name) else
      getFromNamespace(name, package)
  }
  env <- rlang::env(rlang::current_env())
  assign(operator, namespace, env)
  head <- rlang::call2(operator, quote(base), rlang::sym("~"))
  expr <- selection_call_spellings(head, quote(a))[[spelling]]
  list(selection = rlang::new_quosure(expr, env), state = state)
}

test_that("qualified formula-shaped fixed keys use typed selection metadata", {
  data <- data.frame(a = c("A", "B"), b = 1L, value = 1:2)
  for (operator in c("::", ":::")) {
    case <- qualified_formula_selection(operator)
    selection <- case$selection
    expect_identical(names(tidyselect::eval_select(selection, data)), "a")
    case$state$calls <- 0L
    case$state$resolutions <- 0L
    case$state$types <- character()
    expect_identical(inspect_grouping(data, .by = !!selection)$fixed, "(a)")
    expect_identical(case$state$types, "character")
    expect_identical(case$state$calls, 1L)
    expect_identical(case$state$resolutions, 1L)
  }
})

# Both public entrances must agree with the literal a selection, including
# the source partitions returned by the summary.
expect_formula_grouping <- function(data) {
  a <- rlang::sym("a")
  b <- rlang::sym("b")
  original <- serialize(data, NULL)
  for (operator in c("::", ":::")) {
    for (spelling in 1:4) {
      for (entrance in c("fixed", "union")) {
        for (consumer in c("inspect", "summarize")) {
          case <- qualified_formula_selection(operator, spelling)
          selection <- case$selection
          if (consumer == "inspect") {
            actual <- if (entrance == "fixed") {
              inspect_grouping(data, .by = !!selection, .format = "list")
            } else {
              inspect_grouping(data,
                               .grouping = grouping_sets(!!selection, !!b),
                               .format = "list")
            }
            expected <- if (entrance == "fixed") {
              inspect_grouping(data, .by = !!a, .format = "list")
            } else {
              inspect_grouping(data, .grouping = grouping_sets(!!a, !!b),
                               .format = "list")
            }
            expect_identical(actual, expected)
          } else {
            actual <- if (entrance == "fixed") {
              summarize_with_margins(data, count = dplyr::n(),
                                     .by = !!selection)
            } else {
              summarize_with_margins(
                data, count = dplyr::n(),
                .grouping = grouping_sets(!!selection, !!b)
              )
            }
            actual <- tibble::as_tibble(actual)
            expect_identical(actual$a, if (entrance == "fixed") {
              c("A", "B")
            } else {
              c("A", "B", "Total")
            })
            expect_identical(actual$count, if (entrance == "fixed") {
              c(1L, 1L)
            } else {
              c(1L, 1L, 2L)
            })
          }
          expect_identical(case$state$types, "character")
          expect_identical(case$state$calls, 1L)
          expect_identical(case$state$resolutions, 1L)
          expect_identical(serialize(data, NULL), original)
        }
      }
    }
  }
}

test_that("qualified formula-shaped selections preserve eager partitions", {
  data <- data.frame(a = c("A", "B"), b = 1L, value = 1:2)
  expect_formula_grouping(data)
  expect_formula_grouping(tibble::as_tibble(data))
})

test_that("qualified formula-shaped selections preserve data.table inputs", {
  skip_if_suggest_absent("data.table")
  data <- data.table::data.table(a = c("A", "B"), b = 1L, value = 1:2)
  expect_formula_grouping(data)
})

test_that("qualified formula-shaped selections preserve immutable dtplyr", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(a = c("A", "B"), b = 1L, value = 1:2)
  expect_formula_grouping(dtplyr::lazy_dt(data, immutable = TRUE))
})

test_that("genuine formula selections retain tidyselect refusals", {
  data <- data.frame(a = c("A", "B"), b = 1L, value = 1:2)
  private <- rlang::call2(
    rlang::call2(":::", quote(base), rlang::sym("~")), quote(a)
  )
  formulas <- list(quote(~a), quote(base::`~`(a)), private)
  for (expr in formulas) {
    direct <- expect_error(tidyselect::eval_select(expr, data),
                           "Formula shorthand|<formula>", class = "rlang_error")
    for (verb in list(inspect_grouping, summarize_with_margins)) {
      fixed <- expect_error(verb(data, .by = !!expr), class = "rlang_error")
      union <- expect_error(verb(data, .grouping = grouping_sets(!!expr, b)),
                            class = "rlang_error")
      for (actual in list(fixed, union)) {
        expect_identical(class(actual), class(direct))
        expect_identical(conditionMessage(actual), conditionMessage(direct))
      }
    }
  }
})

test_that("deferred public formula operators resolve during typed selection", {
  data <- data.frame(a = c("A", "B"), b = 1L)
  for (binding in c("active", "promise")) {
    case <- qualified_formula_selection("::")
    selection <- case$selection
    env <- rlang::quo_get_env(selection)
    operator <- env[["::"]]
    rm(list = "::", envir = env)
    binding_reads <- 0L
    read_operator <- function() {
      binding_reads <<- binding_reads + 1L
      expect_type(tidyselect::peek_data()$a, "character")
      operator
    }
    if (binding == "active") {
      makeActiveBinding("::", read_operator, env)
    } else {
      delayedAssign("::", read_operator(), assign.env = env)
    }
    expect_identical(inspect_grouping(data, .by = !!selection)$fixed, "(a)")
    expect_identical(binding_reads, 1L)
    expect_identical(case$state$resolutions, 1L)
    expect_identical(case$state$calls, 1L)
  }
})

test_that("leading errors precede qualified formula-shaped caller work", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(a = c("A", "B"), b = 1L, value = 1:2)
  reads <- 0L
  derive <- function(x) {
    reads <<- reads + 1L
    paste0(x, "!")
  }
  source <- dplyr::mutate(dtplyr::lazy_dt(data), derived = derive(a))
  for (operator in c("::", ":::")) {
    case <- qualified_formula_selection(operator)
    selection <- case$selection
    for (verb in list(inspect_grouping, summarize_with_margins)) {
      expect_error(verb(source, .by = base::`~`(a)),
                   "<formula>", class = "vctrs_error_subscript_type")
      expect_error(verb(source, .by = !!selection),
                   "Can't safely determine column metadata",
                   class = "marginplyr_error")
      expect_error(verb(source, .grouping = grouping_sets(absent, !!selection)),
                   "absent", class = "vctrs_error_subscript_oob")
      expect_error(verb(source, .grouping = grouping_sets(~a, !!selection)),
                   "Formula shorthand", class = "rlang_error")
      expect_error(
        verb(source, .grouping = grouping_sets(base::`~`(a), !!selection)),
        "<formula>", class = "vctrs_error_subscript_type"
      )
      expect_error(verb(source, .grouping = grouping_sets(a, a, !!selection)),
                   "Duplicate grouping sets", class = "marginplyr_error")
      expect_identical(case$state$calls, 0L)
      expect_identical(case$state$resolutions, 0L)
      expect_identical(reads, 0L)
    }
  }
})
