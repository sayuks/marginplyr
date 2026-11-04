test_that("derived dtplyr inspection matches stringr patterns from names", {
  skip_if_suggest_absent("dtplyr", "stringr")

  data <- data.frame(a = c("x", "y"), b = 1:2)
  step <- dplyr::mutate(dtplyr::lazy_dt(data), added = paste0(a, "x"))
  local <- dplyr::mutate(data, added = paste0(a, "x"))
  pattern <- stringr::regex("^A", ignore_case = TRUE)
  make_pattern <- stringr::regex
  fixed_pattern <- stringr::fixed("ADD", ignore_case = TRUE)
  coll_pattern <- stringr::coll("ADD", ignore_case = TRUE)
  prefix <- "^a"
  ignore_case <- TRUE
  selections <- rlang::quos(
    matches(stringr::regex("^A", ignore_case = TRUE)),
    matches(stringr::regex(.env$prefix, ignore_case = .env[["ignore_case"]])),
    matches(stringr::regex(paste0(.env[["prefix"]], "dded$"))),
    matches(pattern),
    matches(.env$pattern),
    matches(.env[["pattern"]]),
    matches((pattern)),
    matches((stringr::regex("^a"))),
    (matches)(pattern),
    matches((make_pattern)("^a")),
    matches((stringr::regex)("^a")),
    matches((`$`)(.env, pattern)),
    matches((`[[`)(.env, "pattern")),
    matches(make_pattern("^A", ignore_case = TRUE)),
    !matches(stringr::regex("^a")),
    -matches(pattern),
    matches(stringr::fixed("ADD", ignore_case = TRUE)),
    matches(fixed_pattern),
    matches(stringr::coll("ADD", ignore_case = TRUE)),
    matches(coll_pattern),
    matches(stringr::regex(paste0("^", tidyselect::peek_vars()[[1L]])))
  )

  for (selection in selections) {
    expected <- names(tidyselect::eval_select(selection, local))
    fixed <- inspect_grouping(step, .by = !!selection, .format = "list")
    dimensions <- inspect_grouping(
      step, .grouping = grouping_set(!!selection), .format = "list"
    )

    expect_identical(fixed$fixed, list(expected))
    expect_identical(dimensions$included, list(expected))
  }
})

test_that("stringr patterns that read types still require typed metadata", {
  skip_if_suggest_absent("dtplyr", "stringr")

  data <- data.frame(a = "x", b = 1L)
  step <- dplyr::mutate(dtplyr::lazy_dt(data), added = paste0(a, "x"))
  numeric_names <- function() {
    columns <- tidyselect::peek_data()
    names(columns)[vapply(columns, is.numeric, logical(1))]
  }
  regex <- function() stringr::regex(numeric_names())
  selections <- rlang::quos(
    matches(stringr::regex(numeric_names())),
    matches(regex())
  )

  for (selection in selections) {
    expect_error(
      inspect_grouping(step, .by = !!selection),
      "Can't safely determine column metadata",
      class = "marginplyr_error"
    )
  }
})

test_that("computed pattern callees can depend on typed metadata", {
  skip_if_suggest_absent("dtplyr", "stringr")

  data <- data.frame(a = "x", b = 1L)
  step <- dplyr::mutate(dtplyr::lazy_dt(data), added = paste0(a, "x"))
  local <- dplyr::mutate(data, added = paste0(a, "x"))
  pattern_for_data <- function() {
    columns <- tidyselect::peek_data()
    stringr::regex(if (is.character(columns$a)) "^b$" else "^a")
  }
  bindings <- rlang::env(make_regex = stringr::regex, pattern = "^a")
  bindings[["("]] <- function(x) {
    if (identical(substitute(x), quote(matches))) {
      return(function(...) tidyselect::matches(pattern_for_data()))
    }
    if (identical(substitute(x), quote(pattern))) {
      return(as.character(pattern_for_data()))
    }
    function(...) pattern_for_data()
  }
  expressions <- list(
    quote(matches((make_regex)("^a"))),
    quote(matches((stringr::regex)("^a"))),
    quote((matches)(pattern)),
    quote(matches((`$`)(.env, pattern))),
    quote(matches((`[[`)(.env, "pattern"))),
    quote(matches(stringr::regex((pattern))))
  )
  for (expr in expressions) {
    selection <- rlang::new_quosure(expr, bindings)
    expect_identical(names(tidyselect::eval_select(selection, local)), "b")
    expect_identical(inspect_grouping(local, .by = !!selection)$fixed, "(b)")
    expect_error(
      inspect_grouping(step, .by = !!selection),
      "Can't safely determine column metadata", class = "marginplyr_error"
    )
  }
})

test_that("pattern pronouns preserve deferred bindings and caller operators", {
  skip_if_suggest_absent("dtplyr", "stringr")

  data <- data.frame(a = "x", b = 1L)
  step <- dplyr::mutate(dtplyr::lazy_dt(data), added = paste0(a, "x"))
  counter <- new.env(parent = emptyenv())
  counter$calls <- 0L
  deferred <- function() {
    counter$calls <- counter$calls + 1L
    stringr::regex("^a")
  }
  bindings <- rlang::env()
  makeActiveBinding("active", deferred, bindings)
  delayedAssign("delayed", deferred(), assign.env = bindings)
  bindings$classed <- structure("^a", class = "custom_pattern")
  bindings$make_regex <- stringr::regex
  makeActiveBinding("(", deferred, bindings)
  rebound <- rlang::env(pattern = stringr::regex("^a"))
  rebound[["$"]] <- function(...) stop("caller operator ran")
  lexical <- rlang::env(prefix = "^a", .env = bindings)
  bindings$lexical_prefix <- rlang::new_function(
    pairlist(), quote(.env$prefix), lexical
  )
  selections <- list(
    rlang::new_quosure(quote(matches(.env$active)), bindings),
    rlang::new_quosure(quote(matches(.env[["delayed"]])), bindings),
    rlang::new_quosure(quote(matches(.env$pattern)), rebound),
    rlang::new_quosure(quote(matches((make_regex)("^a"))), bindings),
    rlang::new_quosure(quote(matches(stringr::regex(.env$classed))), bindings),
    rlang::new_quosure(
      quote(matches(stringr::regex(lexical_prefix()))), bindings
    )
  )

  for (selection in selections) {
    expect_error(
      inspect_grouping(step, .by = !!selection),
      "Can't safely determine column metadata", class = "marginplyr_error"
    )
  }
  expect_identical(counter$calls, 0L)
})

test_that("malformed stringr regex fails before DuckDB reads metadata", {
  skip_if_suggest_absent("duckdb", "DBI", "stringr")

  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  data <- data.frame(a = "x", b = 1L)
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  step <- dplyr::copy_to(con, data, "pattern_test")

  # Sent queries describe construction, so check the actual DBI method as well.
  counter <- duckdb_query_counter()
  on.exit(counter$untrace(), add = TRUE)
  expect_equal(DBI::dbGetQuery(con, "SELECT 1 AS n")$n, 1L)
  expect_identical(counter$queries, 1L)
  counter$queries <- 0L

  malformed <- "["
  pattern <- stringr::regex("[")
  selections <- rlang::quos(
    matches(stringr::regex("[")),
    matches(stringr::regex(.env$malformed)),
    matches(.env$pattern)
  )
  diagnostic <- "U_REGEX_MISSING_CLOSE_BRACKET"
  for (selection in selections) {
    direct <- expect_error(tidyselect::eval_select(selection, data), diagnostic)
    fixed <- expect_error(inspect_grouping(step, .by = !!selection), diagnostic)
    dimension <- expect_error(inspect_grouping(
      step, .grouping = grouping_set(!!selection)
    ), diagnostic)
    for (actual in list(fixed, dimension)) {
      expect_identical(class(actual), class(direct))
      expect_identical(class(actual$parent), class(direct$parent))
      expect_identical(
        conditionMessage(actual$parent), conditionMessage(direct$parent)
      )
    }
    expect_identical(nrow(last_sent_queries(include_result = FALSE)), 0L)
    expect_identical(counter$queries, 0L)
  }
})
