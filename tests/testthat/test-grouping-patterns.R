test_that("derived dtplyr inspection matches stringr patterns from names", {
  skip_if_suggest_absent("dtplyr", "stringr")

  data <- data.frame(a = c("x", "y"), b = 1:2)
  step <- dplyr::mutate(dtplyr::lazy_dt(data), added = paste0(a, "x"))
  local <- dplyr::mutate(data, added = paste0(a, "x"))
  pattern <- stringr::regex("^A", ignore_case = TRUE)
  make_pattern <- stringr::regex
  fixed_pattern <- stringr::fixed("ADD", ignore_case = TRUE)
  coll_pattern <- stringr::coll("ADD", ignore_case = TRUE)
  selections <- rlang::quos(
    matches(stringr::regex("^A", ignore_case = TRUE)),
    matches(pattern),
    matches(.env$pattern),
    matches(.env[["pattern"]]),
    matches((pattern)),
    matches((stringr::regex("^a"))),
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

test_that("pattern pronouns preserve deferred bindings and caller operators", {
  skip_if_suggest_absent("dtplyr", "stringr")

  data <- data.frame(a = "x", b = 1L)
  step <- dplyr::mutate(dtplyr::lazy_dt(data), added = paste0(a, "x"))
  calls <- 0L
  deferred <- function() {
    calls <<- calls + 1L
    stringr::regex("^a")
  }
  bindings <- rlang::env()
  makeActiveBinding("active", deferred, bindings)
  delayedAssign("delayed", deferred(), assign.env = bindings)
  rebound <- rlang::env(pattern = stringr::regex("^a"))
  rebound[["$"]] <- function(...) stop("caller operator ran")
  selections <- list(
    rlang::new_quosure(quote(matches(.env$active)), bindings),
    rlang::new_quosure(quote(matches(.env[["delayed"]])), bindings),
    rlang::new_quosure(quote(matches(.env$pattern)), rebound)
  )

  for (selection in selections) {
    expect_error(
      inspect_grouping(step, .by = !!selection),
      "Can't safely determine column metadata", class = "marginplyr_error"
    )
  }
  expect_identical(calls, 0L)
})

test_that("malformed stringr regex fails before DuckDB reads metadata", {
  skip_if_suggest_absent("duckdb", "DBI", "stringr")

  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  step <- dplyr::copy_to(con, data.frame(a = "x", b = 1L), "pattern_test")

  sends <- 0L
  signature <- c("duckdb_connection", "character")
  suppressMessages(trace(
    "dbSendQuery", signature = signature, where = asNamespace("DBI"),
    tracer = function() {
      sends <<- sends + 1L
    },
    print = FALSE
  ))
  on.exit(suppressMessages(untrace(
    "dbSendQuery", signature = signature, where = asNamespace("DBI")
  )), add = TRUE)

  expect_error(
    inspect_grouping(step, .by = matches(stringr::regex("["))),
    "U_REGEX_MISSING_CLOSE_BRACKET"
  )
  expect_identical(nrow(last_sent_queries(include_result = FALSE)), 0L)

  pattern <- stringr::regex("[")
  expect_error(
    inspect_grouping(step, .grouping = grouping_set(matches(.env$pattern))),
    "U_REGEX_MISSING_CLOSE_BRACKET"
  )
  expect_identical(nrow(last_sent_queries(include_result = FALSE)), 0L)
  expect_identical(sends, 0L)
})
