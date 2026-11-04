test_that("literal dots select fixed keys on a derived dtplyr step", {
  skip_if_suggest_absent("dtplyr")

  data <- data.frame(a = "x", b = 1L)
  step <- dplyr::mutate(dtplyr::lazy_dt(data), added = paste0(a, "x"))
  walk <- function(...) {
    inspect_grouping(step, .by = c(...), .format = "list")$fixed
  }
  helper <- function(...) {
    inspect_grouping(
      step, .by = tidyselect::all_of(c(...)), .format = "list"
    )$fixed
  }

  for (select in list(walk, helper)) {
    expect_identical(select(), list(character()))
    expect_identical(select("b", "a"), list(c("b", "a")))
  }
})

test_that("literal dots select Grouping dimensions through nested wrappers", {
  skip_if_suggest_absent("dtplyr")

  data <- data.frame(a = "x", b = 1L)
  step <- dplyr::mutate(dtplyr::lazy_dt(data), added = paste0(a, "x"))
  walk <- function(...) {
    inspect <- function() {
      inspect_grouping(
        step, .grouping = grouping_set(c(...)), .format = "list"
      )$included
    }
    inspect()
  }
  helper <- function(...) {
    inspect <- function() {
      inspect_grouping(
        step, .grouping = grouping_set(tidyselect::all_of(c(...))),
        .format = "list"
      )$included
    }
    inspect()
  }

  for (select in list(walk, helper)) {
    expect_identical(select(), list(character()))
    expect_identical(select("b", "a"), list(c("b", "a")))
  }
})

test_that("absent names forwarded in dots fail before DuckDB reads metadata", {
  skip_if_suggest_absent("duckdb", "DBI")

  data <- data.frame(a = "x", b = 1L)
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  remote <- dplyr::copy_to(con, data, "forwarded_grouping_names")
  counter <- duckdb_query_counter()
  on.exit(counter$untrace(), add = TRUE)
  expect_identical(DBI::dbGetQuery(con, "SELECT 1 AS n")$n, 1L)
  expect_identical(counter$queries, 1L)
  counter$queries <- 0L

  check <- function(...) {
    selections <- rlang::quos(c(...), tidyselect::all_of(c(...)))
    for (selection in selections) {
      expect_error(
        inspect_grouping(remote, .by = !!selection),
        "`absent` doesn't exist", fixed = TRUE
      )
      expect_error(
        inspect_grouping(remote, .grouping = grouping_set(!!selection)),
        "`absent` doesn't exist", fixed = TRUE
      )
      expect_identical(counter$queries, 0L)
    }
  }
  check("absent")
})

test_that("opaque dots stay unforced when dtplyr metadata is unavailable", {
  skip_if_suggest_absent("dtplyr")

  data <- data.frame(a = "x", b = 1L)
  step <- dplyr::mutate(dtplyr::lazy_dt(data), added = paste0(a, "x"))
  inspect <- function(...) {
    expect_error(
      inspect_grouping(step, .by = c(...)),
      "Can't safely determine column metadata", class = "marginplyr_error"
    )
    expect_error(
      inspect_grouping(
        step, .grouping = grouping_set(tidyselect::all_of(c(...)))
      ),
      "Can't safely determine column metadata", class = "marginplyr_error"
    )
  }

  reads <- 0L
  predicate <- function(x) {
    reads <<- reads + 1L
    is.character(x)
  }
  inspect(tidyselect::where(predicate))
  inspect({
    reads <- reads + 1L
    "b"
  })
  expect_identical(reads, 0L)
})

test_that("dots capture respects ordinary and active bindings", {
  skip_if_suggest_absent("dtplyr")

  data <- data.frame(a = "x", b = 1L)
  step <- dplyr::mutate(dtplyr::lazy_dt(data), added = paste0(a, "x"))
  reads <- 0L
  active <- function() {
    reads <<- reads + 1L
    "b"
  }
  inspect <- function(...) {
    makeActiveBinding("list", active, environment())
    inspect_grouping(step, .by = c(...), .format = "list")$fixed
  }
  expect_identical(inspect("b"), list("b"))

  unbound <- rlang::new_quosure(quote(c(...)), baseenv())
  expect_error(
    inspect_grouping(step, .by = !!unbound),
    "'...' used in an incorrect context", fixed = TRUE
  )
  active_dots <- rlang::env()
  makeActiveBinding("...", active, active_dots)
  for (bindings in list(active_dots, rlang::env("..." = "b"))) {
    selection <- rlang::new_quosure(quote(c(...)), bindings)
    expect_error(
      inspect_grouping(step, .by = !!selection),
      "Can't safely determine column metadata", class = "marginplyr_error"
    )
  }
  expect_identical(reads, 0L)
})
