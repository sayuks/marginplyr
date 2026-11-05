test_that("direct context lookups need only the derived column names", {
  skip_if_suggest_absent("dtplyr")
  data <- tibble::tibble(a = c("x", "y"), b = 1:2)
  evaluations <- 0L
  derive <- function(x) {
    evaluations <<- evaluations + 1L
    paste0(x, "!")
  }
  step <- dtplyr::lazy_dt(data) |>
    dplyr::mutate(added = derive(a))
  typed <- dplyr::mutate(data, added = paste0(a, "!"))
  data_alias <- tidyselect::peek_data
  names_alias <- base::names
  names <- "nonfunction shadow"
  idx <- c(3L, 1L)
  selectors <- rlang::exprs(
    tidyselect::peek_vars(),
    names(tidyselect::peek_data()),
    tidyselect::peek_vars()[c(3L, 1L)],
    tidyselect::peek_vars()[.env$idx],
    tidyselect::peek_vars()[[.env[["idx"]][[1L]]]],
    base::`[`(x = tidyselect::peek_vars(), i = c(3L, 1L)),
    names_alias(data_alias())[[2L]]
  )
  constructors <- list(grouping_set, grouping_sets, rollup, cube, grouping_spec)

  for (selector in selectors) {
    selected <- names(tidyselect::eval_select(selector, typed))
    wrapped <- rlang::expr(tidyselect::all_of(!!selector))
    for (selection in list(selector, wrapped)) {
      expect_identical(
        inspect_grouping(step, .by = !!selection, .format = "list")$fixed,
        list(selected)
      )
      for (constructor in constructors) {
        expect_identical(
          inspect_grouping(step, .grouping = constructor(!!selection)),
          inspect_grouping(
            typed, .grouping = constructor(tidyselect::all_of(selected))
          )
        )
      }
    }
  }
  expect_identical(evaluations, 0L)
})

test_that("NULL and direct lexical selectors need only the derived names", {
  skip_if_suggest_absent("dtplyr")
  data <- tibble::tibble(a = c("x", "y"), b = 1:2)
  evaluations <- 0L
  derive <- function(x) {
    evaluations <<- evaluations + 1L
    paste0(x, "!")
  }
  step <- dtplyr::lazy_dt(data) |>
    dplyr::mutate(added = derive(a))
  pick <- "a"
  empty <- NULL
  selectors <- rlang::exprs(
    c(a, NULL), c(a, all_of(NULL)), .env$pick, .env[["pick"]],
    c(a, .env$empty), c(a, all_of(.env[["empty"]]))
  )

  for (selector in selectors) {
    expect_identical(
      inspect_grouping(step, .by = !!selector, .format = "list")$fixed,
      list("a")
    )
    expect_identical(
      inspect_grouping(
        step, .grouping = grouping_set(!!selector), .format = "list"
      )$included,
      list("a")
    )
  }
  for (selector in rlang::exprs(NULL, all_of(NULL), .env$empty)) {
    expect_identical(
      inspect_grouping(step, .by = !!selector, .format = "list")$fixed,
      list(character())
    )
    expect_identical(
      inspect_grouping(
        step, .grouping = grouping_set(!!selector), .format = "list"
      )$included,
      list(character())
    )
  }
  # jarl-ignore missing_argument: The empty constructor argument is under test.
  expect_error(
    inspect_grouping(step, .grouping = grouping_set(, a)),
    "Argument 1", class = "marginplyr_error"
  )
  expect_identical(evaluations, 0L)
})

test_that("missing selection-context names fail before a DuckDB query", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  data <- tibble::tibble(a = "x", b = 1L)
  remote <- dplyr::copy_to(con, data, "selection_context_names")
  # Trace the DBI method to observe query sends before the condition.
  counter <- duckdb_query_counter()
  on.exit(counter$untrace(), add = TRUE, after = FALSE)
  expect_identical(DBI::dbGetQuery(con, "SELECT 1 AS n")$n, 1L)
  expect_identical(counter$queries, 1L)

  names <- "nonfunction shadow"
  idx <- 99L
  pick <- "absent"

  selectors <- rlang::exprs(
    tidyselect::peek_vars()[99L],
    tidyselect::all_of(tidyselect::peek_vars()[99L]),
    tidyselect::peek_vars()[.env$idx],
    tidyselect::all_of(tidyselect::peek_vars()[.env[["idx"]]]),
    names(tidyselect::peek_data())[99L],
    tidyselect::peek_vars()[.env$absent_idx],
    tidyselect::all_of(tidyselect::peek_vars()[.env[["absent_idx"]]]),
    c(NULL, a, absent),
    c(all_of(NULL), a, absent),
    .env$pick,
    .env[["pick"]],
    .env$absent_idx
  )
  for (selector in selectors) {
    counter$queries <- 0L
    expected <- expect_error(tidyselect::eval_select(selector, data))
    fixed <- expect_error(inspect_grouping(remote, .by = !!selector))
    expect_identical(counter$queries, 0L)
    dimension <- expect_error(
      inspect_grouping(remote, .grouping = grouping_set(!!selector))
    )
    for (error in list(fixed, dimension)) {
      expect_identical(class(error), class(expected))
      expect_identical(conditionMessage(error), conditionMessage(expected))
    }
    expect_identical(counter$queries, 0L)
  }
})

test_that("typed and opaque selections retain the dtplyr metadata boundary", {
  skip_if_suggest_absent("dtplyr")
  data <- tibble::tibble(a = c("x", "y"), b = 1:2)
  calls <- new.env()
  calls$upstream <- 0L
  derive <- function(x) {
    calls$upstream <- calls$upstream + 1L
    paste0(x, "!")
  }
  step <- dtplyr::lazy_dt(data) |>
    dplyr::mutate(added = derive(a))

  calls$selector <- 0L
  read_index <- function() {
    calls$selector <- calls$selector + 1L
    1L
  }
  makeActiveBinding("active_idx", read_index, rlang::current_env())
  delayedAssign("delayed_idx", read_index())
  classed_idx <- factor(1L)
  predicate <- is.character
  pick <- "a"
  key <- "pick"
  names <- function(x) {
    calls$selector <- calls$selector + 1L
    x
  }
  where <- tidyselect::peek_vars
  columns <- c("a", "b", "added")
  selectors <- rlang::exprs(
    tidyselect::where(is.character),
    where(),
    base::names(tidyselect::peek_data())[
      vapply(tidyselect::peek_data(), is.character, logical(1))
    ],
    columns[if (is.numeric(tidyselect::peek_data()$b)) 2L else 1L],
    tidyselect::peek_vars()[read_index()],
    tidyselect::peek_vars()[.env$active_idx],
    tidyselect::all_of(tidyselect::peek_vars()[.env$active_idx]),
    tidyselect::peek_vars()[.env[["delayed_idx"]]],
    tidyselect::all_of(tidyselect::peek_vars()[.env[["delayed_idx"]]]),
    tidyselect::peek_vars()[.env$classed_idx],
    .env$active_idx,
    .env[["delayed_idx"]],
    .env$classed_idx,
    .env[[key]],
    .env$predicate,
    names(tidyselect::peek_data())
  )
  check <- function(selector) {
    expect_error(
      inspect_grouping(step, .by = !!selector),
      "Can't safely determine column metadata",
      class = "marginplyr_error"
    )
    expect_error(
      inspect_grouping(step, .grouping = grouping_set(!!selector)),
      "Can't safely determine column metadata",
      class = "marginplyr_error"
    )
  }
  for (selector in selectors) {
    check(selector)
  }
  check(local({
    names <- "nonfunction shadow"
    rlang::quo((names)(tidyselect::peek_data()))
  }))
  check(local({
    `[[` <- function(x, i) {
      calls$selector <- calls$selector + 1L
      pick
    }
    rlang::quo(.env[["pick"]])
  }))
  # `everything` is supplied by tidyselect's mask, not the lexical chain.
  check(rlang::new_quosure(quote(all_of(everything)), baseenv()))
  check_dots <- function(...) {
    check(rlang::quo(tidyselect::peek_vars()[..1]))
    check(rlang::quo(tidyselect::peek_vars()[.env$..1]))
    check(rlang::quo(
      tidyselect::all_of(tidyselect::peek_vars()[.env[["..1"]]])
    ))
  }
  check_missing <- function(idx) {
    check(rlang::quo(tidyselect::peek_vars()[.env$idx]))
    check(rlang::quo(.env$idx))
  }
  check_dots(read_index())
  check_missing()
  expect_identical(calls$selector, 0L)
  expect_identical(calls$upstream, 0L)
})

test_that("opaque namespace promises stay unforced without typed metadata", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(a = "x", b = 1L)
  step <- dplyr::mutate(dtplyr::lazy_dt(data), added = paste0(a, "!"))

  # Give a loaded namespace a deferred export to observe when it is forced.
  ns <- asNamespace("testthat")
  name <- "auto_test_package"
  original <- testthat::auto_test_package
  unlockBinding(name, ns)
  on.exit({
    unlockBinding(name, ns)
    assign(name, original, envir = ns)
    lockBinding(name, ns)
  })
  reads <- 0L
  delayedAssign(name, {
    reads <- reads + 1L
    function() 1L
  }, assign.env = ns)
  lockBinding(name, ns)

  selectors <- rlang::exprs(
    testthat::auto_test_package(),
    tidyselect::peek_vars()[testthat::auto_test_package()],
    tidyselect::all_of(tidyselect::peek_vars()[testthat::auto_test_package()])
  )
  for (selector in selectors) {
    expect_error(
      inspect_grouping(step, .by = !!selector),
      "Can't safely determine column metadata", class = "marginplyr_error"
    )
    expect_error(
      inspect_grouping(step, .grouping = grouping_set(!!selector)),
      "Can't safely determine column metadata", class = "marginplyr_error"
    )
    expect_identical(reads, 0L)
  }
  expect_identical(
    inspect_grouping(data, .by = testthat::auto_test_package())$fixed,
    "(a)"
  )
  expect_identical(reads, 1L)
})

test_that("embraced names select fixed keys and Grouping dimensions", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(a = "x", b = 1L)
  step <- dplyr::mutate(dtplyr::lazy_dt(data), added = paste0(a, "!"))

  fixed <- function(col, pick = "b") {
    inspect_grouping(step, .by = c({{ col }}, b), .format = "list")$fixed
  }
  dimensions <- function(col, pick = "b") {
    inspect_grouping(
      step, .grouping = grouping_set(c({{ col }}, b)), .format = "list"
    )$included
  }

  expect_identical(
    fixed(a), inspect_grouping(step, .by = c(a, b), .format = "list")$fixed
  )
  expect_identical(
    dimensions(a),
    inspect_grouping(
      step, .grouping = grouping_set(c(a, b)), .format = "list"
    )$included
  )
  pick <- "a"
  expect_identical(fixed(.env$pick), list(c("a", "b")))
  expect_identical(dimensions(.env$pick), list(c("a", "b")))
})

test_that("an absent embraced column fails before DuckDB reads metadata", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  data <- data.frame(a = "x", b = 1L)
  remote <- dplyr::copy_to(con, data, "embraced_grouping_names")
  counter <- duckdb_query_counter()
  on.exit(counter$untrace(), add = TRUE, after = FALSE)
  expect_identical(DBI::dbGetQuery(con, "SELECT 1 AS n")$n, 1L)
  expect_identical(counter$queries, 1L)
  counter$queries <- 0L

  fixed <- function(col) inspect_grouping(remote, .by = c({{ col }}, b))
  dimensions <- function(col) {
    inspect_grouping(remote, .grouping = grouping_set(c({{ col }}, b)))
  }
  for (inspect in list(fixed, dimensions)) {
    expect_error(
      inspect(absent), "`absent` doesn't exist", fixed = TRUE,
      class = "vctrs_error_subscript_oob"
    )
    expect_identical(counter$queries, 0L)
  }
})

test_that("embraced predicates still require typed metadata", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(a = "x", b = 1L)
  step <- dplyr::mutate(dtplyr::lazy_dt(data), added = paste0(a, "!"))
  fixed <- function(input, col) {
    inspect_grouping(input, .by = c({{ col }}, b), .format = "list")$fixed
  }
  dimensions <- function(input, col) {
    inspect_grouping(
      input, .grouping = grouping_set(c({{ col }}, b)), .format = "list"
    )$included
  }
  for (inspect in list(fixed, dimensions)) {
    expect_identical(
      inspect(data, tidyselect::where(is.character)), list(c("a", "b"))
    )
    expect_error(
      inspect(step, tidyselect::where(is.character)),
      "Can't safely determine column metadata", class = "marginplyr_error"
    )
  }
})
