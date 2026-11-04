test_that("derived dtplyr inspection resolves lexical helper arguments", {
  skip_if_suggest_absent("dtplyr")

  data <- data.frame(a = c("x", "y"), b = 1:2, cols = c("z", "w"))
  step <- dplyr::mutate(dtplyr::lazy_dt(data), added = paste0(a, "x"))
  cols <- c("added", "a")
  vars <- dplyr::tbl_vars(step)
  proxy <- stats::setNames(seq_along(vars), vars)
  selections <- rlang::quos(
    all_of(.env$cols),
    tidyselect::all_of(.env[["cols"]]),
    any_of(c(.env$cols, "absent"))
  )

  for (selection in selections) {
    expected <- names(tidyselect::eval_select(selection, proxy))
    fixed <- inspect_grouping(step, .by = !!selection, .format = "list")
    dimensions <- inspect_grouping(
      step, .grouping = grouping_set(!!selection), .format = "list"
    )

    expect_identical(fixed$fixed, list(expected))
    expect_identical(dimensions$included, list(expected))
  }
})

test_that("missing lexical columns fail before DuckDB reads metadata", {
  skip_if_suggest_absent("duckdb", "DBI")

  data <- data.frame(a = "x", b = 1L)
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  input <- dplyr::copy_to(con, data, "grouping_env_pronoun")
  step <- dplyr::mutate(input, added = paste0(a, "x"))
  # Trace the DBI method to observe query sends before the condition.
  counter <- duckdb_query_counter()
  on.exit(counter$untrace(), add = TRUE)
  expect_equal(DBI::dbGetQuery(con, "SELECT 1 AS n")$n, 1L)
  expect_identical(counter$queries, 1L)
  counter$queries <- 0L

  cols <- c("added", "absent")
  vars <- dplyr::tbl_vars(step)
  proxy <- stats::setNames(seq_along(vars), vars)
  selections <- rlang::quos(all_of(.env$cols), all_of(.env[["cols"]]))
  for (selection in selections) {
    direct <- expect_error(tidyselect::eval_select(selection, proxy), "absent")
    fixed <- expect_error(inspect_grouping(step, .by = !!selection), "absent")
    dimension <- expect_error(inspect_grouping(
      step, .grouping = grouping_set(!!selection)
    ), "absent")
    for (actual in list(fixed, dimension)) {
      expect_identical(class(actual), class(direct))
      expect_identical(class(actual$parent), class(direct$parent))
      expect_identical(
        conditionMessage(actual$parent), conditionMessage(direct$parent)
      )
    }
    expect_identical(counter$queries, 0L)
  }
})

test_that("helper pronouns defer computed and type-dependent arguments", {
  skip_if_suggest_absent("dtplyr")

  data <- data.frame(a = "x", b = 1L)
  step <- dplyr::mutate(dtplyr::lazy_dt(data), added = paste0(a, "x"))
  cols <- c("added", "a")
  reads <- 0L
  subscript <- function() {
    reads <<- reads + 1L
    "cols"
  }
  makeActiveBinding("active_cols", subscript, rlang::current_env())
  delayedAssign("delayed_cols", subscript())
  lexical_names <- local({
    .env <- new.env(parent = emptyenv())
    makeActiveBinding("cols", subscript, .env)
    function() .env$cols
  })
  numeric_names <- function() {
    columns <- tidyselect::peek_data()
    names(columns)[vapply(columns, is.numeric, logical(1))]
  }
  selections <- rlang::quos(
    all_of(.env$active_cols),
    all_of(.env[["delayed_cols"]]),
    all_of(.env[[subscript()]]),
    all_of(lexical_names()),
    all_of(c(.env$cols, numeric_names()))
  )

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
  expect_identical(reads, 0L)
})
