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
  selectors <- rlang::exprs(
    tidyselect::peek_vars(),
    names(tidyselect::peek_data()),
    tidyselect::peek_vars()[c(3L, 1L)],
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

test_that("missing selection-context names fail before a DuckDB query", {
  skip_if_suggest_absent("duckdb")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  data <- tibble::tibble(a = "x", b = 1L)
  remote <- dplyr::copy_to(con, data, "selection_context_names")
  counter <- duckdb_query_counter()
  on.exit(counter$untrace(), add = TRUE, after = FALSE)

  selectors <- rlang::exprs(
    tidyselect::peek_vars()[99L],
    tidyselect::all_of(tidyselect::peek_vars()[99L])
  )
  for (selector in selectors) {
    expected <- expect_error(tidyselect::eval_select(selector, data))
    fixed <- expect_error(inspect_grouping(remote, .by = !!selector))
    dimension <- expect_error(
      inspect_grouping(remote, .grouping = grouping_set(!!selector))
    )
    for (error in list(fixed, dimension)) {
      expect_identical(class(error), class(expected))
      expect_identical(conditionMessage(error), conditionMessage(expected))
    }
  }
  expect_identical(counter$queries, 0L)
})

test_that("typed and opaque selections retain the dtplyr metadata boundary", {
  skip_if_suggest_absent("dtplyr")
  data <- tibble::tibble(a = c("x", "y"), b = 1:2)
  evaluations <- 0L
  derive <- function(x) {
    evaluations <<- evaluations + 1L
    paste0(x, "!")
  }
  step <- dtplyr::lazy_dt(data) |>
    dplyr::mutate(added = derive(a))

  selector_calls <- 0L
  choose <- function() {
    selector_calls <<- selector_calls + 1L
    1L
  }
  names <- function(x) {
    selector_calls <<- selector_calls + 1L
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
    tidyselect::peek_vars()[choose()],
    names(tidyselect::peek_data())
  )
  for (selector in selectors) {
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
  expect_identical(selector_calls, 0L)
  expect_identical(evaluations, 0L)
})
