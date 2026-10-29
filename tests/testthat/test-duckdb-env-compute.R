# Each case exposes .env through a public expansion role, with literal row data.
duckdb_env_compute_case <- function(con, role, empty) {
  source <- data.frame(g = c("a", "b"), v = c(1L, 2L))
  if (!identical(role, "identifier")) {
    source[[".env"]] <- c("x", "y")
  }
  if (empty) {
    source <- source[FALSE, , drop = FALSE]
  }
  input <- dplyr::copy_to(con, source, overwrite = TRUE)
  query <- switch(role,
    payload = expand_with_margins(
      input, .grouping = rollup("g"), .margin_label = NULL, .id = "sid"
    ),
    dimension = expand_with_margins(
      input, .grouping = rollup(tidyselect::all_of(".env")),
      .margin_label = NULL, .id = "sid"
    ),
    fixed = expand_with_margins(
      input, .by = tidyselect::all_of(".env"), .grouping = rollup("g"),
      .margin_label = NULL, .id = "sid"
    ),
    identifier = expand_with_margins(
      input, .grouping = rollup("g"), .margin_label = NULL, .id = ".env"
    )
  )
  expected <- switch(role,
    payload = data.frame(
      g = c("a", "b", NA, NA), sid = c(1L, 1L, 2L, 2L),
      v = c(1L, 2L, 1L, 2L), .env = c("x", "y", "x", "y")
    ),
    dimension = data.frame(
      .env = c("x", "y", NA, NA), sid = c(1L, 1L, 2L, 2L),
      g = c("a", "b", "a", "b"), v = c(1L, 2L, 1L, 2L)
    ),
    fixed = data.frame(
      .env = c("x", "y", "x", "y"), g = c("a", "b", NA, NA),
      sid = c(1L, 1L, 2L, 2L), v = c(1L, 2L, 1L, 2L)
    ),
    identifier = data.frame(
      g = c("a", "b", NA, NA), .env = c(1L, 1L, 2L, 2L),
      v = c(1L, 2L, 1L, 2L)
    )
  )
  if (empty) {
    expected <- expected[FALSE, , drop = FALSE]
  }
  list(query = query, expected = expected)
}

test_that("invalid DuckDB expansion indexes leave destinations untouched", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  for (role in c("payload", "dimension", "fixed", "identifier")) {
    for (empty in c(FALSE, TRUE)) {
      case <- duckdb_env_compute_case(con, role, empty)
      for (temporary in c(FALSE, TRUE)) {
        for (existing in c(FALSE, TRUE)) {
          for (arg in c("indexes", "unique_indexes")) {
            name <- paste("invalid", role, empty, temporary, existing, arg,
                          sep = "_")
            sentinel <- data.frame(sentinel = 777L, label = "untouched")
            if (existing) {
              dplyr::copy_to(con, sentinel, name, temporary = temporary)
              schema_sql <- paste("DESCRIBE", DBI::dbQuoteIdentifier(con, name))
              schema <- DBI::dbGetQuery(con, schema_sql)
            }
            args <- list(
              x = case$query, name = name, temporary = temporary,
              overwrite = existing, analyze = FALSE
            )
            args[[arg]] <- list(c("v", "absent_column"))
            expect_error(do.call(dplyr::compute, args), "absent_column",
                         info = name)
            expect_identical(DBI::dbExistsTable(con, name), existing,
                             info = name)
            if (existing) {
              actual <- dplyr::collect(dplyr::tbl(con, name))
              expect_identical(as.data.frame(actual), sentinel, info = name)
              expect_identical(DBI::dbGetQuery(con, schema_sql), schema,
                               info = name)
              DBI::dbRemoveTable(con, name)
            }
          }
        }
      }
    }
  }
})

# Compare public rows independently of a destination's physical row order.
expect_duckdb_compute_rows <- function(query, expected) {
  actual <- as.data.frame(dplyr::collect(query))
  actual <- actual[vctrs::vec_order(actual), , drop = FALSE]
  expected <- expected[vctrs::vec_order(expected), , drop = FALSE]
  rownames(actual) <- NULL
  rownames(expected) <- NULL
  expect_identical(actual, expected)
}

test_that("valid DuckDB expansion indexes retain public values and types", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  for (role in c("payload", "dimension", "fixed", "identifier")) {
    for (empty in c(FALSE, TRUE)) {
      case <- duckdb_env_compute_case(con, role, empty)
      for (temporary in c(FALSE, TRUE)) {
        for (arg in c("indexes", "unique_indexes")) {
          name <- paste("valid", role, empty, temporary, arg, sep = "_")
          args <- list(x = case$query, name = name, temporary = temporary)
          args[[arg]] <- list(c("g", ".env", "v"))
          computed <- do.call(dplyr::compute, args)
          expect_duckdb_compute_rows(computed, case$expected)
          index <- DBI::dbGetQuery(
            con,
            paste("SELECT is_unique FROM duckdb_indexes()",
                  "WHERE table_name = ?"),
            params = list(name)
          )
          expect_identical(index$is_unique, identical(arg, "unique_indexes"))
        }
      }
    }
  }
})

test_that("computed DuckDB expansions retain grouping and window order", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  for (role in c("payload", "dimension", "fixed", "identifier")) {
    for (empty in c(FALSE, TRUE)) {
      case <- duckdb_env_compute_case(con, role, empty)
      query <- case$query |>
        dplyr::rename(group_key = g) |>
        dplyr::filter(.data$v > 0L) |>
        dplyr::group_by(.data$group_key)
      control <- dbplyr::window_order(query, dplyr::desc(v))
      query <- dplyr::arrange(query, dplyr::desc(.data$v))
      expected <- case$expected
      names(expected)[names(expected) == "g"] <- "group_key"
      totals <- if (identical(role, "dimension")) {
        data.frame(group_key = c("a", "b"), total = c(2, 4))
      } else {
        data.frame(group_key = c("a", "b", NA), total = c(1, 2, 3))
      }
      if (empty) {
        totals <- totals[FALSE, , drop = FALSE]
      }
      windowed <- expected
      windowed$first_value <- as.integer(ifelse(
        is.na(expected$group_key), 2L, expected$v
      ))
      expect_duckdb_compute_rows(
        dplyr::summarise(control, total = sum(.data$v), .groups = "drop"),
        totals
      )
      expect_duckdb_compute_rows(
        dplyr::mutate(control, first_value = dplyr::first(.data$v)), windowed
      )
      for (temporary in c(FALSE, TRUE)) {
        name <- paste("metadata", role, empty, temporary, sep = "_")
        computed <- dplyr::compute(query, name, temporary = temporary)
        expect_identical(dplyr::group_vars(computed), "group_key")
        expect_identical(dbplyr::op_sort(computed), dbplyr::op_sort(query))
        expect_duckdb_compute_rows(computed, expected)
        expect_duckdb_compute_rows(
          dplyr::summarise(computed, total = sum(.data$v), .groups = "drop"),
          totals
        )
        expect_duckdb_compute_rows(
          dplyr::mutate(computed, first_value = dplyr::first(.data$v)), windowed
        )
      }
    }
  }
})

test_that("ordinary DuckDB expansions retain lazy semantics after compute", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  input <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), v = c(1L, 2L), payload = c("x", "y"))
  )
  query <- expand_with_margins(
    input, .grouping = rollup("g"), .margin_label = NULL
  ) |>
    dplyr::group_by(.data$g) |>
    dplyr::arrange(dplyr::desc(.data$v))
  for (temporary in c(FALSE, TRUE)) {
    computed <- dplyr::compute(
      query, name = paste0("ordinary_metadata_", temporary),
      temporary = temporary
    )
    expect_identical(dplyr::group_vars(computed), "g")
    expect_identical(dbplyr::op_sort(computed), dbplyr::op_sort(query))
    expect_duckdb_compute_rows(
      dplyr::summarise(computed, total = sum(.data$v), .groups = "drop"),
      data.frame(g = c("a", "b", NA), total = c(1, 2, 3))
    )
    expect_duckdb_compute_rows(
      dplyr::mutate(computed, first_value = dplyr::first(.data$v)),
      data.frame(
        g = c("a", "b", NA, NA), v = c(1L, 2L, 1L, 2L),
        payload = c("x", "y", "x", "y"), first_value = c(1L, 2L, 2L, 2L)
      )
    )
  }
})

test_that("direct DuckDB Margin order adds no window order when computed", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  input <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), v = c(1L, 2L), .env = c("x", "y"))
  )
  for (sort in c("none", "first", "last")) {
    query <- expand_with_margins(
      input, .grouping = rollup("g"), .margin_label = NULL, .sort = sort
    )
    expect_length(dbplyr::op_sort(query), 0L)
    for (temporary in c(FALSE, TRUE)) {
      computed <- dplyr::compute(
        query, name = paste("direct_order", sort, temporary, sep = "_"),
        temporary = temporary, indexes = NULL, unique_indexes = NULL
      )
      expect_length(dplyr::group_vars(computed), 0L)
      expect_length(dbplyr::op_sort(computed), 0L)
      if (!identical(sort, "none")) {
        expect_identical(dplyr::collect(computed), dplyr::collect(query))
      }
    }
  }
})

test_that("DuckDB expansion index validation accepts dbplyr argument forms", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  case <- duckdb_env_compute_case(con, "payload", FALSE)
  for (arg in c("indexes", "unique_indexes")) {
    args <- list(x = case$query)
    args[[arg]] <- 1L
    expect_error(do.call(dplyr::compute, args), "character vector",
                 class = "marginplyr_error")
    args[[arg]] <- "absent_column"
    expect_error(do.call(dplyr::compute, args), "absent_column",
                 class = "marginplyr_error")
    args[[arg]] <- "g"
    expect_duckdb_compute_rows(do.call(dplyr::compute, args), case$expected)
  }
})
