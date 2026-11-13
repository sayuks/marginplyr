# A Margin summary over an arranged SQL input (#1596). Its rows do not depend
# on the input's ordering unless a row limit sits beneath it.

arranged_summary_source <- function(con) {
  dplyr::copy_to(
    con, data.frame(g = c("a", "b", "a", "b"), v = c(3L, 1L, 2L, 4L)),
    "arranged_summary_source", temporary = TRUE, overwrite = TRUE
  )
}

test_that("DuckDB summarizes an arranged input without an ordering warning", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  remote <- arranged_summary_source(con)
  run <- function(x, spec, sort, check) {
    summarize_with_margins(
      x, s = sum(.data$v, na.rm = TRUE), .grouping = spec, .sort = sort,
      .check_margin_label = check
    )
  }
  specs <- list(rollup = rollup(dplyr::all_of("g")),
                cube = cube(dplyr::all_of("g")))
  for (spec in names(specs)) {
    for (sort in c("none", "last", "first")) {
      for (check in c(TRUE, FALSE)) {
        info <- paste(spec, sort, check)
        # `expect_no_warning()` takes no `info`.
        expect_warning(
          query <- run(dplyr::arrange(remote, .data$v), specs[[spec]], sort,
                       check),
          NA, info = info
        )
        expect_warning(
          utils::capture.output(dplyr::show_query(query)), NA, info = info
        )
        expect_warning(rows <- collected_row_bag(query), NA, info = info)
        expect_warning(computed <- dplyr::compute(query), NA, info = info)
        expect_identical(collected_row_bag(computed), rows, info = info)
        expect_identical(
          rows, collected_row_bag(run(remote, specs[[spec]], sort, check)),
          info = info
        )
      }
    }
  }
})

test_that("SQLite renders no ORDER BY from an arranged input's ordering", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  remote <- arranged_summary_source(con)
  run <- function(x, spec, sort, check) {
    summarize_with_margins(
      x, s = sum(.data$v, na.rm = TRUE), .grouping = spec, .sort = sort,
      .check_margin_label = check
    )
  }
  # A `.sort` other than "none" emits the Margin order's own ORDER BY, which
  # the unarranged input's query carries too.
  order_clauses <- function(query) {
    sql <- as.character(dbplyr::sql_render(query))
    regmatches(sql, gregexpr("ORDER BY[^\n]*", sql))[[1]]
  }
  specs <- list(rollup = rollup(dplyr::all_of("g")),
                cube = cube(dplyr::all_of("g")))
  for (spec in names(specs)) {
    for (sort in c("none", "last", "first")) {
      for (check in c(TRUE, FALSE)) {
        info <- paste(spec, sort, check)
        arranged <- run(dplyr::arrange(remote, .data$v), specs[[spec]], sort,
                        check)
        plain <- run(remote, specs[[spec]], sort, check)
        expect_identical(order_clauses(arranged), order_clauses(plain),
                         info = info)
        expect_identical(collected_row_bag(arranged),
                         collected_row_bag(plain), info = info)
      }
    }
  }
})

check_limited_arranged_summary <- function(con) {
  remote <- arranged_summary_source(con)
  # The two smallest `v` are one row of each group.
  selected <- utils::head(dplyr::arrange(remote, .data$v), 2L)
  query <- summarize_with_margins(
    selected, s = sum(.data$v, na.rm = TRUE),
    .grouping = rollup(dplyr::all_of("g"))
  )
  # The sum's integer width is the backend's, so compare by value.
  expect_equal(
    collected_row_bag(query),
    data.frame(g = c("Total", "a", "b"), s = c(3, 2, 1))
  )
}

test_that("DuckDB reads the rows a limited input's ordering selects", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  check_limited_arranged_summary(con)
})

test_that("SQLite reads the rows a limited input's ordering selects", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  check_limited_arranged_summary(con)
})

test_that("an arranged input keeps its window ordering on DuckDB", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  query <- summarize_with_margins(
    dplyr::arrange(arranged_summary_source(con), dplyr::desc(.data$g)),
    s = sum(.data$v, na.rm = TRUE), .grouping = rollup(dplyr::all_of("g")),
    .sort = "none"
  )
  # `arrange()` also records a window ordering, which a window function over
  # the result still orders by.
  expect_match(
    dbplyr::sql_render(dplyr::mutate(query, r = cumsum(.data$s))),
    "OVER (ORDER BY g DESC", fixed = TRUE
  )
})

test_that("an arranged input keeps its window ordering on SQLite", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  query <- summarize_with_margins(
    dplyr::arrange(arranged_summary_source(con), dplyr::desc(.data$g)),
    s = sum(.data$v, na.rm = TRUE), .grouping = rollup(dplyr::all_of("g")),
    .sort = "none"
  )
  running <- dplyr::collect(dplyr::mutate(query, r = cumsum(.data$s)))
  # SQLite's binary collation places "Total" before "a", so descending `g`
  # accumulates b, a, Total.
  expect_equal(running$r[match(c("b", "a", "Total"), running$g)],
               c(5, 10, 20))
})

# A typed-missing Margin label keeps the input's window ordering in the native
# summary up to the Margin order, where a term over a column the result lacks
# would reach its ORDER BY (#1641).
test_that("DuckDB sorts a typed-missing summary over a window-ordered input", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  remote <- dplyr::copy_to(
    con,
    data.frame(f = c("p", "p", "q", "q", "p"), a = c("x", "x", "y", "y", "x"),
               v = c(5, 3, 9, 1, 7)),
    "window_ordered_summary_source", temporary = TRUE, overwrite = TRUE
  )
  inputs <- list(
    window_order = dbplyr::window_order(remote, .data$v),
    arrange = dplyr::arrange(remote, .data$v),
    limited = utils::head(dplyr::arrange(remote, .data$v), 3L)
  )
  specs <- list(rollup = rollup(dplyr::all_of("a")),
                cube = cube(dplyr::all_of("a")))
  run <- function(x, spec, sort, label, by) {
    summarize_with_margins(
      x, s = sum(.data$v, na.rm = TRUE), .grouping = spec,
      .by = dplyr::all_of(by), .margin_label = label, .sort = sort
    )
  }
  for (input in names(inputs)) {
    for (spec in names(specs)) {
      for (sort in c("last", "first")) {
        for (label in list(NULL, NA_character_)) {
          for (by in list(character(), "f")) {
            info <- paste(input, spec, sort, deparse(label), deparse(by))
            query <- run(inputs[[input]], specs[[spec]], sort, label, by)
            # Clearing the window ordering by hand is the caller's
            # workaround; its ORDER BY holds the documented key alone, and
            # the input's window ordering renders nowhere in either query.
            unordered <- run(dbplyr::window_order(inputs[[input]]),
                             specs[[spec]], sort, label, by)
            expect_identical(dbplyr::sql_render(query),
                             dbplyr::sql_render(unordered), info = info)
            rows <- as.data.frame(dplyr::collect(query))
            expect_identical(rows, as.data.frame(dplyr::collect(unordered)),
                             info = info)
            expect_identical(
              as.data.frame(dplyr::collect(dplyr::compute(query))), rows,
              info = info
            )
          }
        }
      }
    }
  }
  # The documented key, applied by hand: bit(a), is.na(a), a.
  expect_equal(
    as.data.frame(dplyr::collect(
      run(inputs$window_order, specs$rollup, "last", NULL, NULL)
    )),
    data.frame(a = c("x", "y", NA), s = c(15, 10, 25))
  )
  # The three smallest `v` are 1 (y), 3 and 5 (x).
  expect_equal(
    as.data.frame(dplyr::collect(
      run(inputs$limited, specs$rollup, "last", NULL, NULL)
    )),
    data.frame(a = c("x", "y", NA), s = c(8, 1, 9))
  )
})
