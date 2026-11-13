# An `across()` whose `.fns` is a parenthesized `list()` or `c()` of functions
# names and computes its outputs on a lazy input as it does on the data frame
# (#1602). Each backend runs in a test of its own, so no test requires more
# than one member of `optional_backends()`.

parenthesized_fns_input <- function() {
  data.frame(g = c("a", "a", "b"), v = 1:3, w = c(10L, 20L, 30L))
}

# Named and unnamed lists, with and without `.names`.
parenthesized_list_cases <- rlang::exprs(
  named = across(
    c(v, w),
    (list(a = ~ sum(.x, na.rm = TRUE), b = ~ max(.x, na.rm = TRUE))),
    .names = "{.fn}_{.col}"
  ),
  named_default = across(
    c(v, w), (list(a = ~ sum(.x, na.rm = TRUE), b = ~ max(.x, na.rm = TRUE)))
  ),
  unnamed = across(
    c(v, w), (list(~ sum(.x, na.rm = TRUE), ~ max(.x, na.rm = TRUE))),
    .names = "{.col}.{.fn}"
  ),
  unnamed_default = across(
    c(v, w), (list(~ sum(.x, na.rm = TRUE), ~ max(.x, na.rm = TRUE)))
  )
)

# dbplyr ignores `.names` for a `c()` of functions and dtplyr refuses one, with
# or without the pair, so only Arrow is compared with the data frame here.
parenthesized_c_cases <- rlang::exprs(
  c_named = across(
    c(v, w),
    (c(a = ~ sum(.x, na.rm = TRUE), b = ~ max(.x, na.rm = TRUE))),
    .names = "{.fn}_{.col}"
  ),
  c_named_default = across(
    c(v, w), (c(a = ~ sum(.x, na.rm = TRUE), b = ~ max(.x, na.rm = TRUE)))
  ),
  c_unnamed = across(
    c(v, w), (c(~ sum(.x, na.rm = TRUE), ~ max(.x, na.rm = TRUE))),
    .names = "{.col}.{.fn}"
  )
)

parenthesized_fns_rows <- function(query) {
  result <- as.data.frame(dplyr::collect(query))
  result <- result[order(result$g), , drop = FALSE]
  rownames(result) <- NULL
  result
}

parenthesized_fns_run <- function(source, case) {
  list(
    rollup = parenthesized_fns_rows(summarize_with_margins(
      source, !!case, .grouping = rollup(dplyr::all_of("g"))
    )),
    by = parenthesized_fns_rows(summarize_with_margins(
      source, !!case, .by = dplyr::all_of("g")
    ))
  )
}

expect_fns_match_local <- function(source, cases) {
  data <- parenthesized_fns_input()
  for (label in names(cases)) {
    expect_equal(
      parenthesized_fns_run(source, cases[[label]]),
      parenthesized_fns_run(data, cases[[label]]),
      info = label
    )
  }
}

test_that("the data frame names a parenthesized `.fns` list as dplyr does", {
  data <- parenthesized_fns_input()
  named <- parenthesized_fns_run(data, parenthesized_list_cases$named)$rollup
  expect_named(named, c("g", "a_v", "b_v", "a_w", "b_w"))
  at <- match(c("a", "b", "Total"), named$g)
  expect_equal(named$a_w[at], c(30L, 30L, 60L))
  expect_equal(named$b_v[at], c(2L, 3L, 3L))
  expect_named(
    parenthesized_fns_run(data, parenthesized_list_cases$unnamed)$by,
    c("g", "v.1", "v.2", "w.1", "w.2")
  )
})

test_that("DuckDB names a parenthesized `.fns` list as the data frame", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_fns_match_local(
    dplyr::copy_to(con, parenthesized_fns_input(), "parenthesized_fns"),
    parenthesized_list_cases
  )
})

test_that("SQLite names a parenthesized `.fns` list as the data frame", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_fns_match_local(
    dplyr::copy_to(con, parenthesized_fns_input(), "parenthesized_fns"),
    parenthesized_list_cases
  )
})

test_that("dtplyr accepts a parenthesized `.fns` list", {
  skip_if_suggest_absent("dtplyr")
  expect_fns_match_local(
    dtplyr::lazy_dt(parenthesized_fns_input()),
    parenthesized_list_cases
  )
})

test_that("Arrow accepts a parenthesized `.fns` list or `c()`", {
  skip_if_suggest_absent("arrow")
  expect_fns_match_local(
    arrow::arrow_table(parenthesized_fns_input()),
    c(parenthesized_list_cases, parenthesized_c_cases)
  )
})

test_that("a parenthesized `.fns` list leaves infix parentheses in the SQL", {
  source <- dbplyr::tbl_lazy(
    parenthesized_fns_input(), con = dbplyr::simulate_postgres()
  )
  sql <- as.character(dbplyr::sql_render(summarize_with_margins(
    source,
    across(
      c(v, w), (list(a = ~ sum(.x, na.rm = TRUE))), .names = "{.fn}_{.col}"
    ),
    diff = sum(w, na.rm = TRUE) - (sum(v, na.rm = TRUE) - 1L),
    both = sum(dplyr::if_else(g == "a" & (v > 2L | w > 10L), 1L, 0L),
               na.rm = TRUE),
    .grouping = rollup(dplyr::all_of("g"))
  )))
  expect_match(sql, "SUM(\"w\") - (SUM(\"v\") - 1)", fixed = TRUE)
  expect_match(sql, "(\"v\" > 2 OR \"w\" > 10)", fixed = TRUE)
  expect_match(sql, "AS \"a_v\"", fixed = TRUE)
})
