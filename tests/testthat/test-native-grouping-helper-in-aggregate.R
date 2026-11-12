# A Grouping helper inside an aggregate is the branch's literal on every
# backend, so a native summary holding one runs on the portable adapter
# (ADR 0009, #1587). Expected values are the local data frame's result, which
# substitutes the literal; `rollup(a, b)` fixes the bit of `b` at 0 in the
# `(a, b)` rows and at 1 in every margin row.

aggregate_input <- function() {
  data.frame(a = c("x", "x", "y"), b = c("p", "q", "p"), v = c(1, 2, 4))
}

ordered_result <- function(query) {
  result <- as.data.frame(dplyr::collect(query))
  result <- result[do.call(order, unname(result[c("a", "b")])), ]
  rownames(result) <- NULL
  result
}

expect_duckdb_matches_local <- function(con, table, ...) {
  source <- dplyr::copy_to(con, aggregate_input(), table)
  local <- ordered_result(summarize_with_margins(aggregate_input(), ...))
  native <- ordered_result(summarize_with_margins(source, ...))
  expect_equal(native, local)
  invisible(local)
}

test_that("a helper inside an aggregate returns the literal value on DuckDB", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  local <- expect_duckdb_matches_local(
    con, "inside_aggregate",
    detail_only = sum(v * (1 - grouping_bit(b)), na.rm = TRUE),
    deepest = max(grouping_id(a, b), na.rm = TRUE),
    weighted = sum(v * grouping_id(), na.rm = TRUE),
    bit_max = max(grouping_bit(b), na.rm = TRUE),
    id_weighted = sum(v * grouping_id(a, b), na.rm = TRUE),
    .grouping = rollup(a, b)
  )
  # Written out from the plan rather than read from marginplyr: the label sorts
  # first, so the rows are (), (x), (x, p), (x, q), (y), (y, p) with masks
  # 3, 1, 0, 0, 1, 0.
  expect_equal(local$detail_only, c(0, 0, 1, 2, 0, 4))
  expect_equal(local$deepest, c(3, 1, 0, 0, 1, 0))
  expect_equal(local$weighted, c(21, 3, 0, 0, 4, 0))
  expect_equal(local$bit_max, c(1, 1, 0, 0, 1, 0))
  expect_equal(local$id_weighted, c(21, 3, 0, 0, 4, 0))
})

test_that("a helper outside an aggregate returns the same value on DuckDB", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  local <- expect_duckdb_matches_local(
    con, "outside_aggregate",
    scaled = sum(v) * grouping_bit(b),
    .grouping = rollup(a, b)
  )
  # Rows as above, with sums 7, 3, 1, 2, 4, 4 and bits of `b` 1, 1, 0, 0, 1, 0.
  expect_equal(local$scaled, c(7, 3, 0, 0, 4, 0))
})

test_that("an across() lambda holding a helper inside an aggregate", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  expect_duckdb_matches_local(
    con, "inside_across",
    dplyr::across(dplyr::all_of("v"), ~ sum(.x * grouping_bit(b))),
    .grouping = rollup(a, b)
  )
})

test_that("a duplicate set keeps or drops its rows with a helper inside", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  for (duplicates in c("keep", "drop")) {
    expect_duckdb_matches_local(
      con, paste0("inside_duplicates_", duplicates),
      detail_only = sum(v * (1 - grouping_bit(b)), na.rm = TRUE),
      .grouping = grouping_sets(rollup(a, b), grouping_set()),
      .duplicates = duplicates
    )
  }
})

test_that("rendered PostgreSQL SQL puts no GROUPING() inside an aggregate", {
  source <- dbplyr::tbl_lazy(
    aggregate_input(), con = dbplyr::simulate_postgres()
  )
  inside <- as.character(dbplyr::sql_render(summarize_with_margins(
    source, detail_only = sum(v * (1 - grouping_bit(b)), na.rm = TRUE),
    .grouping = rollup(a, b)
  )))
  expect_no_match(inside, "GROUPING", fixed = TRUE)
  expect_match(inside, "UNION ALL", fixed = TRUE)

  outside <- as.character(dbplyr::sql_render(summarize_with_margins(
    source, scaled = sum(v) * grouping_bit(b),
    bit = dplyr::if_else(grouping_bit(b) == 1L, "margin", "detail"),
    .grouping = rollup(a, b)
  )))
  expect_match(outside, "GROUPING SETS", fixed = TRUE)
  expect_match(outside, "SUM(\"v\") * GROUPING(\"b\")", fixed = TRUE)
  expect_no_match(outside, "UNION ALL", fixed = TRUE)
})

test_that("a call the dialect does not hold as scalar alone is portable", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  source <- dplyr::copy_to(con, aggregate_input(), "inside_dialect")

  # `str_flatten()` is in DuckDB's scalar table and in its aggregate table;
  # `untranslated_fn()` is in neither.
  for (helper_call in list(
    quote(str_flatten(as.character(grouping_bit(b)), ",")),
    quote(untranslated_fn(grouping_bit(b)))
  )) {
    sql <- as.character(dbplyr::sql_render(summarize_with_margins(
      source, value = !!helper_call, .grouping = rollup(a, b)
    )))
    expect_no_match(sql, "GROUPING", fixed = TRUE)
    expect_match(sql, "UNION ALL", fixed = TRUE)
  }
})
