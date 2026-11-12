# A summary on a lazy SQL input keeps the grouping the caller's parentheses
# express, because dbplyr renders infix operators as text and relies on the
# `(` node to group them (#1582). Expected values are native dbplyr summaries
# over the same table at each grain, never another marginplyr path.

parentheses_input <- function() {
  data.frame(
    g = c("a", "a", "b", "b"), h = c("x", "y", "x", "y"),
    v = 1:4, w = c(10L, 20L, 30L, 40L)
  )
}

# Each shape is one dbplyr renders wrongly without the caller's pair: a
# difference or a sum on the right of `-`, `&` beside a parenthesized `|` in
# either order and inside `case_when()`, a difference on the right of `%%`, and
# comparisons against or between parenthesized operands.
parentheses_cases <- rlang::exprs(
  sub_sub = sum(w, na.rm = TRUE) - (sum(v, na.rm = TRUE) - 1L),
  sub_add = sum(w, na.rm = TRUE) - (sum(v, na.rm = TRUE) + 1L),
  and_or = sum(dplyr::if_else(g == "a" & (h == "x" | v > 3L), w, 0L),
               na.rm = TRUE),
  or_and = sum(dplyr::if_else((h == "x" | v > 3L) & g == "b", w, 0L),
               na.rm = TRUE),
  case_and_or = sum(dplyr::case_when(
    g == "b" & (h == "y" | v < 2L) ~ w, .default = 0L
  ), na.rm = TRUE),
  mod_sub = sum(w, na.rm = TRUE) %% (sum(v, na.rm = TRUE) - 1L),
  cmp_sub = sum(dplyr::if_else(w > (v - 1L) * 15L, 1L, 0L), na.rm = TRUE),
  cmp_cmp = sum(dplyr::if_else((v > 1L) == (w > 30L), 1L, 0L), na.rm = TRUE)
)

# DuckDB refuses a number times a logical natively, so this shape is SQLite's.
sqlite_parentheses_cases <- c(parentheses_cases, rlang::exprs(
  mul_cmp = sum(v, na.rm = TRUE) * (max(v, na.rm = TRUE) == 4L)
))

# The native summary at `(g)` followed by the Grand total row, which is what
# `rollup(g)` returns at its two grains.
native_rollup <- function(source, cases) {
  by_g <- dplyr::collect(dplyr::summarise(
    source, !!!cases, .by = dplyr::all_of("g")
  ))
  grand <- dplyr::collect(dplyr::summarise(source, !!!cases))
  rows <- rbind(
    as.data.frame(by_g[order(by_g$g), , drop = FALSE]),
    cbind(g = "Total", as.data.frame(grand))
  )
  rownames(rows) <- NULL
  rows
}

margin_rows <- function(query, cases) {
  result <- as.data.frame(dplyr::collect(query))
  result <- result[order(result$g == "Total", result$g), , drop = FALSE]
  rownames(result) <- NULL
  result[c("g", names(cases))]
}

expect_native_parentheses <- function(source, cases, native = TRUE) {
  expected <- native_rollup(source, cases)
  queries <- list(portable = summarize_with_margins(
    source, !!!cases, .grouping = rollup(dplyr::all_of("g")),
    .duplicates = "keep", .id = "set"
  ))
  if (native) {
    queries$native <- summarize_with_margins(
      source, !!!cases, .grouping = rollup(dplyr::all_of("g"))
    )
  }
  for (adapter in names(queries)) {
    expect_equal(
      margin_rows(queries[[adapter]], cases), expected, info = adapter
    )
  }
  by_only <- dplyr::collect(summarize_with_margins(
    source, !!!cases, .by = dplyr::all_of("g")
  ))
  native_by <- dplyr::collect(dplyr::summarise(
    source, !!!cases, .by = dplyr::all_of("g")
  ))
  expect_equal(
    as.data.frame(by_only[order(by_only$g), , drop = FALSE]),
    as.data.frame(native_by[order(native_by$g), , drop = FALSE])
  )
}

test_that("SQLite summaries keep the caller's parentheses", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(con, parentheses_input(), "parentheses_input")
  expect_native_parentheses(source, sqlite_parentheses_cases, native = FALSE)
})

test_that("DuckDB summaries keep the caller's parentheses on both adapters", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  source <- dplyr::copy_to(con, parentheses_input(), "parentheses_input")
  expect_native_parentheses(source, parentheses_cases)
})

test_that("shares and helpers under caller parentheses match the data frame", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  data <- parentheses_input()
  source <- dplyr::copy_to(con, data, "parentheses_shares")
  run <- function(x) {
    result <- as.data.frame(dplyr::collect(summarize_with_margins(
      x,
      net = sum(w, na.rm = TRUE) - (sum(v, na.rm = TRUE) - 1L),
      sh = share_of_total(net),
      shifted = sum(v, na.rm = TRUE) - (grouping_id() - 1L),
      .grouping = rollup(g, h), .sort = "none"
    )))
    result <- result[order(result$g, result$h), , drop = FALSE]
    rownames(result) <- NULL
    result
  }
  local <- run(data)
  # The Total share is (10 - (1 - 1)) / (100 - (10 - 1)) at the cell `a`/`x`,
  # and the shifted sum at the Grand total, mask 3, is 10 - (3 - 1).
  expect_equal(local$sh[local$g == "a" & local$h == "x"], 10 / 91)
  expect_equal(local$shifted[local$g == "Total"], 8)
  expect_equal(run(source), local)
})

test_that("rendered PostgreSQL and SQL Server text carries the caller's pair", {
  expected <- c(
    postgres = "SUM(\"w\") - (SUM(\"v\") - 1)",
    mssql = "SUM([w]) - (SUM([v]) - 1)"
  )
  simulators <- list(
    postgres = dbplyr::simulate_postgres(), mssql = dbplyr::simulate_mssql()
  )
  for (dialect in names(simulators)) {
    source <- dbplyr::tbl_lazy(
      parentheses_input(), con = simulators[[dialect]]
    )
    for (query in list(
      summarize_with_margins(
        source, diff = sum(w, na.rm = TRUE) - (sum(v, na.rm = TRUE) - 1L),
        .grouping = rollup(g)
      ),
      summarize_with_margins(
        source, diff = sum(w, na.rm = TRUE) - (sum(v, na.rm = TRUE) - 1L),
        .by = g
      )
    )) {
      sql <- as.character(dbplyr::sql_render(query))
      expect_match(sql, expected[[dialect]], fixed = TRUE, info = dialect)
    }
  }
})
