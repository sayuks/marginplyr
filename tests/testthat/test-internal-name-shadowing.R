# Internal locals are injected into every generated expression, so a source
# column can never supply one of them. `new_margin_internal_names()` protects
# generated *column* names; this file protects the *values* the package
# computes in R and then names inside a data-masked expression, which is the
# other half of the collision-safety contract.
#
# The names below are the locals of `reconstruct_factor.data.frame()`,
# `reconstruct_factor.tbl_duckdb_connection()`, and
# `nest_expanded_margins()`. Each one is exercised as an ordinary source
# column that the Margin operation must carry through untouched.

factor_shadow_names <- c("new_levels", "ord", "missing_sentinel", "col")

factor_shadow_data <- function(name) {
  data <- data.frame(
    f = factor(c("a", "b", "a"), levels = c("a", "b")),
    carrier = c("p", "p", "q"),
    v = c(1, 2, 3)
  )
  names(data)[names(data) == "carrier"] <- name
  data
}

# Rows are (f = a, p, 1), (f = b, p, 2), (f = a, q, 3), so partition `p` has
# both levels and a subtotal of 3, and partition `q` has one level and the
# same subtotal.
expected_factor_shadow <- function(name) {
  data.frame(
    carrier = c("p", "p", "p", "q", "q"),
    f = factor(
      c("a", "b", "Total", "a", "Total"),
      levels = c("a", "b", "Total")
    ),
    s = c(1, 2, 3, 3, 3)
  ) |>
    stats::setNames(c(name, "f", "s"))
}

test_that("factor restoration ignores source columns named like its locals", {
  for (name in factor_shadow_names) {
    result <- summarize_with_margins(
      factor_shadow_data(name),
      s = sum(v),
      .by = dplyr::all_of(name),
      .grouping = rollup(f)
    )
    result <- result[order(result[[name]], result$f), ]
    rownames(result) <- NULL

    expect_equal(result, expected_factor_shadow(name), info = name)
    expect_s3_class(result$f, "factor")
    expect_equal(levels(result$f), c("a", "b", "Total"), info = name)
  }
})

test_that("ordered status survives a source column named `ord`", {
  data <- data.frame(
    size = ordered(c("s", "l", "s"), levels = c("s", "l")),
    ord = c("p", "p", "q"),
    v = c(1, 2, 3)
  )

  result <- summarize_with_margins(
    data,
    s = sum(v),
    .by = ord,
    .grouping = rollup(size)
  )

  expect_true(is.ordered(result$size))
  expect_equal(levels(result$size), c("s", "l", "Total"))
})

test_that("a factor dimension may itself be named like an internal local", {
  for (name in factor_shadow_names) {
    data <- data.frame(
      x = factor(c("a", "b", "a"), levels = c("a", "b")),
      v = c(1, 2, 3)
    )
    names(data)[1] <- name

    result <- summarize_with_margins(
      data,
      s = sum(v),
      .grouping = rollup(dplyr::all_of(name))
    )

    expect_equal(
      as.character(result[[name]]),
      c("a", "b", "Total"),
      info = name
    )
    expect_equal(levels(result[[name]]), c("a", "b", "Total"), info = name)
  }
})

test_that("missing factor values survive a column named `missing_sentinel`", {
  data <- data.frame(
    f = addNA(factor(c("a", NA, "a"))),
    missing_sentinel = c("p", "p", "p"),
    v = c(1, 2, 3)
  )

  result <- summarize_with_margins(
    data,
    s = sum(v),
    .by = missing_sentinel,
    .grouping = rollup(f)
  )

  # `addNA()` makes the missing value a level of its own, which the Margin
  # label must not absorb: three rows, one per level plus the total.
  expect_equal(nrow(result), 3L)
  expect_equal(levels(result$f), c("a", NA, "Total"), ignore_attr = FALSE)
  expect_equal(result$s[result$f == "Total" & !is.na(result$f)], 6)
})

test_that("dtplyr factor restoration ignores columns named like its locals", {
  skip_if_backend_absent("dtplyr")

  for (name in c("new_levels", "ord")) {
    result <- dplyr::collect(summarize_with_margins(
      dtplyr::lazy_dt(factor_shadow_data(name)),
      s = sum(v),
      .by = dplyr::all_of(name),
      .grouping = rollup(f)
    ))
    result <- result[order(
      result[[name]],
      match(as.character(result$f), c("a", "b", "Total"))
    ), ]

    expect_equal(
      as.character(result$f),
      c("a", "b", "Total", "a", "Total"),
      info = name
    )
    expect_equal(result$s, c(1, 2, 3, 3, 3), info = name)
  }
})

test_that("duckdb factor restoration ignores a column named `sql_query`", {
  skip_if_backend_absent("duckdb", "DBI")

  data <- factor_shadow_data("sql_query")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  duckdb::duckdb_register(con, "shadow", data)

  result <- dplyr::collect(summarize_with_margins(
    dplyr::tbl(con, "shadow"),
    s = sum(v, na.rm = TRUE),
    .by = sql_query,
    .grouping = rollup(f)
  ))
  # Sort by level position rather than by string, so the assertion does not
  # depend on the collation the backend or locale happens to use.
  result <- result[order(
    result$sql_query,
    match(as.character(result$f), c("a", "b", "Total"))
  ), ]

  expect_equal(
    as.character(result$f),
    c("a", "b", "Total", "a", "Total")
  )
  expect_equal(result$s, c(1, 2, 3, 3, 3))
})

test_that("nesting ignores source columns named like its locals", {
  for (name in c("group_cols", "keep_cols", "set_col")) {
    data <- data.frame(
      g = c("a", "a", "b"),
      carrier = c("p", "q", "r"),
      v = c(1, 2, 3)
    )
    names(data)[names(data) == "carrier"] <- name

    nested <- nest_with_margins(data, .grouping = rollup(g), .keep = TRUE)

    expect_equal(nrow(nested), 3L, info = name)
    expect_equal(as.character(nested$g), c("a", "b", "Total"), info = name)
    expect_equal(
      vapply(nested$data, nrow, integer(1)),
      c(2L, 1L, 3L),
      info = name
    )
    # `.keep = TRUE` retains the pre-margin key, so the nested frames carry
    # the original `g` values rather than the Margin label.
    expect_equal(
      as.character(nested$data[[3L]]$g),
      c("a", "a", "b"),
      info = name
    )
    expect_true(name %in% names(nested$data[[3L]]), info = name)
  }
})

test_that("row-wise nesting ignores source columns named like its locals", {
  data <- data.frame(
    g = c("a", "a", "b"),
    keep_cols = c("p", "q", "r"),
    v = c(1, 2, 3)
  )

  nested <- nest_by_with_margins(data, .grouping = rollup(g), .keep = TRUE)

  expect_equal(nrow(nested), 3L)
  expect_equal(vapply(nested$data, nrow, integer(1)), c(2L, 1L, 3L))
})
