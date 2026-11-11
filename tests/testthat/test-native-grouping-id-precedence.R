# A multi-column `grouping_id()` is the whole bit mask to the expression around
# it (ADR 0009), on native grouping sets as everywhere else (#1569). Expected
# values are R arithmetic over masks written out by hand from the plan, never
# a value marginplyr computes.

precedence_input <- function() {
  data.frame(
    g = "k", a = c("x", "x", "y", "y"), b = c("p", "q", "p", "q"), v = 1:4
  )
}

# `grouping_sets(a, b)` has the masks 1 (a kept, set 1) and 2 (b kept, set 2),
# so no case divides by zero. With `g` fixed by
# `.by`, `grouping_id(g, a, b)` carries a zero most-significant bit and the
# same values.
precedence_masks <- c(`1` = 1, `2` = 2)

# Each case is written over `m`, which stands for the mask. The caller's
# expression is the case with `grouping_id()` in place of `m`; the expected
# value is the case evaluated in R with `m` bound to the mask.
precedence_cases <- list(
  add_right = quote(m + 10L), add_left = quote(10L + m),
  sub_right = quote(m - 10L), sub_left = quote(3L - m),
  mul_right = quote(m * 10L), mul_left = quote(10L * m),
  div_right = quote(m / 2), div_left = quote(6 / m),
  idiv_right = quote(m %/% 2L), idiv_left = quote(7L %/% m),
  mod_right = quote(m %% 2L), mod_left = quote(7L %% m),
  pow_right = quote(m^2L), pow_left = quote(2L^m),
  negate = quote(-m), caller_parens = quote((m) * 10L)
)

precedence_dots <- function(helper) {
  lapply(precedence_cases, function(case) {
    do.call(substitute, list(case, list(m = helper)))
  })
}

test_that("native grouping_id() arithmetic evaluates the whole mask", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  source <- dplyr::copy_to(con, precedence_input(), "precedence_input")

  for (helper in list(quote(grouping_id()), quote(grouping_id(g, a, b)))) {
    query <- summarize_with_margins(
      source, total = sum(v), scaled = sum(v) * !!helper,
      !!!precedence_dots(helper),
      .by = g, .grouping = grouping_sets(a, b), .id = "id"
    )
    result <- dplyr::arrange(dplyr::collect(query), id)
    mask <- precedence_masks[as.character(result$id)]
    expect_identical(unname(mask), c(1, 1, 2, 2))
    for (name in names(precedence_cases)) {
      expected <- eval(precedence_cases[[name]], list(m = unname(mask)))
      expect_equal(result[[name]], expected, info = name)
    }
    expect_equal(result$scaled, result$total * unname(mask))
  }
})

test_that("rendered PostgreSQL SQL groups the mask inside caller arithmetic", {
  source <- dbplyr::tbl_lazy(
    precedence_input(), con = dbplyr::simulate_postgres()
  )
  query <- summarize_with_margins(
    source, weight = grouping_id() * 10L, depth = 3L - grouping_id(),
    negate = -grouping_id(),
    .grouping = rollup(a, b)
  )
  sql <- as.character(dbplyr::sql_render(query))
  mask <- "(GROUPING(\"a\") * 2.0 + GROUPING(\"b\"))"
  expect_match(sql, paste0(mask, " * 10 AS \"weight\""), fixed = TRUE)
  expect_match(sql, paste0("3 - ", mask, " AS \"depth\""), fixed = TRUE)
  expect_match(sql, paste0("-", mask, " AS \"negate\""), fixed = TRUE)
})

test_that("bare masks and one-term helpers keep their native values and type", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  source <- dplyr::copy_to(con, precedence_input(), "precedence_kept")

  # `rollup(a, b)` has the masks 0, 1 and 3 in set order.
  query <- summarize_with_margins(
    source,
    level = grouping_id(), fixed = grouping_id(g, a, b),
    bit = grouping_bit(b), bit_left = 3L - grouping_bit(b),
    one = grouping_id(b) * 10L, fixed_bit = grouping_bit(g) * 10L,
    .by = g, .grouping = rollup(a, b), .id = "id"
  )
  kept <- c("level", "fixed", "bit", "bit_left", "one", "fixed_bit")
  result <- dplyr::distinct(
    dplyr::collect(query), id, dplyr::across(dplyr::all_of(kept))
  )
  result <- dplyr::arrange(result, id)
  expect_equal(result$level, c(0, 1, 3))
  expect_equal(result$fixed, c(0, 1, 3))
  expect_equal(result$bit, c(0, 1, 1))
  expect_equal(result$bit_left, c(3, 2, 2))
  expect_equal(result$one, c(0, 10, 10))
  expect_equal(result$fixed_bit, c(0, 0, 0))
  for (name in kept) {
    expect_type(result[[name]], "double")
  }
})
