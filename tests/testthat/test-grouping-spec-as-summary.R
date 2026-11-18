# A Grouping specification written into `...` instead of `.grouping` (#1699).
#
# dplyr evaluates it as a summary and refuses it as a non-vector, without
# naming `.grouping`, while a lazy input translates the constructor into SQL
# and returns a query. Both readings are replaced by one refusal raised before
# any backend work.

spec_summary_data <- function() {
  data.frame(g = c("a", "a", "b"), v = c(1, 2, 3))
}

spec_summary_refusal <- function(...) {
  rlang::catch_cnd(
    summarize_with_margins(spec_summary_data(), ...),
    classes = "marginplyr_error"
  )
}

test_that("a specification in `...` is refused with the `.grouping` rewrite", {
  unnamed <- spec_summary_refusal(s = sum(v), rollup(g))
  expect_s3_class(unnamed, "marginplyr_error")
  expect_match(
    conditionMessage(unnamed),
    "Did you mean `.grouping = rollup(g)`?",
    fixed = TRUE
  )

  # The name a caller writes when they drop the dot reaches `...` like any
  # other summary name.
  named <- spec_summary_refusal(s = sum(v), grouping = rollup(g))
  expect_s3_class(named, "marginplyr_error")
  expect_match(
    conditionMessage(named),
    "Did you mean `.grouping = rollup(g)`?",
    fixed = TRUE
  )
})

test_that("every constructor spelling resolving to marginplyr is refused", {
  spec <- grouping_sets(g, NULL)
  cases <- list(
    list(dot = quote(marginplyr::cube(g)),
         rewrite = ".grouping = marginplyr::cube(g)"),
    list(dot = quote(grouping_spec(rollup(g))),
         rewrite = ".grouping = grouping_spec(rollup(g))"),
    list(dot = quote(spec), rewrite = ".grouping = spec")
  )
  for (case in cases) {
    refusal <- spec_summary_refusal(s = sum(v), !!case$dot)
    expect_s3_class(refusal, "marginplyr_error")
    expect_match(
      conditionMessage(refusal),
      paste0("Did you mean `", case$rewrite, "`?"),
      fixed = TRUE
    )
  }
})

test_that("a summary that only shares a constructor's name is computed", {
  # A caller's own `rollup()` is ordinary lookup (ADR 0019), and a column named
  # like a specification-bound local is what the data mask reads.
  rollup <- function(x) sum(x)
  spec <- marginplyr::rollup(g)
  data <- transform(spec_summary_data(), spec = v * 10)

  result <- summarize_with_margins(
    data,
    s = rollup(v),
    t = max(spec),
    .grouping = marginplyr::rollup(g)
  )
  expect_identical(result$s, c(3, 3, 6))
  expect_identical(result$t, c(20, 30, 30))

  one_row_per_group <- data.frame(g = c("a", "b"), spec = c(1, 2))
  expect_identical(
    summarize_with_margins(
      one_row_per_group,
      spec,
      .grouping = marginplyr::grouping_set(g)
    )$spec,
    c(1, 2)
  )
})

test_that("a name whose binding fails is left to the data mask", {
  delayedAssign("broken", stop("caller's own failure"))
  expect_error(
    summarize_with_margins(
      spec_summary_data(),
      s = broken,
      .grouping = rollup(g)
    ),
    "caller's own failure"
  )
})

test_that("the refusal is raised on a lazy input before any query", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  source <- dplyr::copy_to(con, spec_summary_data(), "spec_summary")
  counter <- duckdb_query_counter()
  on.exit(counter$untrace(), add = TRUE)

  lazy <- rlang::catch_cnd(
    summarize_with_margins(source, s = sum(v), rollup(g)),
    classes = "marginplyr_error"
  )
  expect_identical(
    conditionMessage(lazy),
    conditionMessage(spec_summary_refusal(s = sum(v), rollup(g)))
  )
  expect_identical(counter$queries, 0L)
  expect_identical(last_sent_queries()$purpose, character())
})
