# A later ordinary summary selecting an earlier share by a string names it as
# surely as the symbol does, and owes the caller the same refusal (ADR 0010).
# Native dplyr selects the summary in each of these, so a missing-column error
# would deny a summary the caller defined.

earlier_share_by_string <- function(data) {
  sh_name <- "sh"
  summaries <- list(
    quote(sum(dplyr::pick("sh")$sh)),
    quote(sum(dplyr::across("sh", sum))),
    quote(sum(dplyr::c_across("sh"))),
    quote(any(dplyr::if_any("sh", function(x) x > 0))),
    quote(all(dplyr::if_all("sh", function(x) x > 0))),
    quote(sum(dplyr::pick(c("sh"))[[1L]])),
    quote(sum(dplyr::pick(c("s", "sh"))[[2L]])),
    quote(sum(dplyr::pick(dplyr::all_of("sh"))[[1L]])),
    rlang::expr(sum(dplyr::pick(!!sh_name)[[1L]])),
    rlang::expr(sum(dplyr::pick(!!rlang::quo("sh"))[[1L]])),
    quote(sum(dplyr::pick(dplyr::all_of("sh") | s)[[1L]]))
  )
  shares <- list(
    "Total share" = quote(share_of_total(s)),
    "Parent share" = quote(share_of_parent(s))
  )
  for (kind in names(shares)) {
    for (summary in summaries) {
      label <- paste(kind, rlang::expr_deparse(summary))
      # `v` is resolved in the summary's data mask.
      # nolint start: object_usage_linter.
      error <- expect_error(
        summarize_with_margins(
          data, s = sum(v), sh = !!shares[[kind]], z = !!summary,
          .grouping = rollup("g")
        ),
        paste0("Ordinary summaries cannot use an earlier ", kind, " (`sh`)"),
        fixed = TRUE, class = "marginplyr_error", info = label
      )
      # nolint end
      expect_null(error$parent, info = label)
    }
  }
}

test_that("a later summary selecting an earlier share by string is refused", {
  plain <- data.frame(g = c("a", "a", "b"), v = c(1, 2, 5))
  earlier_share_by_string(plain)
  # An input column of that name is hidden by the share, as it is from the
  # symbol spelling.
  earlier_share_by_string(transform(plain, sh = c(100, 200, 300)))
})

test_that("a data.table selecting an earlier share by string is refused", {
  skip_if_suggest_absent("data.table")
  earlier_share_by_string(
    data.table::data.table(g = c("a", "a", "b"), v = c(1, 2, 5))
  )
})

test_that("a string the share does not answer is read as before", {
  plain <- data.frame(g = c("a", "a", "b"), v = c(1, 2, 5))
  later <- function(summary) {
    summarize_with_margins(
      plain, s = sum(v), sh = share_of_total(s), z = !!summary,
      .grouping = rollup("g"), .sort = "last"
    )
  }
  # A tolerant selection leaves the share out rather than naming it (#1619).
  expect_identical(later(quote(ncol(dplyr::pick(dplyr::any_of("sh")))))$z,
                   c(0L, 0L, 0L))
  expect_identical(later(quote(sum(dplyr::pick("s")$s)))$z, c(3, 5, 8))
  expect_identical(later(quote(nchar("sh")))$z, c(2L, 2L, 2L))
})
