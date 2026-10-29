# Check source refusal through the public summary and its caller's execution.
expect_special_share_refusals <- function(make_source, materialize) {
  input <- tibble::tibble(g = c("a", "b"), x = c(1, 3))
  for (helper in c("share_of_parent", "share_of_total")) {
    for (column in c("..-1", "..-0", "..+1", "..+0", ".. 1")) {
      for (size in c(0L, 2L)) {
        bad_summary <- function(x) rep(sum(x), size)
        expect_error(
          materialize(summarize_with_margins(
            make_source(input), !!column := bad_summary(.data$x),
            fraction = !!rlang::call2(helper, rlang::sym(column)),
            .grouping = rollup(dplyr::all_of("g"))
          )),
          class = "marginplyr_share_cardinality_error"
        )
      }
      expect_error(
        materialize(summarize_with_margins(
          make_source(input), !!column := "1",
          fraction = !!rlang::call2(helper, rlang::sym(column)),
          .grouping = rollup(dplyr::all_of("g"))
        )),
        "plain integer or double scalar", class = "marginplyr_error"
      )
      expect_error(
        summarize_with_margins(
          make_source(input), !!column := sum(.data$x),
          !!column := sum(.data$x),
          fraction = !!rlang::call2(helper, rlang::sym(column)),
          .grouping = rollup(dplyr::all_of("g"))
        ),
        "defined exactly once", class = "marginplyr_error"
      )
    }
  }
  for (column in c("...", "..1", "..2")) {
    names(input)[1L] <- column
    expect_error(
      summarize_with_margins(
        make_source(input), value = sum(.data$x),
        .by = dplyr::all_of(column), .grouping = grouping_set()
      ),
      class = "vctrs_error_names_cannot_be_dot_dot"
    )
  }
}

test_that("local special share names retain source and input refusals", {
  expect_special_share_refusals(identity, identity)
})

test_that("dtplyr special share names retain source and input refusals", {
  skip_if_suggest_absent("dtplyr")
  expect_special_share_refusals(dtplyr::lazy_dt, dplyr::collect)
})
