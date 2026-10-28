# Compare public names and values without imposing tibble name repair on dtplyr.
expect_lazy_dot_dot_outputs <- function(make_input, materialize) {
  for (name in c("...", "..1", "..42")) {
    for (empty in c(FALSE, TRUE)) {
      data <- data.frame(group = c("b", "a"), value = c(1L, 2L))
      if (empty) data <- data[FALSE, , drop = FALSE]
      input <- make_input(data)
      ordinary <- dplyr::summarize(input, !!name := sum(.data$value))
      plain <- summarize_with_margins(input, !!name := sum(.data$value))
      expect_identical(names(materialize(plain)), names(materialize(ordinary)))
      expect_identical(materialize(plain)[[name]],
                       materialize(ordinary)[[name]])

      for (multiple in c(FALSE, TRUE)) {
        spec <- if (multiple) {
          rollup(dplyr::all_of("group"))
        } else {
          grouping_set(dplyr::all_of("group"))
        }
        for (sort in c("first", "last")) {
          for (with_id in c(FALSE, TRUE)) {
            result <- summarize_with_margins(
              input, !!name := sum(.data$value), .grouping = spec,
              .id = if (with_id) "set" else NULL, .sort = sort
            )
            expect_s3_class(result, if (inherits(input, "dtplyr_step")) {
              "dtplyr_step"
            } else {
              "arrow_dplyr_query"
            })
            result <- materialize(result)
            expected_group <- if (empty) character() else c("a", "b")
            expected_value <- if (empty) integer() else c(2L, 1L)
            expected_id <- rep(1L, length(expected_value))
            if (multiple) {
              position <- if (sort == "first") 0L else length(expected_value)
              expected_group <- append(expected_group, "Total",
                                       after = position)
              expected_value <- append(expected_value, if (empty) 0L else 3L,
                                       after = position)
              expected_id <- append(expected_id, 2L, after = position)
            }
            expect_identical(names(result),
                             c("group", if (with_id) "set", name))
            expect_identical(result[["group"]], expected_group)
            expect_equal(result[[name]], expected_value)
            if (with_id) expect_identical(result[["set"]], expected_id)
          }
        }
      }
      expect_error(
        summarize_with_margins(input, !!name := sum(.data$value), .id = name),
        class = "marginplyr_error"
      )
      expect_error(
        summarize_with_margins(
          input, group = sum(.data$value),
          .grouping = grouping_set(dplyr::all_of("group"))
        ),
        class = "marginplyr_error"
      )
    }
  }
}

test_that("Arrow retains dot-dot summary names through Margin operations", {
  skip_if_suggest_absent("arrow")
  expect_lazy_dot_dot_outputs(arrow::arrow_table, dplyr::collect)
})

test_that("dtplyr retains dot-dot summary names for data-frame conversion", {
  skip_if_suggest_absent("dtplyr")
  expect_lazy_dot_dot_outputs(dtplyr::lazy_dt, as.data.frame)
})

test_that("dtplyr omitted .ROW keys preserve dot-dot summary outputs", {
  skip_if_suggest_absent("dtplyr")
  input <- data.frame(.ROW = c("b", "a"), value = c(1L, 2L))
  for (name in c("...", "..1", "..42")) {
    run <- function(data) {
      summarize_with_margins(
        data, !!name := sum(value), .grouping = rollup(.ROW),
        .margin_label = NULL, .id = "set", .sort = "last"
      )
    }
    expect_identical(as.data.frame(run(dtplyr::lazy_dt(input))), run(input))
  }
})

test_that("dtplyr .ROW identifiers preserve dot-dot summary outputs", {
  skip_if_suggest_absent("dtplyr")
  for (size in c(0L, 1L, 3L)) {
    input <- data.frame(
      g = rep(c("a", NA_character_, "a"), length.out = size),
      value = rep(c(1L, 2L, 1L), length.out = size)
    )
    for (name in c("...", "..1", "..42")) {
      run <- function(data) {
        summarize_with_margins(
          data, !!name := sum(value),
          ..marginplyr_dtplyr_column_1 = dplyr::n(),
          ..marginplyr_dtplyr_column_1_ = sum(value),
          .grouping = rollup(g), .id = ".ROW",
          .margin_label = NULL, .sort = "last"
        )
      }
      actual <- run(dtplyr::lazy_dt(input))
      expect_s3_class(actual, "dtplyr_step")
      expect_identical(as.data.frame(actual), run(input))
    }
  }
})
