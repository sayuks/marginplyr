test_that("dtplyr nests the one-row AsIs numeric rollup", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(g = I(1))
  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    result <- verb(
      dtplyr::lazy_dt(input), .grouping = rollup(g), .sort = "last"
    )
    if (identical(verb, nest_with_margins)) result <- dplyr::collect(result)
    expect_identical(names(result), c("g", "data"))
    expect_identical(result$g, c("1", "Total"))
    expect_identical(vapply(result$data, nrow, integer(1)), c(1L, 1L))
    expect_identical(vapply(result$data, ncol, integer(1)), c(0L, 0L))
  }
})

# Cells are backend data frames; compare their columns without their container
# classes, as ADR 0016 leaves those to the underlying nesting operations.
asis_nesting_values <- function(result) {
  values <- as.list(dplyr::ungroup(result))
  values$data <- lapply(values$data, as.list)
  values
}

test_that("dtplyr AsIs dimensions retain typed cells across Margin labels", {
  skip_if_suggest_absent("dtplyr")
  vectors <- list(
    logical = c(TRUE, NA, TRUE, FALSE),
    integer = c(1L, NA_integer_, 1L, 2L),
    double = c(1, NA_real_, 1, 2),
    character = c("a", NA_character_, "a", "b")
  )
  for (type in names(vectors)) {
    for (rows in c(0L, 1L, 4L)) {
      input <- tibble::tibble(
        g = I(vectors[[type]][seq_len(rows)]), value = seq_len(rows)
      )
      source <- data.table::as.data.table(input)
      before <- data.table::copy(source)
      lazy <- dtplyr::lazy_dt(source)
      for (label in list("Total", NULL, NA_character_)) {
        for (keep in c(FALSE, TRUE)) {
          for (verb in list(nest_with_margins, nest_by_with_margins)) {
            run <- function(data) {
              verb(
                data, .grouping = rollup(g), .margin_label = label,
                .keep = keep, .id = "set", .sort = "last"
              )
            }
            info <- paste(type, rows, keep, deparse(label))
            actual <- run(lazy)
            expect_identical(source, before, info = info)
            if (identical(verb, nest_with_margins)) {
              expect_s3_class(actual, "dtplyr_step")
              actual <- dplyr::collect(actual)
              expect_identical(dplyr::group_vars(actual), character())
            } else {
              expect_s3_class(actual, "rowwise_df")
              expect_identical(dplyr::group_vars(actual), c("g", "set"))
            }
            expect_identical(names(actual), c("g", "set", "data"), info = info)
            expect_identical(
              asis_nesting_values(actual), asis_nesting_values(run(input)),
              info = info
            )
            expect_identical(
              sum(vapply(actual$data, nrow, integer(1))), rows * 2L, info = info
            )
            for (cell in actual$data) {
              expect_identical(
                names(cell), if (keep) c("g", "value") else "value", info = info
              )
              if (keep) {
                expect_identical(cell$g, input$g[cell$value], info = info)
              }
            }
            expect_identical(source, before, info = info)
          }
        }
      }
    }
  }
})

test_that("dtplyr AsIs nesting controls retain representable source types", {
  skip_if_suggest_absent("dtplyr")
  for (values in list(c(TRUE, FALSE), 1:2, c(1, 2), c("a", "b"))) {
    input <- tibble::tibble(g = I(values), value = 1:2)
    lazy <- dtplyr::lazy_dt(input)
    missing <- vctrs::vec_init(input$g, 2L)
    upstream <- dplyr::union_all(lazy, dplyr::mutate(lazy, g = !!missing)) |>
      dplyr::collect()
    expect_identical(upstream$g, vctrs::vec_c(input$g, missing))
    for (verb in list(nest_with_margins, nest_by_with_margins)) {
      for (role in c("single", "fixed", "payload")) {
        run <- function(data) {
          switch(role,
            single = verb(data, .grouping = grouping_set(g), .keep = TRUE,
                          .sort = "last"),
            fixed = verb(data, .by = g, .grouping = rollup(value),
                         .keep = TRUE, .sort = "last"),
            payload = verb(data, .grouping = rollup(value), .keep = TRUE,
                           .sort = "last")
          )
        }
        actual <- run(lazy)
        if (identical(verb, nest_with_margins)) actual <- dplyr::collect(actual)
        expect_identical(
          asis_nesting_values(actual), asis_nesting_values(run(input)),
          info = role
        )
        expect_true(all(vapply(actual$data, function(cell) {
          identical(cell$g, input$g[cell$value])
        }, logical(1))))
      }
    }
  }
  input <- tibble::tibble(g = c(1, 2), value = 1:2)
  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    actual <- verb(dtplyr::lazy_dt(input), .grouping = rollup(g), .keep = TRUE,
                   .sort = "last")
    if (identical(verb, nest_with_margins)) actual <- dplyr::collect(actual)
    expected <- verb(input, .grouping = rollup(g), .keep = TRUE, .sort = "last")
    expect_identical(asis_nesting_values(actual), asis_nesting_values(expected))
  }
})

test_that("dtplyr AsIs nesting executes rows only at its collection boundary", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(g = I(c(1, 2)), value = 1:2)
  before <- data.table::copy(source)
  seen <- integer()
  read_rows <- function(x) {
    seen <<- c(seen, x)
    rep(TRUE, length(x))
  }
  lazy <- dtplyr::lazy_dt(source) |>
    dplyr::filter(read_rows(value))
  for (label in list("Total", NULL, NA_character_)) {
    for (keep in c(FALSE, TRUE)) {
      seen <- integer()
      query <- nest_with_margins(
        lazy, .grouping = rollup(g), .keep = keep, .margin_label = label
      )
      expect_s3_class(query, "dtplyr_step")
      expect_identical(seen, integer())
      expect_identical(source, before)
      result <- dplyr::collect(query)
      expect_identical(sort(unique(seen)), 1:2)
      expect_identical(sum(vapply(result$data, nrow, integer(1))), 4L)
      expect_identical(source, before)
      seen <- integer()
      rowwise <- nest_by_with_margins(
        lazy, .grouping = rollup(g), .keep = keep, .margin_label = label
      )
      expect_s3_class(rowwise, "rowwise_df")
      expect_identical(sort(unique(seen)), 1:2)
      expect_identical(sum(vapply(rowwise$data, nrow, integer(1))), 4L)
      expect_identical(source, before)
    }
  }
})
