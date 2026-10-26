test_that("dtplyr expansion gives omitted list dimensions missing elements", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(g = list(1L, 2L), value = 1:2)

  for (label in list(NULL, NA_character_)) {
    query <- expand_with_margins(
      dtplyr::lazy_dt(input), .grouping = rollup(g), .margin_label = label
    )
    expect_s3_class(query, "dtplyr_step")
    result <- dplyr::collect(query)
    result <- result[
      order(vapply(result$g, is.null, logical(1)), result$value),
    ]
    expect_identical(result$g, list(1L, 2L, NULL, NULL))
    expect_identical(result$value, c(1L, 2L, 1L, 2L))
  }
})

test_that("dtplyr list expansion retains empty and fixed-key row shapes", {
  skip_if_suggest_absent("dtplyr")
  for (rows in 0:2) {
    input <- tibble::tibble(
      fixed = c("a", "b")[seq_len(rows)],
      g = list(1L, 2L)[seq_len(rows)],
      value = seq_len(rows)
    )
    source <- data.table::as.data.table(input)
    before <- data.table::copy(source)
    for (label in list(NULL, NA_character_)) {
      for (id in list(NULL, "set")) {
        query <- expand_with_margins(
          dtplyr::lazy_dt(source), .by = fixed, .grouping = rollup(g),
          .margin_label = label, .id = id
        )
        expect_s3_class(query, "dtplyr_step")
        expect_identical(source, before)
        result <- dplyr::collect(query)
        expected <- tibble::tibble(
          fixed = rep(input$fixed, 2L),
          g = c(input$g, rep(list(NULL), rows)),
          value = rep(input$value, 2L)
        )
        if (!is.null(id)) {
          expected$set <- rep(1:2, each = rows)
          expected <- expected[c("fixed", "g", "set", "value")]
        }
        order_rows <- function(data) {
          data[order(vapply(data$g, is.null, logical(1)), data$value), ]
        }
        expect_identical(order_rows(result), order_rows(expected))
        expect_identical(source, before)
      }
    }
  }
})

test_that("ordinary dtplyr list expansion controls retain their values", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(g = list(1L, 2L), value = 1:2)
  source <- dtplyr::lazy_dt(input)
  control <- dplyr::union_all(source, dplyr::mutate(source, g = list(NULL)))
  expect_s3_class(control, "dtplyr_step")
  control <- dplyr::collect(control)
  control <- control[
    order(vapply(control$g, is.null, logical(1)), control$value),
  ]
  expect_identical(control$g, list(1L, 2L, NULL, NULL))
  expect_identical(control$value, c(1L, 2L, 1L, 2L))

  for (label in list(NULL, NA_character_)) {
    local <- expand_with_margins(
      input, .grouping = rollup(g), .margin_label = label
    )
    local <- local[order(vapply(local$g, is.null, logical(1)), local$value), ]
    expect_identical(local$g, list(1L, 2L, NULL, NULL))
    expect_identical(local$value, c(1L, 2L, 1L, 2L))
    included <- expand_with_margins(
      source, .grouping = grouping_set(g), .margin_label = label
    )
    expect_identical(dplyr::collect(included), input)
    payload <- expand_with_margins(
      source, .grouping = rollup(value), .margin_label = label, .id = "set"
    ) |>
      dplyr::collect()
    payload <- payload[
      order(payload$set, vapply(payload$g, identity, integer(1))),
    ]
    expect_identical(payload$g, list(1L, 2L, 1L, 2L))
    expect_identical(payload$value, c(1L, 2L, NA_integer_, NA_integer_))
  }
  labelled <- expand_with_margins(
    source, .grouping = rollup(g), .margin_label = "Total", .id = "set"
  ) |>
    dplyr::collect() |>
    dplyr::arrange(set, value)
  expect_identical(labelled$g, c("1", "2", "Total", "Total"))
  expect_identical(labelled$value, c(1L, 2L, 1L, 2L))
  expect_identical(dplyr::collect(source), input)
})

test_that("dtplyr list expansion constructs without executing source rows", {
  skip_if_suggest_absent("dtplyr")
  allow_rows <- FALSE
  read_value <- function(x) {
    if (!allow_rows && length(x) > 0L) stop("source rows executed")
    rep(TRUE, length(x))
  }
  source <- dtplyr::lazy_dt(tibble::tibble(g = list(1L, 2L), value = 1:2)) |>
    dplyr::filter(read_value(value))
  for (label in list(NULL, NA_character_)) {
    allow_rows <- FALSE
    query <- expand_with_margins(
      source, .grouping = rollup(g), .margin_label = label
    )
    expect_s3_class(query, "dtplyr_step")
    allow_rows <- TRUE
    result <- dplyr::collect(query)
    expect_identical(nrow(result), 4L)
  }
})

test_that("data.table list grouping refusals remain External conditions", {
  skip_if_suggest_absent("dtplyr")
  source <- dtplyr::lazy_dt(tibble::tibble(g = list(1L, 2L)))
  ordinary <- dplyr::summarise(source, n = dplyr::n(), .by = g)
  margins <- summarize_with_margins(
    source, n = dplyr::n(), .grouping = rollup(g), .margin_label = NULL
  )
  for (query in list(ordinary, margins)) {
    error <- expect_error(dplyr::collect(query), "type 'list'")
    expect_false(inherits(error, "marginplyr_error"))
  }
})
