# Build the caller's direct or across share expression with its requested name.
generated_share_expression <- function(helper, style, output) {
  if (style == "direct") {
    return(rlang::set_names(list(rlang::call2(helper, quote(value))), output))
  }
  rlang::exprs(dplyr::across(
    dplyr::all_of("value"), !!rlang::sym(helper), .names = !!output
  ))
}

test_that("dtplyr shares preserve .ROW outputs and identifiers", {
  skip_if_suggest_absent("dtplyr")
  input <- data.table::data.table(g = c("a", "b"), x = c(1, 3))
  before <- data.table::copy(input)
  source <- dtplyr::lazy_dt(input, immutable = TRUE) |>
    dplyr::filter(TRUE)
  for (role in c("share", "id", "without_id")) {
    output <- if (role == "id") "fraction" else ".ROW"
    identifier <- switch(role, share = "set", id = ".ROW", without_id = NULL)
    for (helper in c("share_of_parent", "share_of_total")) {
      for (style in c("direct", "across")) {
        share <- generated_share_expression(helper, style, output)
        query <- summarize_with_margins(
          source, value = sum(x), !!!share, .grouping = rollup(g),
          .id = identifier, .sort = "last"
        )
        expect_s3_class(query, "dtplyr_step")
        expect_identical(input, before)
        actual <- dplyr::collect(query)
        info <- paste(role, helper, style)
        expect_identical(names(actual), c("g", identifier, "value", output),
                         info = info)
        expect_identical(actual$g, c("a", "b", "Total"), info = info)
        if (!is.null(identifier)) {
          expect_identical(actual[[identifier]], c(1L, 1L, 2L), info = info)
        }
        expect_identical(actual$value, c(1, 3, 4), info = info)
        expect_identical(actual[[output]], c(0.25, 0.75, 1), info = info)
        expect_identical(input, before)
        expect_identical(dplyr::collect(query), actual)
      }
    }
  }
})

test_that(".ROW shares retain partitions, duplicate sets and Margin order", {
  skip_if_suggest_absent("dtplyr")
  fixture <- data.table::data.table(
    fixed = factor(rep(c("P", "Q"), each = 3L)),
    g = rep(c("a", "a", "b"), 2L), h = rep(c("u", "v", "u"), 2L),
    x = c(1, 3, 4, 2, 6, 8)
  )
  for (empty in c(FALSE, TRUE)) {
    rows <- if (empty) integer() else seq_len(nrow(fixture))
    input <- data.table::copy(fixture)[rows]
    before <- data.table::copy(input)
    source <- dtplyr::lazy_dt(input, immutable = TRUE) |> dplyr::filter(TRUE)
    local <- tibble::as_tibble(before)
    for (role in c("share", "id")) {
      output <- if (role == "share") ".ROW" else "fraction"
      identifier <- if (role == "id") ".ROW" else "set"
      for (helper in c("share_of_parent", "share_of_total")) {
        for (style in c("direct", "across")) {
          share <- generated_share_expression(helper, style, output)
          for (sort in c("none", "first", "last")) {
            implicit <- sort == "first"
            run <- function(data) {
              if (implicit) data <- dplyr::group_by(data, fixed)
              by <- if (implicit) NULL else rlang::sym("fixed")
              summarize_with_margins(
                data, value = sum(x),
                ..marginplyr_dtplyr_column_1 = sum(x),
                ..marginplyr_dtplyr_column_1_ = sum(x), !!!share,
                .by = !!by,
                .grouping = rollup(g, g, h), .duplicates = "keep",
                .id = identifier, .sort = sort
              )
            }
            query <- run(source)
            expect_s3_class(query, "dtplyr_step")
            expect_identical(input, before)
            actual <- dplyr::collect(query)
            expected <- tibble::as_tibble(run(local))
            info <- paste(empty, role, helper, style, sort)
            expect_identical(names(actual), c(
              "fixed", "g", "h", identifier, "value",
              "..marginplyr_dtplyr_column_1", "..marginplyr_dtplyr_column_1_",
              output
            ), info = info)
            expect_type(actual[[identifier]], "integer")
            expect_type(actual[[output]], "double")
            if (sort == "none") {
              actual <- dplyr::arrange(actual, fixed, g, h, .data[[identifier]])
              expected <- dplyr::arrange(
                expected, fixed, g, h, .data[[identifier]]
              )
            }
            expect_identical(actual, expected, info = info)
            expect_identical(input, before)
          }
        }
      }
    }
  }
})

test_that(".ROW identifiers retain the no-share control", {
  skip_if_suggest_absent("dtplyr")
  source <- dtplyr::lazy_dt(tibble::tibble(g = c("a", "b"), x = c(1, 3)))
  query <- summarize_with_margins(
    source, value = sum(x), .grouping = rollup(g), .id = ".ROW", .sort = "last"
  )
  expect_identical(dplyr::collect(query), tibble::tibble(
    g = c("a", "b", "Total"), .ROW = c(1L, 1L, 2L), value = c(1, 3, 4)
  ))
})

test_that(".ROW shares defer source and summary evaluation", {
  skip_if_suggest_absent("dtplyr")
  reads <- 0L
  summaries <- 0L
  visit <- function(x) {
    reads <<- reads + length(x)
    rep(TRUE, length(x))
  }
  measure <- function(x) {
    summaries <<- summaries + 1L
    sum(x)
  }
  input <- data.table::data.table(g = c("a", "b"), x = c(1, 3))
  before <- data.table::copy(input)
  source <- dtplyr::lazy_dt(input, immutable = TRUE) |> dplyr::filter(visit(x))
  for (role in c("share", "id")) {
    output <- if (role == "share") ".ROW" else "fraction"
    identifier <- if (role == "id") ".ROW" else "set"
    reads <- 0L
    summaries <- 0L
    query <- summarize_with_margins(
      source, value = measure(x), !!output := share_of_parent(value),
      total = share_of_total(value), .grouping = rollup(g), .id = identifier,
      .sort = "last"
    )
    expect_identical(reads, 0L)
    expect_identical(summaries, 0L)
    expect_identical(input, before)
    result <- dplyr::collect(query)
    expect_identical(result[[output]], c(0.25, 0.75, 1))
    expect_identical(result$total, c(0.25, 0.75, 1))
    expect_identical(result[[identifier]], c(1L, 1L, 2L))
    expect_gt(reads, 0L)
    expect_gt(summaries, 0L)
    expect_identical(input, before)
  }
})

test_that(".ROW Total shares preserve a lone Grand total set", {
  skip_if_suggest_absent("dtplyr")
  for (size in 0:1) {
    input <- tibble::tibble(x = rep(2, size))
    for (identifier in list(NULL, ".ROW")) {
      output <- if (is.null(identifier)) ".ROW" else "fraction"
      query <- summarize_with_margins(
        dtplyr::lazy_dt(input), value = sum(x),
        !!output := share_of_total(value), .grouping = grouping_set(),
        .id = identifier
      )
      actual <- dplyr::collect(query)
      expect_identical(names(actual), c(identifier, "value", output))
      expect_identical(actual[[output]], 1)
      expect_identical(actual$value, if (size == 0L) 0 else 2)
      if (!is.null(identifier)) expect_identical(actual[[identifier]], 1L)
    }
  }
})
