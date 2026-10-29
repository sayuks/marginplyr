test_that("local shares read signed and whitespace dot-dot sources", {
  input <- tibble::tibble(g = c("a", "b"), x = c(1, 3))
  before <- input
  for (column in c("..-1", "..-0", "..+1", "..+0", ".. 1")) {
    for (helper in c("share_of_parent", "share_of_total")) {
      share <- rlang::call2(helper, rlang::sym(column))
      actual <- summarize_with_margins(
        input, !!column := sum(x), fraction = !!share,
        .grouping = rollup(g), .sort = "last"
      )
      info <- paste(column, helper)
      expect_identical(names(actual), c("g", column, "fraction"), info = info)
      expect_identical(actual[[column]], c(1, 3, 4), info = info)
      expect_identical(actual$fraction, c(0.25, 0.75, 1), info = info)
      expect_identical(input, before)
    }
  }
})

test_that("local signed and whitespace fixed keys survive copies and shares", {
  input <- data.frame(
    fixed = rep(c("P", "Q", NA_character_), each = 2L),
    g = rep(c("a", "b"), 3L), x = c(1, 3, 2, 6, 3, 1)
  )
  for (column in c("..-1", "..-0", "..+1", "..+0", ".. 1")) {
    for (tibble in c(FALSE, TRUE)) {
      for (empty in c(FALSE, TRUE)) {
        data <- input[if (empty) integer() else seq_len(nrow(input)), ]
        names(data)[1L] <- column
        if (tibble) {
          data <- tibble::as_tibble(data)
        }
        before <- data
        for (sort in c("none", "first", "last")) {
          ordinary <- summarize_with_margins(
            data, value = sum(x), .by = dplyr::all_of(column),
            .grouping = rollup(g), .id = "set", .sort = sort
          )
          actual <- summarize_with_margins(
            data, value = sum(x),
            parent = share_of_parent(value), total = share_of_total(value),
            dplyr::across(value, share_of_parent, .names = "parent_across"),
            dplyr::across(value, share_of_total, .names = "total_across"),
            .by = dplyr::all_of(column), .grouping = rollup(g),
            .id = "set", .sort = sort
          )
          expected <- tibble::tibble(
            fixed = rep(c("P", "Q", NA_character_), each = 3L),
            g = rep(c("a", "b", "Total"), 3L),
            set = rep(c(1L, 1L, 2L), 3L),
            value = c(1, 3, 4, 2, 6, 8, 3, 1, 4),
            parent = c(0.25, 0.75, 1, 0.25, 0.75, 1, 0.75, 0.25, 1)
          )
          expected$total <- expected$parent
          expected$parent_across <- expected$parent
          expected$total_across <- expected$parent
          if (empty) {
            expected <- expected[integer(), ]
          } else if (identical(sort, "first")) {
            expected <- expected[c(3L, 1L, 2L, 6L, 4L, 5L, 9L, 7L, 8L), ]
          }
          names(expected)[1L] <- column
          if (identical(sort, "none")) {
            actual <- actual[order(actual[[column]], actual$set, actual$g), ]
            rows <- order(ordinary[[column]], ordinary$set, ordinary$g)
            ordinary <- ordinary[rows, ]
            rows <- order(expected[[column]], expected$set, expected$g)
            expected <- expected[rows, ]
          }
          info <- paste(column, tibble, empty, sort)
          expect_equal(tibble::as_tibble(actual), expected, info = info)
          expect_equal(tibble::as_tibble(ordinary), expected[1:4], info = info)
          expect_identical(names(actual), names(expected), info = info)
          expect_identical(actual[[column]], expected[[column]], info = info)
          expect_identical(actual$set, expected$set, info = info)
          for (share in c("parent", "total", "parent_across", "total_across")) {
            expect_type(actual[[share]], "double")
          }
          expect_identical(data, before)
        }
      }
    }
  }
})

test_that("local contextual across reads signed dot-dot source summaries", {
  input <- tibble::tibble(g = c("a", "b"), x = c(1, 3))
  for (column in c("..-1", "..-0", "..+1", "..+0", ".. 1")) {
    actual <- summarize_with_margins(
      input, dplyr::across(x, sum, .names = column),
      dplyr::across(dplyr::all_of(column), share_of_parent, .names = "parent"),
      dplyr::across(dplyr::all_of(column), share_of_total, .names = "total"),
      .grouping = rollup(g), .sort = "last"
    )
    expect_identical(names(actual), c("g", column, "parent", "total"))
    expect_identical(actual[[column]], c(1, 3, 4))
    expect_identical(actual$parent, c(0.25, 0.75, 1))
    expect_identical(actual$total, actual$parent)
  }
})

test_that("dtplyr shares read signed and whitespace dot-dot sources lazily", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr imports data.table; only the dtplyr backend is required.
  input <- data.table::data.table(g = c("a", "b"), x = c(1, 3))
  before <- data.table::copy(input)
  for (column in c("..-1", "..-0", "..+1", "..+0", ".. 1")) {
    for (helper in c("share_of_parent", "share_of_total")) {
      for (style in c("direct", "across")) {
        reads <- 0L
        summaries <- 0L
        predicate <- function(x) {
          reads <<- reads + length(x)
          x > 0
        }
        tally <- function(x) {
          summaries <<- summaries + 1L
          sum(x)
        }
        source <- dtplyr::lazy_dt(input, immutable = TRUE) |>
          dplyr::filter(predicate(x))
        share <- if (identical(style, "direct")) {
          rlang::exprs(fraction = !!rlang::call2(helper, rlang::sym(column)))
        } else {
          rlang::exprs(dplyr::across(
            dplyr::all_of(column), !!rlang::sym(helper), .names = "fraction"
          ))
        }
        query <- summarize_with_margins(
          source, !!column := tally(x), !!!share,
          .grouping = rollup(g), .sort = "last"
        )
        info <- paste(column, helper, style)
        expect_s3_class(query, "dtplyr_step")
        expect_identical(reads, 0L)
        expect_identical(summaries, 0L)
        expect_identical(input, before)
        actual <- dplyr::collect(query)
        expect_gt(reads, 0L)
        expect_gt(summaries, 0L)
        expect_identical(names(actual), c("g", column, "fraction"), info = info)
        expect_identical(actual[[column]], c(1, 3, 4), info = info)
        expect_identical(actual$fraction, c(0.25, 0.75, 1), info = info)
        expect_identical(input, before)
      }
    }
  }
})
