test_that("dtplyr dot fixed shares preserve partitions and Margin order", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr imports data.table; only the dtplyr backend is required.
  input <- data.table::data.table(
    fixed = rep(c("P", "Q", NA_character_), each = 4L),
    g = rep(c("a", "a", "b", "b"), 3L),
    h = rep(c("x", "y"), 6L), x = c(1, 3, 2, 6, 3, 1, 4, 8, 2, 2, 1, 3)
  )
  for (column in c(".", "..0", "..00", "..01", "..-1", "..-0", "..+1",
                   "..+0", ".. 1")) {
    for (empty in c(FALSE, TRUE)) {
      rows <- if (empty) integer() else seq_len(nrow(input))
      table <- data.table::copy(input)[rows]
      data.table::setnames(table, "fixed", column)
      before <- data.table::copy(table)
      source <- dtplyr::lazy_dt(table, immutable = TRUE)
      for (implicit in c(FALSE, TRUE)) {
        for (sort in c("none", "first", "last")) {
          data <- source
          if (implicit) {
            if (identical(column, ".")) {
              # dtplyr accepts a grouped root; its group_by(.) cannot build one.
              grouped <- dplyr::group_by(
                tibble::as_tibble(table), !!!rlang::syms(column)
              )
              data <- dtplyr::lazy_dt(grouped, immutable = TRUE)
            } else {
              data <- dplyr::group_by(
                data, dplyr::across(dplyr::all_of(column))
              )
            }
          }
          by <- if (implicit) {
            rlang::quo(NULL)
          } else {
            rlang::quo(dplyr::all_of(column))
          }
          query <- summarize_with_margins(
            data, value = sum(x), level = grouping_id(g, h),
            parent = share_of_parent(value), total = share_of_total(value),
            dplyr::across(value, share_of_parent, .names = "parent_across"),
            dplyr::across(value, share_of_total, .names = "total_across"),
            .by = !!by, .grouping = rollup(g, h), .id = "set", .sort = sort
          )
          info <- paste(column, empty, implicit, sort)
          expect_s3_class(query, "dtplyr_step")
          expect_identical(table, before, info = info)
          actual <- dplyr::collect(query)
          expect_identical(names(actual), c(
            column, "g", "h", "set", "value", "level", "parent", "total",
            "parent_across", "total_across"
          ), info = info)
          expected <- tibble::tibble(
            fixed = rep(c("P", "Q", NA_character_), each = 7L),
            g = rep(c("a", "a", "b", "b", "a", "b", "Total"), 3L),
            h = rep(c("x", "y", "x", "y", "Total", "Total", "Total"), 3L),
            set = rep(c(1L, 1L, 1L, 1L, 2L, 2L, 3L), 3L),
            value = c(1, 3, 2, 6, 4, 8, 12, 3, 1, 4, 8, 4, 12, 16,
                      2, 2, 1, 3, 4, 4, 8),
            level = rep(c(0L, 0L, 0L, 0L, 1L, 1L, 3L), 3L),
            parent = c(1 / 4, 3 / 4, 1 / 4, 3 / 4, 1 / 3, 2 / 3, 1,
                       3 / 4, 1 / 4, 1 / 3, 2 / 3, 1 / 4, 3 / 4, 1,
                       1 / 2, 1 / 2, 1 / 4, 3 / 4, 1 / 2, 1 / 2, 1),
            total = c(1 / 12, 3 / 12, 2 / 12, 6 / 12, 4 / 12, 8 / 12, 1,
                      3 / 16, 1 / 16, 4 / 16, 8 / 16, 4 / 16, 12 / 16, 1,
                      2 / 8, 2 / 8, 1 / 8, 3 / 8, 4 / 8, 4 / 8, 1)
          )
          expected$parent_across <- expected$parent
          expected$total_across <- expected$total
          if (empty) {
            expected <- expected[integer(), ]
          } else if (!identical(sort, "none")) {
            within <- if (identical(sort, "first")) {
              c(7L, 5L, 1L, 2L, 6L, 3L, 4L)
            } else {
              c(1L, 2L, 5L, 3L, 4L, 6L, 7L)
            }
            expected <- expected[c(within, within + 7L, within + 14L), ]
          }
          names(expected)[1L] <- column
          if (identical(sort, "none")) {
            # Unordered results promise rows, not the backend's group order.
            rows <- order(actual[[column]], actual$set, actual$g, actual$h)
            actual <- actual[rows, ]
            rows <- order(
              expected[[column]], expected$set, expected$g, expected$h
            )
            expected <- expected[rows, ]
          }
          expect_equal(actual, expected, info = info)
          expect_identical(actual[[column]], expected[[column]], info = info)
          expect_type(actual$set, "integer")
          for (share in c("parent", "total", "parent_across", "total_across")) {
            expect_type(actual[[share]], "double")
          }
          expect_identical(table, before, info = info)
        }
      }
    }
  }
})

test_that("dtplyr dot fixed shares defer reads and summary evaluation", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr imports data.table; only the dtplyr backend is required.
  for (column in c(".", "..0", "..00", "..01")) {
    input <- data.table::data.table(
      fixed = c("P", "P"), g = c("a", "b"), x = c(1, 3)
    )
    data.table::setnames(input, "fixed", column)
    before <- data.table::copy(input)
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
    query <- summarize_with_margins(
      source, value = tally(x),
      parent = share_of_parent(value), total = share_of_total(value),
      .by = dplyr::all_of(column), .grouping = rollup(g), .sort = "last"
    )
    expect_s3_class(query, "dtplyr_step")
    expect_identical(reads, 0L)
    expect_identical(summaries, 0L)
    expect_identical(input, before)
    actual <- dplyr::collect(query)
    expect_gt(reads, 0L)
    expect_gt(summaries, 0L)
    expect_identical(actual$parent, c(0.25, 0.75, 1))
    expect_identical(actual$total, actual$parent)
    expect_identical(input, before)
  }
})

test_that("dtplyr dot fixed keys still refuse expanded grouped share results", {
  skip_if_suggest_absent("dtplyr")
  for (column in c(".", "..0", "..00", "..01", "..-1", ".. 1")) {
    input <- tibble::tibble(fixed = c("P", "P"), g = c("a", "b"), x = c(1, 3))
    names(input)[1L] <- column
    for (helper in c("share_of_parent", "share_of_total")) {
      query <- summarize_with_margins(
        dtplyr::lazy_dt(input), `..+0` = sum(x), extra = 1:2,
        fraction = !!rlang::call2(helper, rlang::sym("..+0")),
        .by = dplyr::all_of(column), .grouping = rollup(g)
      )
      expect_s3_class(query, "dtplyr_step")
      error <- expect_error(
        dplyr::collect(query), "expanded the grouped result",
        class = "marginplyr_share_cardinality_error"
      )
      expect_identical(error$share_output, "fraction")
      expect_identical(rlang::call_name(conditionCall(error)),
                       "summarize_with_margins")
    }
  }
})
