test_that("dtplyr shares distinguish an NA factor level from a missing code", {
  skip_if_suggest_absent("dtplyr")
  dimension <- structure(c(2L, NA_integer_),
                         levels = c("unused", NA_character_), class = "factor")
  data <- tibble::tibble(g = dimension, value = c(2, 3))
  for (ordered in c(FALSE, TRUE)) {
    class(data$g) <- if (ordered) c("ordered", "factor") else "factor"
    for (label in list(NULL, "All")) {
      for (kind in c("parent", "total", "both")) {
        expressions <- list(total = rlang::expr(sum(value)))
        if (kind %in% c("parent", "both")) {
          expressions$p <- rlang::expr(share_of_parent(total))
        }
        if (kind %in% c("total", "both")) {
          expressions$t <- rlang::expr(share_of_total(total))
        }
        local <- summarize_with_margins(
          data, !!!expressions, .grouping = rollup(g), .id = "set",
          .margin_label = label
        )
        query <- summarize_with_margins(
          dtplyr::lazy_dt(data), !!!expressions,
          .grouping = rollup(g), .id = "set", .margin_label = label
        )
        expect_s3_class(query, "dtplyr_step")
        result <- dplyr::arrange(dplyr::collect(query), .data$set, .data$total)
        local <- dplyr::arrange(local, .data$set, .data$total)
        expect_equal(result, local)
        expect_identical(as.integer(result$g[1:2]), c(2L, NA_integer_))
        expect_identical(levels(result$g), c(levels(dimension), label))
        expect_identical(is.ordered(result$g), ordered)
        expect_equal(result$total, c(2, 3, 5))
        for (share in intersect(c("p", "t"), names(result))) {
          expect_equal(result[[share]], c(0.4, 0.6, 1))
        }
      }
    }
  }
})

test_that("dtplyr Parent shares match distinct NA factor parents", {
  skip_if_suggest_absent("dtplyr")
  dimension <- structure(c(2L, 2L, NA_integer_, NA_integer_),
                         levels = c("unused", NA_character_),
                         class = c("ordered", "factor"))
  data <- tibble::tibble(g = dimension, h = c("x", "y", "x", "y"),
                         value = c(2, 6, 3, 9))
  for (label in list(NULL, "All")) {
    for (position in c("first", "last")) {
      report <- function(source) {
        summarize_with_margins(
          source, total = sum(value), p = share_of_parent(total),
          t = share_of_total(total), .grouping = rollup(g, h), .id = "set",
          .margin_label = label, .margin_label_position = position
        )
      }
      local <- dplyr::arrange(report(data), .data$set, .data$total)
      query <- report(dtplyr::lazy_dt(data))
      expect_s3_class(query, "dtplyr_step")
      result <- dplyr::arrange(dplyr::collect(query), .data$set, .data$total)
      expect_equal(result, local)
      expect_equal(result$total, c(2, 3, 6, 9, 8, 12, 20))
      expect_equal(result$p, c(0.25, 0.25, 0.75, 0.75, 0.4, 0.6, 1))
      expect_equal(result$t, c(0.1, 0.15, 0.3, 0.45, 0.4, 0.6, 1))
      expect_identical(result$g, local$g)
    }
  }
})

test_that("dtplyr NA factor shares evaluate summaries only on collection", {
  skip_if_suggest_absent("dtplyr")
  dimension <- structure(c(1L, NA_integer_), levels = NA_character_,
                         class = "factor")
  data <- tibble::tibble(g = dimension, value = c(2, 3))
  calls <- new.env(parent = emptyenv())
  calls$count <- 0L
  measured_sum <- function(x) {
    calls$count <- calls$count + 1L
    sum(x)
  }
  query <- summarize_with_margins(
    dtplyr::lazy_dt(data), total = measured_sum(value),
    p = share_of_parent(total), t = share_of_total(total),
    .grouping = rollup(g), .margin_label = NULL
  )
  expect_s3_class(query, "dtplyr_step")
  expect_identical(calls$count, 0L)
  result <- dplyr::arrange(dplyr::collect(query), .data$total)
  expect_gt(calls$count, 0L)
  expect_equal(result$p, c(0.4, 0.6, 1))
  expect_equal(result$t, c(0.4, 0.6, 1))
})

test_that("dtplyr NA factor shares still refuse expanded ordinary summaries", {
  skip_if_suggest_absent("dtplyr")
  dimension <- structure(c(1L, NA_integer_), levels = NA_character_,
                         class = "factor")
  source <- dtplyr::lazy_dt(tibble::tibble(g = dimension, value = c(2, 3)))
  for (share in list(rlang::expr(share_of_parent(total)),
                     rlang::expr(share_of_total(total)))) {
    query <- summarize_with_margins(
      source, total = sum(value), s = !!share, extra = 1:2,
      .grouping = rollup(g), .margin_label = NULL
    )
    expect_s3_class(query, "dtplyr_step")
    error <- expect_error(dplyr::collect(query),
                          class = "marginplyr_share_cardinality_error")
    expect_match(conditionMessage(error), "expanded the grouped result")
  }
})
