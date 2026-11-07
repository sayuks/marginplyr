test_that("dtplyr shares preserve dot-dot names at data-frame conversion", {
  skip_if_suggest_absent("dtplyr")
  input <- dtplyr::lazy_dt(data.frame(g = c("a", "b"), x = c(1, 3)))
  query <- summarize_with_margins(
    input, `..1` = sum(x), p = share_of_total(`..1`),
    .grouping = rollup(g), .id = "set", .sort = "last"
  )
  expect_s3_class(query, "dtplyr_step")
  actual <- as.data.frame(query)
  expect_identical(names(actual), c("g", "set", "..1", "p"))
  expect_identical(actual[["..1"]], c(1, 3, 4))
  expect_identical(actual$p, c(0.25, 0.75, 1))
})

test_that("dtplyr shares retain dot-dot outputs across Margin options", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr imports data.table; only the dtplyr backend is required.
  for (fixed in c(FALSE, TRUE)) {
    fixture <- if (fixed) {
      data.table::data.table(
        f = c(2L, 2L, 1L, 1L), g = rep(c("a", "b"), 2L),
        x = c(1L, 3L, 3L, 1L)
      )
    } else {
      data.table::data.table(g = c("a", "b"), x = c(1L, 3L))
    }
    for (empty in c(FALSE, TRUE)) {
      input <- fixture[if (empty) integer() else seq_len(nrow(fixture))]
      before <- data.table::copy(input)
      source <- dtplyr::lazy_dt(input, immutable = TRUE)
      for (sort in c("none", "first", "last")) {
        expected <- if (fixed) {
          data.frame(
            f = rep(1:2, each = 3L), g = rep(c("a", "b", "Total"), 2L),
            set = rep(c(1L, 1L, 2L), 2L),
            value = c(3L, 1L, 4L, 1L, 3L, 4L),
            fraction = c(0.75, 0.25, 1, 0.25, 0.75, 1),
            other = c(3L, 1L, 4L, 1L, 3L, 4L)
          )
        } else {
          data.frame(
            g = c("a", "b", "Total"), set = c(1L, 1L, 2L),
            value = c(1L, 3L, 4L), fraction = c(0.25, 0.75, 1),
            other = c(1L, 3L, 4L)
          )
        }
        if (empty) {
          expected <- if (fixed) {
            expected[integer(), ]
          } else {
            data.frame(g = "Total", set = 2L, value = 0L,
                       fraction = 1, other = 0L)
          }
        } else if (sort == "first") {
          expected <- expected[if (fixed) c(3L, 1L, 2L, 6L, 4L, 5L)
                               else c(3L, 1L, 2L), ]
        }
        rownames(expected) <- NULL
        for (role in c("source", "ordinary", "share")) {
          for (name in c("...", "..1", "..42")) {
            value <- if (role == "source") name else "value"
            output <- if (role == "share") name else "fraction"
            other <- if (role == "ordinary") name else "other"
            for (helper in c("share_of_parent", "share_of_total")) {
              for (style in c("direct", "across")) {
                share <- if (style == "direct") {
                  rlang::set_names(list(
                    rlang::call2(helper, rlang::sym(value))
                  ), output)
                } else {
                  rlang::exprs(dplyr::across(
                    dplyr::all_of(!!value), !!rlang::sym(helper),
                    .names = !!output
                  ))
                }
                query <- summarize_with_margins(
                  source, !!value := sum(x), !!!share, !!other := sum(x),
                  .by = if (fixed) dplyr::all_of("f"),
                  .grouping = rollup(g), .id = "set", .sort = sort
                )
                expect_s3_class(query, "dtplyr_step")
                expect_identical(input, before)
                actual <- as.data.frame(query)
                control <- expected
                names(control) <- c(if (fixed) "f", "g", "set",
                                    value, output, other)
                if (sort == "none") {
                  ordering <- function(data) {
                    data[order(if (fixed) data$f else rep(0L, nrow(data)),
                               data$set, data$g), , drop = FALSE]
                  }
                  actual <- ordering(actual)
                  control <- ordering(control)
                }
                rownames(actual) <- rownames(control) <- NULL
                info <- paste(fixed, empty, sort, role, name, helper, style)
                expect_identical(actual, control, info = info)
                expect_type(actual$set, "integer")
                expect_type(actual[[output]], "double")
                expect_identical(input, before)
              }
            }
          }
        }
      }
    }
  }
})

test_that("dot-dot shares retain distinct Parent and Total denominators", {
  skip_if_suggest_absent("dtplyr")
  input <- dtplyr::lazy_dt(data.frame(
    g = c("a", "a", "b"), h = c("u", "v", "u"), x = c(1L, 3L, 4L)
  ))
  query <- summarize_with_margins(
    input, `..1` = sum(x), `..2` = share_of_parent(`..1`),
    dplyr::across(dplyr::all_of("..1"), share_of_total, .names = "..42"),
    .grouping = rollup(g, h), .id = "set", .sort = "last"
  )
  actual <- as.data.frame(query)
  expect_identical(names(actual), c("g", "h", "set", "..1", "..2", "..42"))
  expect_identical(actual[["..1"]], c(1L, 3L, 4L, 4L, 4L, 8L))
  expect_identical(actual[["..2"]], c(0.25, 0.75, 0.5, 1, 0.5, 1))
  expect_identical(actual[["..42"]], c(0.125, 0.375, 0.5, 0.5, 0.5, 1))
})

test_that("dtplyr dot-dot shares defer evaluation and preserve the source", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr imports data.table; only the dtplyr backend is required.
  input <- data.table::data.table(g = c("a", "b"), x = c(1L, 3L))
  before <- data.table::copy(input)
  reads <- summaries <- 0L
  visit <- function(x) {
    reads <<- reads + length(x)
    rep(TRUE, length(x))
  }
  measure <- function(x) {
    summaries <<- summaries + 1L
    sum(x)
  }
  source <- dtplyr::lazy_dt(input, immutable = TRUE) |> dplyr::filter(visit(x))
  query <- summarize_with_margins(
    source, `..1` = measure(x), `..2` = share_of_parent(`..1`),
    dplyr::across(dplyr::all_of("..1"), share_of_total, .names = "..42"),
    .grouping = rollup(g), .id = "set", .sort = "last"
  )
  expect_s3_class(query, "dtplyr_step")
  expect_identical(reads, 0L)
  expect_identical(summaries, 0L)
  expect_identical(input, before)
  actual <- as.data.frame(query)
  expect_gt(reads, 0L)
  expect_gt(summaries, 0L)
  expect_identical(actual[["..1"]], c(1L, 3L, 4L))
  expect_identical(actual[["..2"]], c(0.25, 0.75, 1))
  expect_identical(actual[["..42"]], c(0.25, 0.75, 1))
  expect_identical(input, before)
})

test_that("dtplyr dot-dot share support preserves genuine refusals", {
  skip_if_suggest_absent("dtplyr")
  input <- dtplyr::lazy_dt(data.frame(g = c("a", "b"), x = c(1L, 3L)))
  for (helper in c("share_of_parent", "share_of_total")) {
    expect_error(summarize_with_margins(
      input, `..1` = sum(x), p = !!rlang::call2(helper, quote(missing)),
      .grouping = rollup(g)
    ), "unknown preceding ordinary summary", class = "marginplyr_error")
    expect_error(summarize_with_margins(
      input, value = sum(x), `..1` = !!rlang::call2(helper, quote(value)),
      .grouping = rollup(g), .id = "..1"
    ), "conflicts", class = "marginplyr_error")
    expect_error(summarize_with_margins(
      input, `..1` = sum(x),
      dplyr::across(dplyr::all_of("..1"), !!rlang::sym(helper), .names = "..1"),
      .grouping = rollup(g)
    ), "conflicts", class = "marginplyr_error")
    for (style in c("direct", "across")) {
      share <- if (style == "direct") {
        rlang::exprs(p = !!rlang::call2(helper, rlang::sym("..1")))
      } else {
        rlang::exprs(dplyr::across(
          dplyr::all_of("..1"), !!rlang::sym(helper), .names = "p"
        ))
      }
      for (value in list(quote(numeric()), quote(c(min(x), max(x))))) {
        query <- summarize_with_margins(
          input, `..1` = !!value, !!!share, .grouping = rollup(g)
        )
        expect_s3_class(query, "dtplyr_step")
        expect_error(as.data.frame(query),
                     class = "marginplyr_share_cardinality_error")
      }
    }
  }
  invalid <- data.frame(g = "a", value = 1L)
  names(invalid)[2L] <- "..1"
  expect_error(summarize_with_margins(
    dtplyr::lazy_dt(invalid), value = dplyr::n(), p = share_of_total(value),
    .grouping = rollup(g)
  ), class = "vctrs_error_names_cannot_be_dot_dot")
})

test_that("dtplyr dot-dot shares leave default tibble name repair unchanged", {
  skip_if_suggest_absent("dtplyr")
  input <- dtplyr::lazy_dt(data.frame(g = c("a", "b"), x = c(1L, 3L)))
  ordinary <- dplyr::summarize(input, `..1` = sum(x))
  plain <- summarize_with_margins(
    input, `..1` = sum(x), .grouping = rollup(g), .sort = "last"
  )
  shares <- summarize_with_margins(
    input, `..1` = sum(x), p = share_of_total(`..1`),
    .grouping = rollup(g), .sort = "last"
  )
  expect_identical(as.data.frame(ordinary)[["..1"]], 4L)
  expect_identical(as.data.frame(plain)[["..1"]], c(1L, 3L, 4L))
  expect_identical(as.data.frame(shares)[["..1"]], c(1L, 3L, 4L))
  for (query in list(ordinary, plain, shares)) {
    expect_error(dplyr::collect(query),
                 class = "vctrs_error_names_cannot_be_dot_dot")
  }
})
