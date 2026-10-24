test_that("eager expansion preserves triple-dot suffixes on payload names", {
  data <- data.frame(
    group = c("a", "b"),
    sales = c(10L, 20L),
    sales...1 = c(1L, 2L),
    sales...2 = c(3L, 4L)
  )

  result <- expand_with_margins(data, .grouping = rollup(group))

  expect_identical(names(result), names(data))
  expect_identical(result[["group"]], c("a", "b", "Total", "Total"))
  expect_identical(result[["sales"]], c(10L, 20L, 10L, 20L))
  expect_identical(result[["sales...1"]], c(1L, 2L, 1L, 2L))
  expect_identical(result[["sales...2"]], c(3L, 4L, 3L, 4L))
})

# One fixture exercises suffixes in fixed keys, dimensions, payloads, and
# output names, including distinct columns sharing the same unsuffixed stem.
suffix_name_data <- function() {
  data.frame(
    account...2 = c("p", "p", "q"),
    region...1 = c("b", "a", "a"),
    sales = c(20L, 10L, 40L),
    sales...1 = c(2L, 1L, 4L),
    sales...2 = c(6L, 3L, 12L)
  )
}

# Check only names, values, and the grouping shape marginplyr constructs.
# Classes and arbitrary attributes stay under ADR 0016's delegation contract.
expect_suffix_margin_results <- function(data) {
  source <- suffix_name_data()
  keys <- c("account...2", "region...1", "set...3")
  payload <- c("sales", "sales...1", "sales...2")
  expected <- data.frame(
    account...2 = c("p", "p", "p", "q", "q"),
    region...1 = c("a", "b", "Total", "a", "Total"),
    set...3 = c(1L, 1L, 2L, 1L, 2L),
    sales = c(10L, 20L, 30L, 40L, 40L),
    sales...1 = c(1L, 2L, 3L, 4L, 4L),
    sales...2 = c(3L, 6L, 9L, 12L, 12L)
  )
  source_rows <- list(2L, 1L, 1:2, 3L, 3L)

  for (multiple in c(FALSE, TRUE)) {
    spec <- if (multiple) {
      rollup(dplyr::all_of("region...1"))
    } else {
      grouping_set(dplyr::all_of("region...1"))
    }
    for (sort in c("none", "first", "last")) {
      info <- paste("multiple:", multiple, "sort:", sort)
      rows <- if (!multiple) {
        c(1L, 2L, 4L)
      } else if (sort == "first") {
        c(3L, 1L, 2L, 5L, 4L)
      } else {
        seq_len(5L)
      }
      wanted <- expected[rows, , drop = FALSE]
      cells <- source_rows[rows]
      normalize <- function(result) {
        result <- dplyr::ungroup(dplyr::collect(result))
        if (sort == "none") {
          result <- dplyr::arrange(
            result, !!!rlang::syms(c("account...2", "set...3", "region...1"))
          )
        }
        result
      }
      expect_columns <- function(result, reference) {
        expect_identical(names(result), names(reference), info = info)
        for (column in names(reference)) {
          expect_identical(result[[column]], reference[[column]], info = info)
        }
      }

      summary <- normalize(summarize_with_margins(
        data,
        sales = sum(!!rlang::sym("sales")),
        sales...1 = sum(!!rlang::sym("sales...1")),
        sales...2 = sum(!!rlang::sym("sales...2")),
        .by = "account...2", .grouping = spec, .id = "set...3", .sort = sort
      ))
      expect_columns(summary, wanted)

      expanded <- normalize(expand_with_margins(
        data,
        .by = "account...2", .grouping = spec, .id = "set...3", .sort = sort
      ))
      expect_identical(names(expanded), c(keys, payload), info = info)
      # Rows within one expanded key have no promised order.
      for (i in seq_along(cells)) {
        selected <- expanded[[keys[[1L]]]] == wanted[[keys[[1L]]]][[i]] &
          expanded[[keys[[2L]]]] == wanted[[keys[[2L]]]][[i]] &
          expanded[[keys[[3L]]]] == wanted[[keys[[3L]]]][[i]]
        actual <- expanded[selected, , drop = FALSE] |>
          dplyr::arrange(!!rlang::sym("sales"))
        input <- source[cells[[i]], payload, drop = FALSE] |>
          dplyr::arrange(!!rlang::sym("sales"))
        for (column in payload) {
          expect_identical(actual[[column]], input[[column]], info = info)
        }
      }
      expected_keys <- wanted[rep(seq_len(nrow(wanted)), lengths(cells)), keys]
      for (column in keys) {
        expect_identical(
          expanded[[column]], expected_keys[[column]], info = info
        )
      }

      for (verb in list(nest_with_margins, nest_by_with_margins)) {
        for (keep in c(FALSE, TRUE)) {
          nested <- verb(
            data, .by = "account...2", .grouping = spec, .id = "set...3",
            .key = "data...4", .keep = keep, .sort = sort
          )
          if (inherits(nested, "rowwise_df")) {
            expect_identical(dplyr::group_vars(nested), keys, info = info)
          }
          nested <- normalize(nested)
          expect_identical(names(nested), c(keys, "data...4"), info = info)
          for (column in keys) {
            expect_identical(nested[[column]], wanted[[column]], info = info)
          }
          for (i in seq_along(cells)) {
            actual <- dplyr::arrange(
              nested[["data...4"]][[i]], !!rlang::sym("sales")
            )
            columns <- if (keep) names(source) else payload
            input <- source[cells[[i]], columns, drop = FALSE] |>
              dplyr::arrange(!!rlang::sym("sales"))
            expect_columns(actual, input)
          }
        }
      }
    }
  }
}

test_that("data frames retain suffix names across Margin verbs and sorting", {
  expect_suffix_margin_results(suffix_name_data())
})

test_that("tibbles retain suffix names across Margin verbs and sort modes", {
  expect_suffix_margin_results(dplyr::as_tibble(suffix_name_data()))
})

test_that("data tables retain suffix names across Margin verbs and sorting", {
  skip_if_suggest_absent("data.table")
  expect_suffix_margin_results(data.table::as.data.table(suffix_name_data()))
})

test_that("dtplyr retains suffix names across Margin verbs and sort modes", {
  skip_if_suggest_absent("dtplyr")
  expect_suffix_margin_results(dtplyr::lazy_dt(suffix_name_data()))
})
