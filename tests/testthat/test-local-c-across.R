c_across_data <- function() {
  tibble::tibble(
    g = c(100, 100, 200, 200, 300),
    v = c(1, 2, 3, 4, 10),
    w = c(1, 0, 2, 5, 2)
  )
}

# Each grain's native grouped summary, over the input with every plan key the
# grain does not group by removed, keyed by the grain's keys.
native_c_across_grain <- function(data, keys, plan, summary) {
  data <- data[setdiff(names(data), setdiff(plan, keys))]
  out <- dplyr::summarise(
    dplyr::group_by(data, dplyr::across(tidyselect::all_of(keys))),
    !!!summary,
    .groups = "drop"
  )
  as.data.frame(out)
}

test_that("c_across() selects neither a key nor its private copy", {
  data <- c_across_data()
  actual <- summarize_with_margins(
    data,
    u = sum(dplyr::c_across(tidyselect::everything())),
    .grouping = rollup(g),
    .margin_label = NULL
  )
  expect_identical(actual$u, c(4, 14, 12, 30))
})

test_that("c_across() gives each grain's native grouped summary", {
  # `c_across()` with no `cols` is deprecated in native dplyr as well.
  withr::local_options(lifecycle_verbosity = "quiet")
  base <- dplyr::mutate(c_across_data(), h = c("a", "b", "a", "a", "b"))
  summaries <- rlang::exprs(
    everything = sum(dplyr::c_across(tidyselect::everything())),
    default = sum(dplyr::c_across()),
    negative = sum(dplyr::c_across(-w)),
    where = sum(dplyr::c_across(tidyselect::where(is.numeric)))
  )
  inputs <- list(
    data_frame = as.data.frame(base),
    tibble = base,
    grouped = dplyr::group_by(c_across_data(), g)
  )
  if (suggest_available("data.table")) {
    inputs$data_table <- data.table::as.data.table(base)
  }
  for (input in names(inputs)) {
    for (form in names(summaries)) {
      summary <- rlang::list2(u = summaries[[form]])
      info <- paste(input, form)
      if (identical(input, "grouped")) {
        actual <- summarize_with_margins(inputs[[input]], !!!summary)
        expected <- native_c_across_grain(c_across_data(), "g", "g", summary)
        expect_identical(as.data.frame(actual), expected, info = info)
        next
      }
      by <- summarize_with_margins(
        inputs[[input]], !!!summary, .by = c(g, h)
      )
      expect_identical(
        as.data.frame(by),
        native_c_across_grain(base, c("g", "h"), c("g", "h"), summary),
        info = info
      )
      actual <- as.data.frame(summarize_with_margins(
        inputs[[input]], !!!summary,
        .by = h, .grouping = rollup(g), .margin_label = NULL
      ))
      for (keys in list(c("h", "g"), "h")) {
        rows <- if (identical(keys, "h")) is.na(actual$g) else !is.na(actual$g)
        expected <- native_c_across_grain(base, keys, c("g", "h"), summary)
        observed <- actual[rows, c(keys, "u"), drop = FALSE]
        rownames(observed) <- NULL
        expected <- expected[match(
          do.call(paste, observed[keys]), do.call(paste, expected[keys])
        ), , drop = FALSE]
        rownames(expected) <- NULL
        expect_identical(observed, expected, info = paste(info, keys[[1L]]))
      }
    }
  }
})

test_that("shares over c_across() divide the corrected values", {
  data <- c_across_data()
  actual <- summarize_with_margins(
    data,
    u = sum(dplyr::c_across(tidyselect::everything())),
    total = share_of_total(u),
    parent = share_of_parent(u),
    .grouping = rollup(g),
    .margin_label = NULL
  )
  expect_identical(actual$u, c(4, 14, 12, 30))
  expect_equal(actual$total, c(4, 14, 12, 30) / 30)
  expect_equal(actual$parent, c(4, 14, 12, 30) / 30)
})

test_that("c_across() names no private column", {
  data <- dplyr::mutate(c_across_data(), g = as.character(g))
  expect_identical(
    summarize_with_margins(
      data, u = sum(dplyr::c_across(tidyselect::everything())),
      .grouping = rollup(g), .margin_label = NULL
    )$u,
    c(4, 14, 12, 30)
  )
  after_share <- summarize_with_margins(
    c_across_data(),
    s = sum(v),
    t = share_of_total(s),
    u = length(dplyr::c_across(tidyselect::everything())),
    .grouping = rollup(g),
    .margin_label = NULL
  )
  # `v` and `w` for each row of the grain and the preceding `s`; a share is
  # not selectable, as for `pick()`.
  expect_identical(after_share$u, c(5L, 5L, 3L, 11L))
  cnd <- rlang::catch_cnd(summarize_with_margins(
    data, u = sum(dplyr::c_across(-g)), .grouping = rollup(g)
  ))
  expect_s3_class(cnd, "error")
  expect_no_match(conditionMessage(cnd), "..marginplyr_", fixed = TRUE)
})

test_that("c_across() refuses a plan key as pick() does", {
  data <- c_across_data()
  for (selection in rlang::exprs(c(g, v), g)) {
    for (helper in c("c_across", "pick")) {
      summary <- rlang::list2(
        u = rlang::call2("sum", rlang::call2(helper, selection, .ns = "dplyr"))
      )
      for (call in list(
        rlang::expr(summarize_with_margins(data, !!!summary, .by = g)),
        rlang::expr(summarize_with_margins(
          data, !!!summary, .grouping = rollup(g)
        ))
      )) {
        expect_error(
          rlang::eval_tidy(call),
          "Column `g` doesn't exist",
          fixed = TRUE,
          info = paste(helper, deparse(selection))
        )
      }
    }
  }
})
