# A wrapper forwarding its dots into an unnamed top-level `across()` reaches
# dplyr's per-group evaluation whenever dplyr declines to expand the call: a
# non-FALSE `.unpack`, a `...` extra, or parentheses around the call. There
# `across()` captures the forwarded selection as a promise, which the Margin
# summary has to capture the same way (#1618).

forwarded_across_fixture <- function() {
  tibble::tibble(
    g = c("a", "a", "b", "b"), v = c(1, 2, 3, 4), w = c(4, 5, 6, 7)
  )
}

forwarded_across_range <- function(x) {
  tibble::tibble(lo = min(x), hi = max(x))
}

# Native and Margin wrappers over one plan, so a case is one forwarded call.
forwarded_across_wrappers <- function(parenthesized) {
  if (parenthesized) {
    list(
      native = function(data, ...) {
        dplyr::summarise(data, (dplyr::across(...)), .by = "g")
      },
      native_total = function(data, ...) {
        dplyr::summarise(data, (dplyr::across(...)))
      },
      by = function(data, ...) {
        summarize_with_margins(data, (dplyr::across(...)), .by = "g")
      },
      rollup = function(data, ...) {
        summarise_with_margins(
          data, (dplyr::across(...)), .grouping = rollup("g")
        )
      }
    )
  } else {
    list(
      native = function(data, ...) {
        dplyr::summarise(data, dplyr::across(...), .by = "g")
      },
      native_total = function(data, ...) {
        dplyr::summarise(data, dplyr::across(...))
      },
      by = function(data, ...) {
        summarize_with_margins(data, dplyr::across(...), .by = "g")
      },
      rollup = function(data, ...) {
        summarise_with_margins(
          data, dplyr::across(...), .grouping = rollup("g")
        )
      }
    )
  }
}

test_that("forwarded selections agree with dplyr where it runs per group", {
  withr::local_options(lifecycle_verbosity = "quiet")
  cols <- c("v", "w")
  selections <- rlang::exprs(
    v, c(v, w), "v", c("v", "w"), dplyr::all_of(cols),
    dplyr::where(is.numeric), dplyr::everything(), dplyr::starts_with("v")
  )
  dynamic <- TRUE
  shapes <- list(
    unpack = list(
      parenthesized = FALSE,
      args = rlang::exprs(forwarded_across_range, .unpack = TRUE)
    ),
    unpack_glue = list(
      parenthesized = FALSE,
      args = rlang::exprs(forwarded_across_range, .unpack = "{outer}.{inner}")
    ),
    unpack_dynamic = list(
      parenthesized = FALSE,
      args = rlang::exprs(forwarded_across_range, .unpack = dynamic)
    ),
    extra = list(
      parenthesized = FALSE, args = rlang::exprs(mean, na.rm = TRUE)
    ),
    parenthesized = list(parenthesized = TRUE, args = rlang::exprs(mean))
  )
  inputs <- list(tibble = forwarded_across_fixture())
  if (suggest_available("data.table")) {
    inputs$data_table <- data.table::as.data.table(inputs$tibble)
  }
  for (shape in names(shapes)) {
    wrappers <- forwarded_across_wrappers(shapes[[shape]]$parenthesized)
    for (selection in selections) {
      for (input in names(inputs)) {
        data <- inputs[[input]]
        info <- paste(shape, rlang::as_label(selection), input)
        call_with <- function(wrapper, data = inputs[[input]]) {
          rlang::eval_tidy(rlang::expr(
            (!!wrapper)(data, !!selection, !!!shapes[[shape]]$args)
          ))
        }
        detail <- call_with(wrappers$native)
        expect_equal(
          as.data.frame(call_with(wrappers$by)), as.data.frame(detail),
          info = info
        )
        # The Total row's selection excludes the key it collapses, as the
        # grouped detail's does.
        total <- call_with(wrappers$native_total, dplyr::select(data, -"g"))
        expected <- dplyr::bind_rows(
          tibble::as_tibble(detail),
          dplyr::mutate(tibble::as_tibble(total), g = "Total")
        )
        expect_equal(
          as.data.frame(call_with(wrappers$rollup)), as.data.frame(expected),
          info = info
        )
      }
    }
  }
})

test_that("a forwarded per-group selection still cannot reach a key", {
  wrappers <- forwarded_across_wrappers(FALSE)
  data <- forwarded_across_fixture()
  for (wrapper in wrappers[c("native", "by", "rollup")]) {
    expect_error(
      wrapper(data, c(g, v), forwarded_across_range, .unpack = TRUE),
      "doesn't exist"
    )
  }
})

test_that("a forwarded selection dplyr expands is refused as dplyr does", {
  wrappers <- forwarded_across_wrappers(FALSE)
  data <- forwarded_across_fixture()
  dynamic <- FALSE
  calls <- list(
    function(wrapper) wrapper(data, c(v, w), mean),
    function(wrapper) wrapper(data, c(v, w), mean, .unpack = FALSE),
    function(wrapper) wrapper(data, c(v, w), mean, .unpack = dynamic)
  )
  for (call in calls) {
    for (wrapper in wrappers[c("native", "by", "rollup")]) {
      expect_error(call(wrapper), "Column `..1` doesn't exist")
    }
  }
})

test_that("a forwarded per-group across() sits beside a Total share", {
  data <- forwarded_across_fixture()
  margin <- function(data, ...) {
    summarize_with_margins(
      data, n = sum(v), dplyr::across(...), p = share_of_total(n),
      .grouping = rollup("g")
    )
  }
  actual <- margin(data, c(v, w), forwarded_across_range, .unpack = TRUE)
  expect_equal(as.data.frame(actual), data.frame(
    g = c("a", "b", "Total"), n = c(3, 7, 10),
    v_lo = c(1, 3, 1), v_hi = c(2, 4, 4), w_lo = c(4, 6, 4), w_hi = c(5, 7, 7),
    p = c(0.3, 0.7, 1)
  ))
})

test_that("forwarded if_any() and if_all() agree with dplyr per group", {
  data <- forwarded_across_fixture()
  native <- function(data, ...) {
    dplyr::summarise(
      data, any = sum(dplyr::if_any(...)), all = sum(dplyr::if_all(...)),
      .by = "g"
    )
  }
  margin <- function(data, ...) {
    summarize_with_margins(
      data, any = sum(dplyr::if_any(...)), all = sum(dplyr::if_all(...)),
      .by = "g"
    )
  }
  above <- function(x) x > 2
  expect_equal(
    as.data.frame(margin(data, c(v, w), above)),
    as.data.frame(native(data, c(v, w), above))
  )
})
