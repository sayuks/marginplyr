test_that("later across sources retain ordinary expansion context", {
  data <- tibble::tibble(g = c("a", "b", "b"), x = c(1, 10, 20))
  factory <- function(offset) {
    force(offset)
    function(x) sum(x) + offset
  }
  ordinary <- rlang::quos(
    sentinel = 7L, dplyr::across(x, list(s = factory(dplyr::n())))
  )
  expected <- dplyr::bind_rows(
    dplyr::summarise(data, !!!ordinary, .by = g),
    dplyr::mutate(dplyr::summarise(data, !!!ordinary), g = "Total")
  )
  expect_identical(expected$x_s, c(1, 30, 31))
  control <- summarize_with_margins(data, !!!ordinary, .grouping = rollup(g))
  expect_identical(control, expected)
  for (share in rlang::syms(c("share_of_parent", "share_of_total"))) {
    actual <- summarize_with_margins(
      data, !!!ordinary, p = (!!share)(x_s), .grouping = rollup(g)
    )
    expect_identical(actual[names(expected)], expected)
    expect_identical(actual$p, c(1 / 31, 30 / 31, 1))
  }
})

test_that("later across preserves single function and lambda output names", {
  data <- data.frame(g = c("a", "b", "b"), x = c(1, 10, 20))
  for (fn in rlang::exprs(sum, ~ sum(.x), function(x) sum(x))) {
    ordinary <- rlang::quos(sentinel = 7L, dplyr::across(x, !!fn))
    expected <- summarize_with_margins(data, !!!ordinary, .grouping = rollup(g))
    actual <- summarize_with_margins(
      data, !!!ordinary, p = share_of_total(x), .grouping = rollup(g)
    )
    expect_identical(actual[names(expected)], expected)
    expect_identical(actual$p, c(1 / 31, 30 / 31, 1))
  }
})

# Separate dplyr grain calls establish factory timing independently of shares.
check_later_factory_grains <- function(convert) {
  data <- convert(data.frame(
    fixed = c("u", "u", "u", "v", "v", "v"),
    g = rep(c("a", "b", "b"), 2L), x = c(1, 10, 20, 2, 20, 40)
  ))
  for (first in c(FALSE, TRUE)) {
    for (sets in c("single", "rollup", "duplicate")) {
      run <- function(margin, share = NULL, sort = NULL) {
        observed <- rlang::env(sizes = integer())
        factory <- function(offset) {
          force(offset)
          observed$sizes <- c(observed$sizes, offset)
          index <- length(observed$sizes)
          function(x) sum(x) + offset + index
        }
        ordinary <- rlang::quos(dplyr::across(
          "x", list(s = factory(dplyr::n()), mean = ~ mean(.x))
        ))
        if (!first) ordinary <- c(rlang::quos(sentinel = 7L), ordinary)
        if (margin) {
          result <- summarize_with_margins(
            data, !!!ordinary, p = (!!share)(!!rlang::sym("x_s")), tail = 9L,
            .by = "fixed",
            .grouping = switch(sets,
              single = grouping_set(), rollup = rollup("g"),
              duplicate = rollup("g", "g")
            ),
            .duplicates = "keep", .id = "set", .sort = sort
          )
        } else {
          detail <- function(set) {
            dplyr::mutate(dplyr::summarise(
              data, !!!ordinary, tail = 9L, .by = c("fixed", "g")
            ), set = set)
          }
          grains <- if (sets == "single") list() else list(detail(1L))
          if (sets == "duplicate") grains <- c(grains, list(detail(2L)))
          total <- dplyr::mutate(
            dplyr::summarise(data, !!!ordinary, tail = 9L, .by = "fixed"),
            set = length(grains) + 1L
          )
          if (sets != "single") total$g <- "Total"
          result <- dplyr::bind_rows(!!!grains, total)
        }
        order <- c("set", "fixed", if (sets != "single") "g")
        result <- dplyr::arrange(result, !!!rlang::syms(order))
        list(result = as.list(result), contexts = observed$sizes)
      }
      expected <- run(FALSE)
      expect_identical(expected$contexts, rep(0L, switch(
        sets, single = 1L, rollup = 2L, duplicate = 3L
      )))
      expect_identical(expected$result$x_s, switch(
        sets, single = c(32, 63), rollup = c(2, 31, 3, 61, 33, 64),
        duplicate = c(2, 31, 3, 61, 3, 32, 4, 62, 34, 65)
      ))
      shares <- if (sets == "single") "share_of_total" else
        c("share_of_parent", "share_of_total")
      for (share in rlang::syms(shares)) {
        for (sort in c("none", "first", "last")) {
          actual <- run(TRUE, share, sort)
          expect_identical(actual$contexts, expected$contexts)
          expect_identical(
            actual$result[names(expected$result)], expected$result
          )
          expect_identical(actual$result$p, switch(
            sets, single = c(1, 1),
            rollup = c(2 / 33, 31 / 33, 3 / 64, 61 / 64, 1, 1),
            duplicate = c(2 / 34, 31 / 34, 3 / 65, 61 / 65,
                          3 / 34, 32 / 34, 4 / 65, 62 / 65, 1, 1)
          ))
          expect_identical(names(actual$result), c(
            "fixed", if (sets != "single") "g", "set",
            if (!first) "sentinel", "x_s", "x_mean", "p", "tail"
          ))
        }
      }
    }
  }
}

test_that("bare factories preserve positions, grains and Margin orders", {
  check_later_factory_grains(identity)
  check_later_factory_grains(tibble::as_tibble)
})

test_that("eager data.table later source factories preserve grain context", {
  skip_if_suggest_absent("data.table")
  check_later_factory_grains(data.table::as.data.table)
})

test_that("later bare across keeps scalar and provenance validation", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), y = c(2, 6))
  for (share in rlang::syms(c("share_of_parent", "share_of_total"))) {
    for (fn in rlang::exprs(
      list(s = ~ as.character(sum(.x))), list(s = ~ as.Date("2026-01-01")),
      list(s = ~ c(1, 2)), list(s = ~ numeric())
    )) {
      expect_error(summarize_with_margins(
        data, sentinel = 7L, dplyr::across(x, !!fn),
        p = (!!share)(x_s), .grouping = rollup(g)
      ), class = "marginplyr_error")
    }
    expect_error(summarize_with_margins(
      data, sentinel = 7L, dplyr::across(x, list(s = sum)), x_s = sum(x),
      p = (!!share)(x_s), .grouping = rollup(g)
    ), "defined exactly once", class = "marginplyr_error")
    expect_error(summarize_with_margins(
      data, sentinel = sum(x), dplyr::across(x, list(s = ~ sum(.x) + sentinel)),
      p = (!!share)(x_s), .grouping = rollup(g)
    ), "depends on earlier summary", class = "marginplyr_error")
    expect_error(summarize_with_margins(
      data, sentinel = 7L, dplyr::across(x, list(s = sum)),
      x_s = (!!share)(x_s), .grouping = rollup(g)
    ), class = "marginplyr_error")
    actual <- summarize_with_margins(
      data, sentinel = 7L, dplyr::across(c(x, y), list(s = sum, m = mean)),
      p = (!!share)(y_m), .grouping = rollup(g)
    )
    expect_identical(actual$x_s, c(1, 3, 4))
    expect_identical(actual$y_s, c(2, 6, 8))
    expect_identical(actual$x_m, c(1, 3, 2))
    expect_identical(actual$y_m, c(2, 6, 4))
    expect_identical(actual$p, c(0.5, 1.5, 1))
  }
})

test_that("later bare across preserves lambda mask and column context", {
  data <- data.frame(g = c("a", "b", "b"), x = c(1, 10, 20), y = c(2, 20, 40))
  ordinary <- rlang::quos(sentinel = 7L, dplyr::across(
    c(x, y), list(s = ~ {
      if (dplyr::cur_column() == "x") sum(.data$x) else sum(.data$y)
    })
  ))
  expected <- summarize_with_margins(data, !!!ordinary, .grouping = rollup(g))
  actual <- summarize_with_margins(
    data, !!!ordinary, p = share_of_total(y_s), .grouping = rollup(g)
  )
  expect_identical(actual[names(expected)], expected)
  expect_identical(actual$p, c(1 / 31, 30 / 31, 1))
})

test_that("later bare across without functions retains source admission", {
  data <- data.frame(g = c("a", "b", "b"), x = c(1, 10, 20))
  expect_error(summarize_with_margins(
    data, sentinel = 7L, dplyr::across(x), p = share_of_total(x),
    .grouping = rollup(g)
  ), "unknown preceding ordinary summary",
  class = "marginplyr_error")
})

test_that("later source functions retain ordinary call arguments", {
  data <- data.frame(g = c("a", "b", "b"), x = c(1, 10, 20))
  observe <- function(value) sum(value) + nchar(deparse(substitute(value)))
  ordinary <- rlang::quos(sentinel = 7L, dplyr::across(x, list(s = observe)))
  expected <- dplyr::bind_rows(
    dplyr::summarise(data, !!!ordinary, .by = g),
    dplyr::mutate(dplyr::summarise(data, !!!ordinary), g = "Total")
  )
  expect_identical(expected$x_s, c(2, 31, 32))
  actual <- summarize_with_margins(
    data, !!!ordinary, p = share_of_total(x_s), .grouping = rollup(g)
  )
  expect_identical(actual[names(expected)], expected)
  expect_identical(actual$p, c(2 / 32, 31 / 32, 1))
})

test_that("later source checks precede following summary evaluation", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  for (value in rlang::exprs(c(1, 2), numeric())) {
    reached <- FALSE
    expect_error(summarize_with_margins(
      data, sentinel = 7L, dplyr::across(x, list(s = ~ !!value)),
      after = {
        reached <<- TRUE
        1
      }, p = share_of_total(x_s),
      .grouping = rollup(g)
    ), "exactly one value per grouping row",
    class = "marginplyr_share_cardinality_error")
    expect_false(reached)
  }
})

test_that("later source factory warnings retain ordinary repetition", {
  data <- data.frame(g = c("a", "b", "b"), x = c(1, 10, 20))
  run <- function(share) {
    warnings <- character()
    factory <- function(offset) {
      force(offset)
      warning("later factory warning")
      function(x) sum(x) + offset
    }
    request <- if (share) rlang::quos(p = share_of_total(x_s)) else list()
    withCallingHandlers(summarize_with_margins(
      data, sentinel = 7L, dplyr::across(x, list(s = factory(dplyr::n()))),
      !!!request, .grouping = rollup(g)
    ), warning = function(cnd) {
      warnings <<- c(warnings, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    })
    warnings
  }
  expected <- run(FALSE)
  expect_length(expected, 1L)
  expect_match(expected, "1 further grouping set", fixed = TRUE)
  expect_identical(run(TRUE), expected)
})

test_that("generated later source checks ignore caller namespace bindings", {
  data <- data.frame(g = c("a", "b", "b"), x = c(1, 10, 20))
  for (operator in c("::", ":::")) {
    hits <- 0L
    caller <- rlang::env()
    caller[[operator]] <- function(pkg, name) {
      pkg <- as.character(substitute(pkg))
      name <- as.character(substitute(name))
      if (identical(pkg, "marginplyr")) {
        hits <<- hits + 1L
        stop("generated namespace escaped")
      }
      get(name, envir = asNamespace(pkg))
    }
    ordinary <- rlang::new_quosure(
      quote(dplyr::across(x, list(s = sum))), caller
    )
    actual <- summarize_with_margins(
      data, sentinel = 7L, !!ordinary, p = share_of_total(x_s),
      .grouping = rollup(g)
    )
    expect_identical(actual$x_s, c(1, 30, 31))
    expect_identical(actual$p, c(1 / 31, 30 / 31, 1))
    expect_identical(hits, 0L)
  }
})

test_that("several later across sources retain written output order", {
  data <- data.frame(g = c("a", "b", "b"), x = c(1, 10, 20), y = c(2, 20, 40))
  factory <- function(offset) {
    force(offset)
    function(x) sum(x) + offset
  }
  actual <- summarize_with_margins(
    data, sentinel = 7L, dplyr::across(x, list(s = factory(dplyr::n()))),
    p = share_of_total(x_s), dplyr::across(y, list(t = factory(dplyr::n()))),
    q = share_of_parent(y_t), tail = 9L, .grouping = rollup(g)
  )
  expect_identical(
    names(actual), c("g", "sentinel", "x_s", "p", "y_t", "q", "tail")
  )
  expect_identical(actual$x_s, c(1, 30, 31))
  expect_identical(actual$y_t, c(2, 60, 62))
  expect_identical(actual$p, c(1 / 31, 30 / 31, 1))
  expect_identical(actual$q, c(1 / 31, 30 / 31, 1))
})
