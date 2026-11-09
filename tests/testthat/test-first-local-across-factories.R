test_that("first across source factories retain their Grouping context", {
  data <- tibble::tibble(g = c("a", "b"), x = c(1, 3))
  for (helper in rlang::syms(c("grouping_bit", "grouping_id"))) {
    for (later in c(FALSE, TRUE)) {
      ordinary <- rlang::quos(dplyr::across(x, list(s = (function() {
        offset <- (!!helper)(g)
        function(x) sum(x) + offset
      })())))
      if (later) ordinary <- c(rlang::quos(prior = sum(x)), ordinary)
      control <- summarize_with_margins(
        data, !!!ordinary, .grouping = rollup(g)
      )
      expect_identical(control$x_s, c(1, 3, 5))
      for (share in rlang::syms(c("share_of_parent", "share_of_total"))) {
        actual <- summarize_with_margins(
          data, !!!ordinary, p = (!!share)(x_s), .grouping = rollup(g)
        )
        expect_identical(actual[names(control)], control)
        expect_identical(actual$p, c(1 / 5, 3 / 5, 1))
      }
    }
  }
})

# Compare each retained grain to a separate dplyr call using a fresh counter.
check_first_factory_grains <- function(convert) {
  data <- convert(data.frame(
    fixed = rep(c("u", "v"), each = 2L),
    g = rep(c("a", "b"), 2L), x = c(1, 3, 2, 6)
  ))
  for (named in c(FALSE, TRUE)) {
    for (duplicate in c(FALSE, TRUE)) {
      run <- function(margin, share = NULL, sort = NULL) {
        k <- 0L
        functions <- rlang::expr(list({
          k <<- k + 1L
          function(x) sum(x) + k
        }))
        if (named) names(functions) <- c("", "s")
        ordinary <- rlang::quos(dplyr::across("x", !!functions))
        if (margin) {
          source <- rlang::sym(if (named) "x_s" else "x_1")
          result <- summarize_with_margins(
            data, !!!ordinary, p = (!!share)(!!source), .by = "fixed",
            .grouping = if (duplicate) rollup("g", "g") else rollup("g"),
            .duplicates = "keep", .id = "set", .sort = sort
          )
        } else {
          detail <- function(set) {
            dplyr::mutate(
              dplyr::summarise(data, !!!ordinary, .by = c("fixed", "g")),
              set = set
            )
          }
          grains <- list(detail(1L))
          if (duplicate) grains <- c(grains, list(detail(2L)))
          total <- dplyr::mutate(
            dplyr::summarise(data, !!!ordinary, .by = "fixed"),
            g = "Total", set = if (duplicate) 3L else 2L
          )
          result <- dplyr::bind_rows(!!!grains, total)
        }
        result <- dplyr::arrange(result, .data$set, .data$fixed, .data$g)
        list(result = as.list(result), calls = k)
      }
      expected <- run(FALSE)
      source <- if (named) "x_s" else "x_1"
      expect_identical(expected$calls, if (duplicate) 3L else 2L)
      expect_identical(
        expected$result[[source]],
        if (duplicate) c(2, 4, 3, 7, 3, 5, 4, 8, 7, 11) else
          c(2, 4, 3, 7, 6, 10)
      )
      for (share in rlang::syms(c("share_of_parent", "share_of_total"))) {
        for (sort in c("none", "first", "last")) {
          actual <- run(TRUE, share, sort)
          expect_identical(actual$calls, expected$calls)
          expect_identical(actual$result[names(expected$result)],
                           expected$result)
          expect_identical(
            actual$result$p,
            if (duplicate) c(2 / 7, 4 / 7, 3 / 11, 7 / 11,
                             3 / 7, 5 / 7, 4 / 11, 8 / 11, 1, 1) else
              c(2 / 6, 4 / 6, 3 / 10, 7 / 10, 1, 1)
          )
          expect_identical(names(actual$result),
                           c("fixed", "g", "set", source, "p"))
        }
      }
    }
  }
}

test_that("first across factories retain fixed grains and Margin orders", {
  check_first_factory_grains(identity)
  check_first_factory_grains(tibble::as_tibble)
})

test_that("eager data.table first across factories retain fixed grains", {
  skip_if_suggest_absent("data.table")
  check_first_factory_grains(data.table::as.data.table)
})

test_that("per-grain source factory warnings retain ordinary repetition", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  run <- function(share) {
    warnings <- character()
    factory <- function() {
      warning("factory warning")
      function(x) sum(x)
    }
    request <- if (share) rlang::quos(p = share_of_total(x_s)) else list()
    withCallingHandlers(summarize_with_margins(
      data, dplyr::across(x, list(s = factory())), !!!request,
      .grouping = rollup(g)
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

test_that("first across source factories retain ordinary grain lifetimes", {
  data <- tibble::tibble(g = c("a", "b"), x = c(1, 3))
  run <- function(margin) {
    k <- 0L
    ordinary <- rlang::quos(dplyr::across(x, list(s = {
      k <<- k + 1L
      function(x) sum(x) + k
    })))
    result <- if (margin) {
      summarize_with_margins(
        data, !!!ordinary, p = share_of_total(x_s), .grouping = rollup(g)
      )
    } else {
      dplyr::bind_rows(
        dplyr::summarise(data, !!!ordinary, .by = g),
        dplyr::mutate(dplyr::summarise(data, !!!ordinary), g = "Total")
      )
    }
    list(result = result, calls = k)
  }
  expected <- run(FALSE)
  expect_identical(expected$result$x_s, c(2, 4, 6))
  expect_identical(expected$calls, 2L)
  actual <- run(TRUE)
  expect_identical(actual$calls, expected$calls)
  expect_identical(actual$result[names(expected$result)], expected$result)
  expect_identical(actual$result$p, c(2 / 6, 4 / 6, 1))
})

test_that("first factory planning retains source validation", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  for (share in rlang::syms(c("share_of_parent", "share_of_total"))) {
    expect_error(summarize_with_margins(
      data, dplyr::across(x, list(s = (function() sum)())), x_s = sum(x),
      p = (!!share)(x_s), .grouping = rollup(g)
    ), "defined exactly once", class = "marginplyr_error")
    expect_error(summarize_with_margins(
      data, dplyr::across(x, list(s = (function() as.character)())),
      p = (!!share)(x_s), .grouping = rollup(g)
    ), "plain integer or double scalar", class = "marginplyr_error")
    expect_error(summarize_with_margins(
      data, dplyr::across(x, list(s = (function() function(x) c(1, 2))())),
      p = (!!share)(x_s), .grouping = rollup(g)
    ), "exactly one value per grouping row",
    class = "marginplyr_share_cardinality_error")
    expect_error(summarize_with_margins(
      data, dplyr::across(x, list(s = (function() sum)())),
      x_s = (!!share)(x_s), .grouping = rollup(g)
    ), class = "marginplyr_error")
  }
})

test_that("unknown source factories remain unevaluated", {
  calls <- 0L
  factory <- function() {
    calls <<- calls + 1L
    sum
  }
  for (data in list(data.frame(g = "a", x = 1),
                    dbplyr::lazy_frame(g = "a", x = 1))) {
    expect_error(summarize_with_margins(
      data, dplyr::across(x, factory(), .names = "s"),
      p = share_of_total(s), .grouping = rollup(g)
    ), "unknown preceding ordinary summary", class = "marginplyr_error")
  }
  expect_identical(calls, 0L)
})
