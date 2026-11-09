test_that("parenthesized across outputs remain numeric share sources", {
  data <- tibble::tibble(g = c("a", "b"), x = c(1, 3))
  for (expression in alist(
    (across)(x, sum, .names = "measure"),
    (across(x, sum, .names = "measure"))
  )) {
    for (share in rlang::syms(c("share_of_parent", "share_of_total"))) {
      actual <- summarize_with_margins(
        data, sentinel = 7L, !!expression, p = (!!share)(measure),
        .grouping = rollup(g)
      )
      expect_identical(actual$measure, c(1, 3, 4))
      expect_identical(actual$p, c(0.25, 0.75, 1))
      expect_identical(names(actual), c("g", "sentinel", "measure", "p"))
    }
  }
})

# Each spelling is exercised before local isolation rewrites its runtime head.
# jarl-ignore unnecessary_parentheses: Repeated pairs are syntax under test.
parenthesized_source_spellings <- alist(
  bare = across(x, sum, .names = "measure"),
  qualified = dplyr::across(x, sum, .names = "measure"),
  head = (across)(x, sum, .names = "measure"),
  call = (across(x, sum, .names = "measure")),
  repeated_head = ((across))(x, sum, .names = "measure"),
  repeated_call = ((across(x, sum, .names = "measure"))),
  qualified_head = (dplyr::across)(x, sum, .names = "measure"),
  qualified_call = (dplyr::across(x, sum, .names = "measure")),
  qualified_repeated_head = ((dplyr::across))(x, sum, .names = "measure"),
  qualified_repeated_call = ((dplyr::across(x, sum, .names = "measure"))),
  both = (((dplyr::across))(x, sum, .names = "measure"))
)

# Local frame classes share the same summary-mask contract.
check_across_identity_frames <- function(convert) {
  data <- convert(data.frame(g = c("a", "b"), x = c(1, 3)))
  for (expression in parenthesized_source_spellings) {
    for (first in c(FALSE, TRUE)) {
      ordinary <- rlang::quos(!!expression)
      if (!first) ordinary <- c(rlang::quos(sentinel = 7L), ordinary)
      for (single in c(FALSE, TRUE)) {
        grouping <- if (single) grouping_set() else rollup("g")
        expected_values <- if (single) 4 else c(1, 3, 4)
        control <- summarize_with_margins(
          data, !!!ordinary, tail = .data$measure + 2, .grouping = grouping
        )
        for (share in rlang::syms(c("share_of_parent", "share_of_total"))) {
          if (single && identical(share, rlang::sym("share_of_parent"))) next
          actual <- summarize_with_margins(
            data, !!!ordinary, p = (!!share)(!!rlang::sym("measure")),
            tail = .data$measure + 2,
            .grouping = grouping
          )
          expect_identical(actual$measure, expected_values)
          expect_identical(actual$p, if (single) 1 else c(0.25, 0.75, 1))
          expect_identical(actual[names(control)], control)
          expect_identical(names(actual), c(
            if (!single) "g", if (!first) "sentinel", "measure", "p", "tail"
          ))
        }
      }
    }
  }
}

test_that("parenthesized sources retain order on data frames and tibbles", {
  check_across_identity_frames(identity)
  check_across_identity_frames(tibble::as_tibble)
})

test_that("parenthesized sources retain order on eager data tables", {
  skip_if_suggest_absent("data.table")
  check_across_identity_frames(data.table::as.data.table)
})

# jarl-ignore unnecessary_parentheses: Repeated pairs are syntax under test.
test_that("parenthesized source factories retain per-group evaluation", {
  data <- tibble::tibble(g = c("a", "b", "b"), x = c(1, 10, 20))
  expressions <- alist(
    (across)(x, list(s = factory()), .names = "measure"),
    (across(x, list(s = factory()), .names = "measure")),
    ((dplyr::across))(x, list(s = factory()), .names = "measure"),
    ((dplyr::across(x, list(s = factory()), .names = "measure")))
  )
  for (expression in expressions) {
    for (first in c(FALSE, TRUE)) {
      run <- function(margin, share = NULL) {
        observed <- rlang::env(sizes = integer())
        factory <- function() {
          size <- dplyr::n()
          observed$sizes <- c(observed$sizes, size)
          function(x) sum(x) + size
        }
        across <- dplyr::across
        ordinary <- rlang::quos(!!expression)
        if (!first) ordinary <- c(rlang::quos(sentinel = 7L), ordinary)
        if (margin) {
          actual <- summarize_with_margins(
            data, !!!ordinary, p = (!!share)(!!rlang::sym("measure")),
            tail = .data$measure + 2,
            .grouping = rollup(g)
          )
        } else {
          ordinary <- c(ordinary, rlang::quos(tail = .data$measure + 2))
          actual <- dplyr::bind_rows(
            dplyr::summarise(data, !!!ordinary, .by = g),
            dplyr::mutate(dplyr::summarise(data, !!!ordinary), g = "Total")
          )
        }
        list(result = actual, contexts = observed$sizes)
      }
      expected <- run(FALSE)
      expect_identical(expected$result$measure, c(2, 32, 34))
      expect_identical(expected$contexts, rep(1:3, each = 2L))
      for (share in rlang::syms(c("share_of_parent", "share_of_total"))) {
        actual <- run(TRUE, share)
        expect_identical(actual$contexts, expected$contexts)
        expect_identical(actual$result[names(expected$result)], expected$result)
        expect_identical(actual$result$p, c(2 / 34, 32 / 34, 1))
      }
    }
  }
})

test_that("parenthesized sources preserve lexical and namespace isolation", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  for (expression in parenthesized_source_spellings[-c(1L, 2L)]) {
    hits <- 0L
    caller <- rlang::env(
      across = function(...) stop("caller across ran"),
      sum = function(x) base::sum(x) * 2
    )
    reject <- function(pkg, name) {
      pkg <- as.character(substitute(pkg))
      name <- as.character(substitute(name))
      if (identical(pkg, "marginplyr") ||
            (identical(pkg, "dplyr") && identical(name, "across"))) {
        hits <<- hits + 1L
        stop("caller namespace operator ran")
      }
      get(name, envir = asNamespace(pkg))
    }
    caller[["::"]] <- reject
    caller[[":::"]] <- reject
    ordinary <- rlang::new_quosure(expression, caller)
    control <- summarize_with_margins(
      data, sentinel = 7L, !!ordinary, .grouping = rollup(g)
    )
    actual <- summarize_with_margins(
      data, sentinel = 7L, !!ordinary, p = share_of_total(measure),
      .grouping = rollup(g)
    )
    expect_identical(hits, 0L)
    expect_identical(actual[names(control)], control)
    expect_identical(actual$measure, c(2, 6, 8))
    expect_identical(actual$p, c(0.25, 0.75, 1))
  }
})

test_that("parenthesized sources retain type and scalar cardinality checks", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  for (share in rlang::syms(c("share_of_parent", "share_of_total"))) {
    for (value in rlang::exprs(
      "text", as.Date("2026-01-01"), c(1, 2), numeric()
    )) {
      fn <- rlang::expr(~ !!value)
      for (expression in rlang::exprs(
        (across)(x, !!fn, .names = "measure"),
        (dplyr::across(x, !!fn, .names = "measure"))
      )) {
        cardinality <- !identical(value, "text") &&
          !identical(value, quote(as.Date("2026-01-01")))
        reached <- FALSE
        expect_error(summarize_with_margins(
          data, sentinel = 7L, !!expression,
          after = {
            reached <<- TRUE
            1
          }, p = (!!share)(measure), .grouping = rollup(g)
        ), if (cardinality) "exactly one value per grouping row" else
          "plain integer or double scalar",
        class = if (cardinality) "marginplyr_share_cardinality_error" else
          "marginplyr_error")
        expect_false(reached)
      }
    }
  }
})

test_that("parenthesized sources retain admission and dependency checks", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  for (share in rlang::syms(c("share_of_parent", "share_of_total"))) {
    for (expression in parenthesized_source_spellings[-c(1L, 2L)]) {
      expect_error(summarize_with_margins(
        data, sentinel = 7L, p = (!!share)(measure), !!expression,
        .grouping = rollup(g)
      ), "forward reference", class = "marginplyr_error")
      expect_error(summarize_with_margins(
        data, sentinel = 7L, packed = !!expression,
        p = (!!share)(packed), .grouping = rollup(g)
      ), "packs", class = "marginplyr_error")
      expect_error(summarize_with_margins(
        data, sentinel = 7L, !!expression, measure = sum(x),
        p = (!!share)(measure), .grouping = rollup(g)
      ), "defined exactly once", class = "marginplyr_error")
      expect_error(summarize_with_margins(
        data, sentinel = 7L, !!expression,
        measure = (!!share)(measure), .grouping = rollup(g)
      ), class = "marginplyr_error")
    }
    expect_error(summarize_with_margins(
      data, sentinel = sum(x),
      (across)(sentinel, sum, .names = "measure"),
      p = (!!share)(measure), .grouping = rollup(g)
    ), "depends on earlier summary", class = "marginplyr_error")
    expect_error(summarize_with_margins(
      data, sentinel = sum(x),
      (dplyr::across(x, ~ sum(.x) + sentinel, .names = "measure")),
      p = (!!share)(measure), .grouping = rollup(g)
    ), "depends on earlier summary", class = "marginplyr_error")
    expect_error(summarize_with_margins(
      data, sentinel = 7L, ((across))(x),
      p = (!!share)(x), .grouping = rollup(g)
    ), "unknown preceding ordinary summary", class = "marginplyr_error")
    unknown <- list(s = sum)
    expect_error(summarize_with_margins(
      data, sentinel = 7L, (across(x, unknown)),
      p = (!!share)(x_s), .grouping = rollup(g)
    ), "unknown preceding ordinary summary", class = "marginplyr_error")
  }
})

# jarl-ignore unnecessary_parentheses: Function parentheses are under test.
test_that("parenthesized source checks distinguish expanded outputs", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), y = c(2, 6))
  actual <- summarize_with_margins(
    data, sentinel = 7L,
    ((across))(c(y, x), list(total = (sum), average = ((mean)))),
    p = share_of_parent(x_total),
    (dplyr::across(y, sum, .names = "other")),
    q = share_of_total(other), tail = x_average + other,
    .grouping = rollup(g)
  )
  expect_identical(names(actual), c(
    "g", "sentinel", "y_total", "y_average", "x_total", "x_average",
    "p", "other", "q", "tail"
  ))
  expect_identical(actual$x_total, c(1, 3, 4))
  expect_identical(actual$other, c(2, 6, 8))
  expect_identical(actual$tail, c(3, 9, 10))
  expect_identical(actual$p, c(0.25, 0.75, 1))
  expect_identical(actual$q, c(0.25, 0.75, 1))
})
