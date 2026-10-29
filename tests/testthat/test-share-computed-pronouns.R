# jarl-ignore-file unnecessary_parentheses: Evaluated indices are under test.
test_that("computed data targets cannot hide either share dependency", {
  data <- data.frame(group = c("a", "b"), value = c(1, 3), first = c(100, 300))
  observed <- new.env(parent = emptyenv())
  observed$calls <- 0L
  target <- function() {
    observed$calls <- observed$calls + 1L
    "first"
  }
  probe <- function(x) {
    observed$calls <- observed$calls + 1L
    sum(x)
  }
  column <- "first"
  reads <- alist(
    (.data)[[column]],
    (.data)[[paste0("fir", "st")]],
    ((.data))[[paste0("fir", "st")]],
    base::`[[`((.data), paste0("fir", "st")),
    ((base::`[[`))((.data), paste0("fir", "st")),
    (.data)[[target()]]
  )
  for (input in list(data, dbplyr::tbl_lazy(data))) {
    for (helper in c("share_of_parent", "share_of_total")) {
      share <- rlang::call2(helper, quote(total))
      for (read in reads) {
        source_call <- rlang::call2(
          "summarize_with_margins", quote(input),
          first = quote(probe(value)), total = rlang::call2("sum", read),
          fraction = share, .grouping = quote(rollup(group))
        )
        error <- expect_error(
          rlang::eval_bare(source_call),
          "depends on earlier summary alias `first`",
          class = "marginplyr_error"
        )
        expect_identical(conditionCall(error), source_call)
        derived_call <- rlang::call2(
          "summarize_with_margins", quote(input),
          total = quote(probe(value)), first = share,
          derived = rlang::call2("sum", read), .grouping = quote(rollup(group))
        )
        error <- expect_error(
          rlang::eval_bare(derived_call),
          "Ordinary summaries cannot use an earlier",
          class = "marginplyr_error"
        )
        expect_identical(conditionCall(error), derived_call)
        expect_identical(observed$calls, 0L)
      }
    }
  }
  expect_identical(column, "first")
})

test_that("environment indices participate in both share dependency checks", {
  data <- data.frame(
    group = c("a", "b"), value = c(1, 3), first = c("foo", "bar")
  )
  foo <- 10
  bar <- 30
  observed <- new.env(parent = emptyenv())
  observed$calls <- 0L
  probe <- function(x) {
    observed$calls <- observed$calls + 1L
    x
  }
  indices <- alist(
    .env[[first[[1L]]]],
    (.env)[[first[[1L]]]],
    ((.env))[[first[[1L]]]],
    base::`[[`(.env, first[[1L]]),
    base::`[[`((.env), first[[1L]]),
    ((base::`[[`))((.env), first[[1L]]),
    .env[[if (first > 0) "foo" else "bar"]],
    (.env)[[probe(first[[1L]])]]
  )
  for (input in list(data, dbplyr::tbl_lazy(data))) {
    for (helper in c("share_of_parent", "share_of_total")) {
      share <- rlang::call2(helper, quote(total))
      for (index in indices) {
        expect_error(
          rlang::inject(summarize_with_margins(
            input, first = sum(value), total = !!index, fraction = !!share,
            .grouping = rollup(group)
          )),
          "depends on earlier summary alias `first`", class = "marginplyr_error"
        )
        expect_error(
          rlang::inject(summarize_with_margins(
            input, total = sum(value), first = !!share, derived = !!index,
            .grouping = rollup(group)
          )),
          "Ordinary summaries cannot use an earlier", class = "marginplyr_error"
        )
        expect_identical(observed$calls, 0L)
      }
    }
  }
  expect_identical(c(foo, bar), c(10, 30))
})

test_that("computed pronouns retain independent input and lexical lookups", {
  data <- data.frame(group = c("a", "b"), value = c(1, 3), first = c(100, 300))
  foo <- 10
  other <- data.frame(first = c(5, 15))
  for (helper in c("share_of_parent", "share_of_total")) {
    share <- rlang::call2(helper, quote(total))
    result <- rlang::inject(summarize_with_margins(
      data, first = sum(value),
      total = sum((.data)[[c("value")]]) + (.env)[[paste0("f", "oo")]],
      fraction = !!share,
      independent = sum((.data)[["value"]]),
      literal = base::`[[`((.env), "foo"),
      lexical = (function(first) (.env)[[first]])("foo"),
      nested = dplyr::summarise(
        other, result = sum((.data)[[paste0("fir", "st")]])
      )$result,
      .grouping = rollup(group)
    ))
    expect_equal(result$total, c(11, 13, 14))
    expect_identical(result$fraction, c(11 / 14, 13 / 14, 1))
    expect_equal(result$independent, c(1, 3, 4))
    expect_equal(result$literal, rep(10, 3))
    expect_equal(result$lexical, rep(10, 3))
    expect_equal(result$nested, rep(20, 3))
  }
  expect_identical(foo, 10)
})

test_that(".BY across planning retains caller dependencies before wrapping", {
  skip_if_suggest_absent("dtplyr")
  input <- dtplyr::lazy_dt(tibble::tibble(
    group = c("a", "b"), .BY = c(1, 3), first = c(100, 300)
  ))
  observed <- new.env(parent = emptyenv())
  observed$calls <- 0L
  probe <- function(x) {
    observed$calls <- observed$calls + 1L
    sum(x)
  }
  bodies <- alist(
    ~ sum((.data)[[paste0("fir", "st")]]),
    ~ (.env)[[if (first > 0) "foo" else "bar"]]
  )
  for (helper in c("share_of_parent", "share_of_total")) {
    share <- rlang::call2(helper, quote(total))
    for (body in bodies) {
      expect_error(
        rlang::inject(summarize_with_margins(
          input, dplyr::across(dplyr::all_of(".BY"), probe, .names = "first"),
          dplyr::across(dplyr::all_of(".BY"), !!body, .names = "total"),
          fraction = !!share, .grouping = rollup(group)
        )),
        "depends on earlier summary alias `first`", class = "marginplyr_error"
      )
      expect_error(
        rlang::inject(summarize_with_margins(
          input, dplyr::across(dplyr::all_of(".BY"), probe, .names = "total"),
          first = !!share,
          dplyr::across(dplyr::all_of(".BY"), !!body, .names = "derived"),
          .grouping = rollup(group)
        )),
        "Ordinary summaries cannot use an earlier", class = "marginplyr_error"
      )
      expect_identical(observed$calls, 0L)
    }
  }
})
