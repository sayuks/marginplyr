# jarl-ignore unnecessary_parentheses: Repeated pairs are syntax under test.
test_that("parenthesized data pronouns obey both share dependency rules", {
  data <- data.frame(group = c("a", "b"), value = c(1, 3), first = c(100, 300))
  reads <- alist(
    .data$first,
    (.data)$first,
    ((.data))$first,
    .data[["first"]],
    (.data)[["first"]],
    ((.data))[["first"]]
  )

  for (input in list(data, dbplyr::tbl_lazy(data))) {
    for (kind in c("parent", "total")) {
      share <- rlang::call2(paste0("share_of_", kind), quote(total))
      for (read in reads) {
        source_call <- rlang::call2(
          "summarize_with_margins", quote(input),
          first = quote(sum(value)), total = rlang::call2("sum", read),
          share = share, .grouping = quote(rollup(group))
        )
        source_error <- expect_error(
          rlang::eval_bare(source_call),
          "depends on earlier summary alias `first`",
          class = "marginplyr_error"
        )
        expect_identical(conditionCall(source_error), source_call)

        derived_call <- rlang::call2(
          "summarize_with_margins", quote(input),
          total = quote(sum(value)), first = share,
          derived = rlang::call2("sum", read),
          .grouping = quote(rollup(group))
        )
        derived_error <- expect_error(
          rlang::eval_bare(derived_call),
          paste0("Ordinary summaries cannot use an earlier ",
                 if (kind == "parent") "Parent" else "Total", " share"),
          class = "marginplyr_error"
        )
        expect_identical(conditionCall(derived_error), derived_call)
      }
    }
  }
})

# jarl-ignore unnecessary_parentheses: Repeated pairs are syntax under test.
test_that("parenthesized pronouns preserve independent summary lookups", {
  data <- data.frame(group = c("a", "b"), value = c(1, 3), first = c(100, 300))
  first <- 10
  outside <- list(first = 10)
  reads <- alist(
    (.env)$first,
    ((.env))[["first"]],
    (outside)$first,
    ((outside))[["first"]]
  )

  for (kind in c("parent", "total")) {
    share <- rlang::call2(paste0("share_of_", kind), quote(total))
    for (read in reads) {
      result <- rlang::inject(summarize_with_margins(
        data,
        first = sum(value),
        total = sum((.data)$value) + !!read,
        share = !!share,
        .grouping = rollup(group)
      ))
      expect_equal(result$total, c(11, 13, 14))
      expect_equal(result$share, c(11 / 14, 13 / 14, 1))

      result <- rlang::inject(summarize_with_margins(
        data,
        total = sum(value), first = !!share,
        derived = !!read,
        independent = sum(((.data))[["value"]]),
        .grouping = rollup(group)
      ))
      expect_equal(result$first, c(0.25, 0.75, 1))
      expect_equal(result$derived, rep(10, 3))
      expect_equal(result$independent, c(1, 3, 4))
    }
  }
})

# jarl-ignore unnecessary_parentheses: Repeated pairs are syntax under test.
test_that("parenthesized data reads belong to their own nested mask", {
  data <- data.frame(group = c("a", "b"), value = c(1, 3))
  other <- data.frame(first = c(10, 20), share = c(40, 50))
  for (kind in c("parent", "total")) {
    share <- rlang::call2(paste0("share_of_", kind), quote(total))
    result <- rlang::inject(summarize_with_margins(
      data,
      first = sum(value),
      total = dplyr::summarise(other, x = sum((.data)$first))$x,
      share = !!share,
      nested = dplyr::summarise(other, x = sum(((.data))[["share"]])),
      .grouping = rollup(group)
    ))
    expect_equal(result$total, rep(30, 3))
    expect_equal(result$share, rep(1, 3))
    expect_equal(result$nested$x, rep(90, 3))
  }
})

# jarl-ignore unnecessary_parentheses: Repeated pairs are syntax under test.
test_that("pronoun dependency checks do not evaluate summary expressions", {
  data <- data.frame(group = c("a", "b"), value = c(1, 3), first = c(100, 300))
  observed <- new.env(parent = emptyenv())
  observed$calls <- 0L
  probe <- function() {
    observed$calls <- observed$calls + 1L
    0
  }
  reads <- alist(
    probe() + sum((.data)$first),
    probe() + sum(((.data))[["first"]])
  )
  for (input in list(data, dbplyr::tbl_lazy(data))) {
    for (kind in c("parent", "total")) {
      share <- rlang::call2(paste0("share_of_", kind), quote(total))
      for (read in reads) {
        expect_error(
          rlang::inject(summarize_with_margins(
            input, first = sum(value), total = !!read, share = !!share,
            .grouping = rollup(group)
          )),
          "depends on earlier summary alias `first`",
          class = "marginplyr_error"
        )
        expect_error(
          rlang::inject(summarize_with_margins(
            input, total = sum(value), first = !!share, derived = !!read,
            .grouping = rollup(group)
          )),
          "Ordinary summaries cannot use an earlier",
          class = "marginplyr_error"
        )
        expect_identical(observed$calls, 0L)
      }
    }
  }
})
