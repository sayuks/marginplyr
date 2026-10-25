test_that("shares reject dimensional numeric sources during validation", {
  data <- data.frame(group = c("a", "b"), value = c(2, 6))
  sources <- list(
    quote(crossprod(value)),
    quote(matrix(2)),
    quote(matrix(2L)),
    quote(array(2, dim = c(1L, 1L, 1L))),
    quote(array(2L, dim = c(1L, 1L, 1L)))
  )
  shares <- list(
    list(share = quote(share_of_parent(total))),
    list(share = quote(share_of_total(total))),
    list(quote(dplyr::across(total, share_of_parent, .names = "share"))),
    list(quote(dplyr::across(total, share_of_total, .names = "share")))
  )

  for (source in sources) {
    for (share in shares) {
      error <- expect_error(
        rlang::inject(summarize_with_margins(
          data,
          total = !!source,
          !!!share,
          .grouping = rollup(group)
        )),
        class = "marginplyr_error"
      )
      expect_match(conditionMessage(error), "source summary `total`")
      expect_match(conditionMessage(error), "plain integer or double scalar")
      expect_match(conditionMessage(error), "matrix|array")
      expect_identical(
        rlang::call_name(conditionCall(error)),
        "summarize_with_margins"
      )
    }
  }
})

test_that("ordinary matrix summaries retain their values and dimensions", {
  data <- data.frame(group = c("a", "b"), value = c(2, 6))
  result <- summarize_with_margins(
    data,
    total = crossprod(value),
    .grouping = rollup(group),
    .id = "set"
  ) |>
    dplyr::arrange(set, group)

  expect_identical(result$total, matrix(c(4, 36, 40), ncol = 1L))
})

test_that("plain numeric share sources retain their ratios", {
  data <- data.frame(group = c("a", "b"), value = c(2, 6))
  for (source in list(quote(sum(value^2)), quote(as.integer(sum(value^2))))) {
    result <- rlang::inject(summarize_with_margins(
      data,
      total = !!source,
      parent = share_of_parent(total),
      overall = share_of_total(total),
      .grouping = rollup(group),
      .id = "set"
    )) |>
      dplyr::arrange(set, group)

    expect_identical(result$parent, c(0.1, 0.9, 1))
    expect_identical(result$overall, c(0.1, 0.9, 1))
  }
})

test_that("share source expression errors retain their original cause", {
  data <- data.frame(group = c("a", "b"), value = c(2, 6))
  cause <- rlang::error_cnd(
    "source_expression_error",
    message = "source failed",
    source_detail = "original failure"
  )
  shares <- list(quote(share_of_parent(total)), quote(share_of_total(total)))
  for (share in shares) {
    error <- expect_error(
      rlang::inject(summarize_with_margins(
        data,
        total = rlang::cnd_signal(cause),
        share = !!share,
        .grouping = rollup(group)
      )),
      "source failed"
    )

    expect_s3_class(error, "rlang_error")
    expect_false(inherits(error, "marginplyr_error"))
    expect_identical(class(error$parent), class(cause))
    expect_identical(conditionMessage(error$parent), "source failed")
    expect_identical(error$parent$source_detail, "original failure")
  }
})
