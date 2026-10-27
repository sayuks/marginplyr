test_that("dtplyr shares preserve integer64 grouping dimensions", {
  skip_if_suggest_absent("dtplyr")
  skip_if_suggest_absent("bit64")
  keys <- bit64::as.integer64(c("9007199254740992", "9007199254740993", NA))
  # data.table is imported by dtplyr; it is not a second backend requirement.
  data <- data.table::data.table(group = keys, value = c(1, 3, 4))
  original <- data.table::copy(data)
  source <- dtplyr::lazy_dt(data)
  evaluations <- 0L
  total_value <- function(x) {
    evaluations <<- evaluations + 1L
    sum(x)
  }

  for (helper in c("share_of_parent", "share_of_total")) {
    for (label in list(NULL, NA_character_, "Total")) {
      evaluations <- 0L
      query <- rlang::inject(summarize_with_margins(
        source,
        total = total_value(value),
        fraction = !!rlang::call2(helper, rlang::sym("total")),
        .grouping = rollup(group),
        .margin_label = label,
        .id = "set"
      ))
      expect_s3_class(query, "dtplyr_step")
      expect_identical(evaluations, 0L)
      expect_identical(
        as.character(dplyr::tbl_vars(query)),
        c("group", "set", "total", "fraction")
      )
      expect_identical(data, original)
      result <- dplyr::arrange(dplyr::collect(query), .data$set, .data$total)
      expect_gt(evaluations, 0L)
      expect_identical(names(result), c("group", "set", "total", "fraction"))
      expect_identical(result$set, c(1L, 1L, 1L, 2L))
      expect_equal(result$total, c(1, 3, 4, 8))
      expect_identical(result$fraction, c(1 / 8, 3 / 8, 1 / 2, 1))
      expected_keys <- if (identical(label, "Total")) {
        c("9007199254740992", "9007199254740993", NA, "Total")
      } else {
        c(keys, bit64::as.integer64(NA))
      }
      expect_identical(result$group, expected_keys)
      expect_identical(data, original)
    }
  }
})

test_that("dtplyr shares partition denominators by integer64 fixed keys", {
  skip_if_suggest_absent("dtplyr")
  skip_if_suggest_absent("bit64")
  keys <- bit64::as.integer64(c("9007199254740992", "9007199254740993", NA))
  data <- data.frame(
    fixed = rep(keys, each = 2L),
    group = rep(c("x", "y"), 3L),
    value = c(1, 3, 2, 6, 3, 3)
  )
  source <- dtplyr::lazy_dt(data)

  for (helper in c("share_of_parent", "share_of_total")) {
    query <- rlang::inject(summarize_with_margins(
      source,
      total = sum(value),
      fraction = !!rlang::call2(helper, rlang::sym("total")),
      .by = fixed,
      .grouping = rollup(group),
      .margin_label = NULL,
      .id = "set"
    ))
    expect_s3_class(query, "dtplyr_step")
    result <- dplyr::arrange(dplyr::collect(query),
                             .data$fixed, .data$set, .data$group)
    expect_identical(
      names(result), c("fixed", "group", "set", "total", "fraction")
    )
    expect_identical(result$fixed, rep(keys, each = 3L))
    expect_identical(result$set, rep(c(1L, 1L, 2L), 3L))
    expect_equal(result$total, c(1, 3, 4, 2, 6, 8, 3, 3, 6))
    expect_identical(
      result$fraction, c(0.25, 0.75, 1, 0.25, 0.75, 1, 0.5, 0.5, 1)
    )
  }
  expect_identical(as.data.frame(dplyr::collect(source)), data)

  fixed_only <- summarize_with_margins(
    source,
    total = sum(value),
    fraction = share_of_total(total),
    .by = fixed,
    .grouping = grouping_set(),
    .id = "set"
  )
  expect_s3_class(fixed_only, "dtplyr_step")
  result <- dplyr::arrange(dplyr::collect(fixed_only), .data$fixed)
  expect_identical(names(result), c("fixed", "set", "total", "fraction"))
  expect_identical(result$fixed, keys)
  expect_identical(result$set, rep(1L, 3L))
  expect_equal(result$total, c(4, 8, 6))
  expect_identical(result$fraction, rep(1, 3L))
  expect_identical(as.data.frame(dplyr::collect(source)), data)
})

test_that("dtplyr integer64 shares still refuse expanded ordinary summaries", {
  skip_if_suggest_absent("dtplyr")
  skip_if_suggest_absent("bit64")
  source <- dtplyr::lazy_dt(data.frame(
    group = bit64::as.integer64(c("9007199254740992", "9007199254740993")),
    value = c(1, 3)
  ))
  for (helper in c("share_of_parent", "share_of_total")) {
    query <- rlang::inject(summarize_with_margins(
      source,
      total = sum(value),
      fraction = !!rlang::call2(helper, rlang::sym("total")),
      dplyr::across(value, ~c(min(.x), max(.x)), .names = "extra"),
      .grouping = rollup(group),
      .margin_label = NULL
    ))
    expect_s3_class(query, "dtplyr_step")
    error <- expect_error(
      dplyr::collect(query),
      class = "marginplyr_share_cardinality_error"
    )
    expect_s3_class(error, "marginplyr_error")
    expect_match(conditionMessage(error), "ordinary summary.*expanded")
    expect_identical(rlang::call_name(conditionCall(error)),
                     "summarize_with_margins")
  }
})
