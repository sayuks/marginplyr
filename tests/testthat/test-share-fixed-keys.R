test_that("dtplyr shares keep whitespace-named fixed partitions distinct", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(a = c("X", "X"), group = c("x", "x"), value = c(2, 3))
  data[[" a "]] <- c("F", "G")

  for (helper in c("share_of_parent", "share_of_total")) {
    share <- rlang::call2(helper, rlang::sym("total"))
    summarize <- function(input) {
      rlang::inject(summarize_with_margins(
        input,
        total = sum(value),
        fraction = !!share,
        .by = dplyr::all_of(c(" a ", "a")),
        .grouping = rollup(group),
        .id = "set",
        .margin_label = NULL
      ))
    }
    arrange_result <- function(result) {
      as.data.frame(dplyr::arrange(result, .data[[" a "]], a, set))
    }
    expected <- arrange_result(summarize(data))
    query <- summarize(dtplyr::lazy_dt(data))

    expect_s3_class(query, "dtplyr_step")
    expect_identical(as.character(dplyr::tbl_vars(query)), names(expected))
    actual <- arrange_result(dplyr::collect(query))
    expect_identical(nrow(actual), 4L, info = helper)
    expect_equal(actual$fraction, rep(1, 4L), info = helper)
    expect_equal(actual, expected, info = helper)
  }
})

test_that("dtplyr shares preserve surrounding whitespace in every fixed key", {
  skip_if_suggest_absent("dtplyr")
  for (key in c(" a", "a ", "\ta", "a\t", "\na", "a\n")) {
    for (counterpart in c(FALSE, TRUE)) {
      data <- data.frame(group = c("x", "x"), value = c(2, 3))
      data[[key]] <- c("F", "G")
      keys <- key
      if (counterpart) {
        data[["a"]] <- c("X", "X")
        keys <- c(keys, "a")
      }
      for (implicit in c(FALSE, TRUE)) {
        by <- if (implicit) {
          rlang::quo(NULL)
        } else {
          rlang::quo(dplyr::all_of(keys))
        }
        for (helper in c("share_of_parent", "share_of_total")) {
          info <- paste(encodeString(key), counterpart, implicit, helper)
          share <- rlang::call2(helper, rlang::sym("total"))
          summarize <- function(input) {
            if (implicit) {
              input <- dplyr::group_by(
                input, dplyr::across(dplyr::all_of(keys))
              )
            }
            rlang::inject(summarize_with_margins(
              input,
              total = sum(value),
              fraction = !!share,
              .by = !!by,
              .grouping = rollup(group),
              .id = "set",
              .margin_label = NULL
            ))
          }
          arrange_result <- function(result) {
            as.data.frame(dplyr::arrange(result, .data[[key]], set))
          }
          expected <- arrange_result(summarize(data))
          query <- summarize(dtplyr::lazy_dt(data))

          expect_s3_class(query, "dtplyr_step")
          expect_identical(
            as.character(dplyr::tbl_vars(query)), names(expected), info = info
          )
          actual <- arrange_result(dplyr::collect(query))
          expect_identical(names(actual), names(expected), info = info)
          expect_identical(nrow(actual), 4L, info = info)
          expect_equal(actual$fraction, rep(1, 4L), info = info)
          expect_equal(actual, expected, info = info)
        }
      }
    }
  }
})

test_that("dtplyr whitespace-key shares defer summaries until collection", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(group = c("x", "x"), value = c(2, 3))
  data[[" a "]] <- c("F", "G")
  evaluations <- 0L
  total_value <- function(x) {
    evaluations <<- evaluations + 1L
    sum(x)
  }
  query <- summarize_with_margins(
    dtplyr::lazy_dt(data),
    total = total_value(value),
    parent = share_of_parent(total),
    whole = share_of_total(total),
    .by = dplyr::all_of(" a "),
    .grouping = rollup(group)
  )
  expect_s3_class(query, "dtplyr_step")
  expect_identical(evaluations, 0L)
  actual <- dplyr::collect(query)
  expect_gt(evaluations, 0L)
  expect_identical(nrow(actual), 4L)
  expect_equal(actual$parent, rep(1, 4L))
  expect_equal(actual$whole, rep(1, 4L))
})
