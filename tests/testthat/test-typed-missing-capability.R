test_that("omitted raw dimensions reject typed-missing labels", {
  data <- tibble::tibble(key = as.raw(1))
  expect_error(
    expand_with_margins(data, .grouping = rollup(key), .margin_label = NULL),
    "typed-missing.*key|key.*typed-missing",
    class = "marginplyr_error"
  )
})

test_that("each Margin verb refuses unrepresentable omitted keys", {
  verbs <- list(
    expand_with_margins = function(data, label, check) {
      expand_with_margins(data, .grouping = rollup(key),
                          .margin_label = label, .check_margin_label = check)
    },
    summarize_with_margins = function(data, label, check) {
      summarize_with_margins(data, n = dplyr::n(), .grouping = rollup(key),
                             .margin_label = label, .check_margin_label = check)
    },
    nest_with_margins = function(data, label, check) {
      nest_with_margins(data, .grouping = rollup(key),
                        .margin_label = label, .check_margin_label = check)
    },
    nest_by_with_margins = function(data, label, check) {
      nest_by_with_margins(data, .grouping = rollup(key),
                           .margin_label = label, .check_margin_label = check)
    }
  )
  keys <- list(raw = as.raw(c(0, 1)),
               matrix = matrix(as.raw(c(0, 1)), ncol = 1),
               packed = tibble::tibble(raw = as.raw(c(0, 1)), value = 1:2))
  for (verb in names(verbs)) {
    for (key in keys) {
      for (label in list(NULL, NA_character_)) {
        for (check in c(FALSE, TRUE)) {
          data <- tibble::tibble(key = key)
          error <- rlang::catch_cnd(verbs[[verb]](data, label, check))
          expect_s3_class(error, "marginplyr_error")
          expect_match(conditionMessage(error), "typed-missing")
          expect_match(conditionMessage(error), "key")
          expect_identical(as.character(error$call[[1]]), verb)
        }
      }
    }
  }
})

test_that("raw fixed and included keys and explicit labels are preserved", {
  data <- tibble::tibble(key = as.raw(c(0, 1)), fixed = as.raw(c(1, 1)),
                         value = c(10L, 20L))
  included <- expand_with_margins(data, .grouping = grouping_set(key),
                                  .margin_label = NULL)
  expect_identical(included$key, data$key)
  fixed <- summarize_with_margins(data, total = sum(value), .by = fixed,
                                  .grouping = rollup(value),
                                  .margin_label = NULL)
  expect_true(all(fixed$fixed == as.raw(1)))
  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    result <- verb(data, .grouping = rollup(key), .margin_label = "Total",
                   .id = "set", .keep = TRUE)
    expect_identical(result$key, c("00", "01", "Total"))
    expect_identical(result$data[[3]]$key, data$key)
    expect_identical(result$data[[3]]$value, data$value)
  }
})

test_that("structured keys that represent missing values remain supported", {
  for (key in list(matrix(1:2, ncol = 1),
                   tibble::tibble(a = 1:2, b = c("a", "b")))) {
    data <- tibble::tibble(key = key)
    result <- expand_with_margins(data, .grouping = rollup(key),
                                  .margin_label = NULL, .id = "set")
    margin <- vctrs::vec_slice(result$key, which(result$set == 2L))
    expect_true(all(vctrs::vec_detect_missing(margin)))
  }
})
