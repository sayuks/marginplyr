test_that("included packed keys work through all four local Margin verbs", {
  data <- tibble::tibble(
    key = tibble::tibble(code = c(1L, 2L), day = as.Date(c(
      "2026-01-01", "2026-01-02"
    ))),
    value = c(3L, 5L)
  )
  spec <- grouping_set(key)

  for (label in list(NULL, NA_character_)) {
    summary <- summarize_with_margins(
      data, total = sum(value), .grouping = spec,
      .margin_label = label, .sort = "none"
    )
    expansion <- expand_with_margins(
      data, .grouping = spec, .margin_label = label, .sort = "none"
    )
    nested <- nest_with_margins(
      data, .grouping = spec, .margin_label = label, .sort = "none"
    )
    nested_by <- nest_by_with_margins(
      data, .grouping = spec, .margin_label = label, .sort = "none"
    )

    expect_identical(summary$key, data$key)
    expect_identical(summary$total, c(3L, 5L))
    expect_identical(expansion$key, data$key)
    expect_identical(expansion$value, data$value)
    expect_identical(nested$key, data$key)
    expect_identical(nested_by$key, data$key)
    expect_identical(vapply(nested$data, nrow, integer(1)), c(1L, 1L))
    expect_identical(vapply(nested_by$data, nrow, integer(1)), c(1L, 1L))
  }
})

test_that("omitted packed keys are one typed-missing row in every verb", {
  key <- tibble::tibble(
    code = c(1L, NA_integer_, 2L),
    day = as.Date(c("2026-01-01", NA, "2026-01-02")),
    tier = factor(c("a", NA, "b"), levels = c("a", "b"))
  )
  data <- tibble::tibble(key = key, value = c(3L, 5L, 7L))
  missing_key <- tibble::tibble(
    code = NA_integer_, day = as.Date(NA),
    tier = factor(NA, levels = c("a", "b"))
  )
  spec <- rollup(key)

  summary <- summarize_with_margins(
    data, n = dplyr::n(), bit = grouping_bit(key),
    .grouping = spec, .margin_label = NULL, .sort = "none"
  )
  expect_identical(nrow(summary), 4L)
  expect_identical(as.integer(table(summary$bit)), c(3L, 1L))
  expect_identical(summary$key[summary$bit == 1L, ], missing_key)
  expect_identical(summary$n[summary$bit == 1L], 3L)
  expect_true(anyNA(summary$key$code[summary$bit == 0L]))

  expansion <- expand_with_margins(
    data, .grouping = spec, .margin_label = NULL,
    .id = "set", .sort = "none"
  )
  expect_identical(nrow(expansion), 6L)
  expect_identical(as.integer(table(expansion$set)), c(3L, 3L))
  expect_identical(expansion$key[expansion$set == 2L, ],
                   missing_key[c(1L, 1L, 1L), ])
  expect_true(anyNA(expansion$key$code[expansion$set == 1L]))

  for (nest in list(nest_with_margins, nest_by_with_margins)) {
    result <- nest(
      data, .grouping = spec, .margin_label = NULL,
      .id = "set", .sort = "none"
    )
    expect_identical(nrow(result), 4L)
    expect_identical(as.integer(table(result$set)), c(3L, 1L))
    expect_identical(result$key[result$set == 2L, ], missing_key)
    expect_identical(sort(vapply(result$data, nrow, integer(1))),
                     c(1L, 1L, 1L, 3L))
    expect_true(anyNA(result$key$code[result$set == 1L]))
  }

  missing_character <- summarize_with_margins(
    data, n = dplyr::n(), bit = grouping_bit(key),
    .grouping = spec, .margin_label = NA_character_, .sort = "none"
  )
  expect_identical(missing_character$key[missing_character$bit == 1L, ],
                   missing_key)
})

test_that("base data-frame packed keys follow row-vector semantics", {
  data <- as.data.frame(tibble::tibble(
    key = data.frame(code = c(1L, 2L), day = as.Date(c(
      "2026-01-01", "2026-01-02"
    ))),
    value = c(3L, 5L)
  ))
  result <- expand_with_margins(
    data, .grouping = rollup(key), .margin_label = NULL,
    .id = "set", .sort = "none"
  )

  expect_identical(nrow(result), 4L)
  omitted <- result$key[result$set == 2L, ]
  rownames(omitted) <- NULL
  expect_identical(omitted, data.frame(
    code = c(NA_integer_, NA_integer_), day = as.Date(c(NA, NA))
  ))
})

test_that("packed and ordinary keys accept different per-dimension labels", {
  data <- tibble::tibble(
    region = c("East", "West"),
    key = tibble::tibble(code = c(1L, 2L), day = as.Date(c(
      "2026-01-01", "2026-01-02"
    ))),
    value = c(3L, 5L)
  )
  result <- summarize_with_margins(
    data, n = dplyr::n(), id = grouping_id(region, key),
    .grouping = rollup(region, key),
    .margin_label = list(region = "All regions", key = NULL),
    .sort = "none"
  )

  expect_identical(nrow(result), 5L)
  expect_identical(result$region[result$id == 3L], "All regions")
  expect_identical(result$key[result$id == 3L, ], tibble::tibble(
    code = NA_integer_, day = as.Date(NA)
  ))
  expect_identical(result$region[result$id == 1L], c("East", "West"))
  expect_true(all(is.na(result$key$code[result$id != 0L])))
})

test_that("empty packed inputs keep typed keys and nested row counts", {
  data <- tibble::tibble(
    key = tibble::tibble(code = integer(), day = as.Date(character())),
    value = integer()
  )
  spec <- rollup(key)
  missing_key <- tibble::tibble(code = NA_integer_, day = as.Date(NA))

  summary <- summarize_with_margins(
    data, n = dplyr::n(), bit = grouping_bit(key),
    .grouping = spec, .margin_label = NULL, .sort = "none"
  )
  expansion <- expand_with_margins(
    data, .grouping = spec, .margin_label = NULL, .sort = "none"
  )
  expect_identical(nrow(summary), 1L)
  expect_identical(summary$key, missing_key)
  expect_identical(summary$n, 0L)
  expect_identical(summary$bit, 1L)
  expect_identical(nrow(expansion), 0L)
  expect_identical(names(expansion$key), c("code", "day"))
  expect_identical(typeof(expansion$key$code), "integer")
  expect_s3_class(expansion$key$day, "Date")

  for (nest in list(nest_with_margins, nest_by_with_margins)) {
    result <- nest(
      data, .grouping = spec, .margin_label = NULL, .sort = "none"
    )
    expect_identical(nrow(result), 0L)
    expect_identical(names(result$key), c("code", "day"))
    expect_identical(typeof(result$key$code), "integer")
    expect_s3_class(result$key$day, "Date")
  }
})

test_that("a string label for a packed key names the dimension and remedy", {
  data <- tibble::tibble(
    key = tibble::tibble(code = 1L, day = as.Date("2026-01-01")),
    value = 3L
  )
  spec <- grouping_set(key)
  operations <- list(
    function() summarize_with_margins(
      data, n = dplyr::n(), .grouping = spec,
      .margin_label = "All", .check_margin_label = FALSE
    ),
    function() expand_with_margins(
      data, .grouping = spec,
      .margin_label = "All", .check_margin_label = FALSE
    ),
    function() nest_with_margins(
      data, .grouping = spec,
      .margin_label = "All", .check_margin_label = FALSE
    ),
    function() nest_by_with_margins(
      data, .grouping = spec,
      .margin_label = "All", .check_margin_label = FALSE
    )
  )

  for (operation in operations) {
    error <- expect_error(operation(), "packed grouping dimensions")
    expect_s3_class(error, "marginplyr_error")
    expect_match(conditionMessage(error), "`key`")
    expect_match(conditionMessage(error), "NULL")
    expect_match(conditionMessage(error), "unpack")
  }
})
