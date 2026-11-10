test_that("shares keep distinct complex missing fixed partitions isolated", {
  input <- tibble::tibble(
    k = complex(real = c(NaN, NaN), imaginary = c(0, NaN)),
    g = c("a", "a"), x = c(1, 3)
  )
  before <- serialize(input, NULL)
  ordinary <- dplyr::summarize(input, value = sum(x), .by = k)
  expect_identical(ordinary$value, c(1, 3))

  result <- summarize_with_margins(
    input, value = sum(x), parent = share_of_parent(value),
    whole = share_of_total(value), .by = k, .grouping = rollup(g),
    .margin_label = NULL, .id = "set"
  )

  expect_identical(nrow(result), 4L)
  expect_identical(result$value, c(1, 3, 1, 3))
  expect_identical(result$parent, rep(1, 4))
  expect_identical(result$whole, rep(1, 4))
  expect_identical(serialize(input, NULL), before)
})

test_that("Parent shares match complex missing keys across duplicates", {
  for (real in c(NaN, NA_real_)) {
    input <- tibble::tibble(
      k = rep(complex(real = rep(real, 2), imaginary = c(0, NaN)), each = 2),
      g = rep(c("a", "b"), 2), x = c(1, 3, 2, 6)
    )
    before <- serialize(input, NULL)
    ordinary <- dplyr::summarize(input, value = sum(x), .by = k)
    expect_identical(ordinary$value, c(4, 8))

    result <- summarize_with_margins(
      input, value = sum(x), parent = share_of_parent(value),
      whole = share_of_total(value), .grouping = rollup(k, g, g),
      .duplicates = "keep", .margin_label = NULL, .id = "set"
    )

    expect_identical(nrow(result), 11L)
    expect_identical(result$value, c(1, 3, 2, 6, 1, 3, 2, 6, 4, 8, 12))
    expect_equal(result$parent, c(rep(c(0.25, 0.75), 4), 1 / 3, 2 / 3, 1))
    expect_equal(result$whole, c(rep(c(1 / 12, 0.25, 1 / 6, 0.5), 2),
                                 1 / 3, 2 / 3, 1))
    expect_type(result$parent, "double")
    expect_type(result$whole, "double")
    expect_identical(serialize(input, NULL), before)
  }
})

test_that("shares preserve identical missing and finite complex key groups", {
  keys <- list(
    identical_nan = rep(complex(real = NaN, imaginary = 0), 2),
    identical_na = rep(NA_complex_, 2),
    partial_na = complex(real = c(NA_real_, NA_real_), imaginary = c(0, NaN)),
    finite = c(1 + 1i, 2 + 2i)
  )
  for (name in names(keys)) {
    input <- tibble::tibble(k = keys[[name]], g = c("a", "b"), x = c(1, 3))
    before <- serialize(input, NULL)
    ordinary <- dplyr::summarize(input, value = sum(x), .by = k)
    combined <- startsWith(name, "identical")
    expect_identical(ordinary$value, if (combined) 4 else c(1, 3), info = name)

    result <- summarize_with_margins(
      input, value = sum(x), parent = share_of_parent(value),
      whole = share_of_total(value), .by = k, .grouping = rollup(g, g),
      .duplicates = "keep", .margin_label = NULL, .id = "set"
    )

    expected <- if (combined) c(0.25, 0.75, 0.25, 0.75, 1) else rep(1, 6)
    expect_identical(nrow(result), length(expected), info = name)
    expect_identical(result$parent, expected, info = name)
    expect_identical(result$whole, expected, info = name)
    expect_identical(serialize(input, NULL), before, info = name)
  }
})
