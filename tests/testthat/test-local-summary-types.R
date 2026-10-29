test_that("an unnamed local summary refuses mixed frame and scalar values", {
  data <- data.frame(g = c(1L, 1L, 2L), x = 1:3)
  mixed <- function(columns) {
    value <- sum(columns[[1L]])
    if (nrow(columns) > 1L) data.frame(z = value) else value
  }

  expect_error(
    dplyr::summarise(data, mixed(dplyr::pick(x)), .by = "g"),
    "compatible vectors across groups"
  )
  expect_error(
    summarize_with_margins(
      data, mixed(dplyr::pick(x)), .grouping = grouping_set("g")
    ),
    "Can.t combine"
  )
})

# Exercise the public verb with each input class in a separate test so only
# the data.table case depends on an optional backend (AGENTS.md).
check_mixed_local_types <- function(convert) {
  non_frames <- list(
    integer = 3L, double = 3, character = "3", logical = TRUE,
    list = list(3L), matrix = matrix(3L, nrow = 1L),
    date = as.Date("2026-01-01")
  )
  for (rows in list(1:3, c(3L, 1L, 2L))) {
    data <- convert(data.frame(fixed = "a", g = c(1L, 1L, 2L), x = 1:3)[rows, ])
    for (non_frame in non_frames) {
      counter <- new.env(parent = emptyenv())
      counter$calls <- 0L
      mixed <- function(x) {
        counter$calls <- counter$calls + 1L
        if (length(x) > 1L) data.frame(z = sum(x)) else non_frame
      }
      expect_error(
        dplyr::summarise(data, mixed(.data$x), .by = c("fixed", "g")),
        "compatible vectors across groups"
      )
      counter$calls <- 0L
      error <- expect_error(
        summarize_with_margins(data, mixed(.data$x), .by = "g"),
        "Can.t combine"
      )
      expect_s3_class(error$parent, "vctrs_error_incompatible_type")
      expect_false(inherits(error, "marginplyr_error"))
      expect_identical(counter$calls, 2L)

      counter$calls <- 0L
      expect_error(
        summarize_with_margins(
          data, mixed(.data$x), .grouping = grouping_set("g")
        ),
        "Can.t combine"
      )
      expect_identical(counter$calls, 2L)

      counter$calls <- 0L
      expect_error(
        summarize_with_margins(
          data, mixed(.data$x), .by = "fixed", .grouping = grouping_set("g")
        ),
        "Can.t combine"
      )
      expect_identical(counter$calls, 2L)

      counter$calls <- 0L
      expect_error(
        summarize_with_margins(
          data, packed = mixed(.data$x),
          .by = "fixed", .grouping = grouping_set("g")
        ),
        "compatible vectors across groups"
      )
      expect_identical(counter$calls, 2L)
    }
  }
}

test_that("plain frames refuse mixed types in either group order", {
  check_mixed_local_types(identity)
})

test_that("tibbles refuse mixed unnamed summary types in either order", {
  check_mixed_local_types(tibble::as_tibble)
})

test_that("eager data.tables refuse mixed types in either group order", {
  skip_if_suggest_absent("data.table")
  check_mixed_local_types(data.table::as.data.table)
})

check_uniform_local_types <- function(convert) {
  for (rows in list(1:3, c(3L, 1L, 2L))) {
    data <- convert(data.frame(
      fixed = "a", g = c(1L, 1L, 2L), x = 1:3, z = 100L
    )[rows, ])
    counter <- new.env(parent = emptyenv())
    counter$frame <- 0L
    counter$scalar <- 0L
    counter$packed <- 0L
    frame <- function(columns) {
      counter$frame <- counter$frame + 1L
      data.frame(z = sum(columns[[1L]]))
    }
    scalar <- function(columns) {
      counter$scalar <- counter$scalar + 1L
      sum(columns[[1L]])
    }
    packed <- function(columns) {
      counter$packed <- counter$packed + 1L
      tibble::tibble(z = sum(columns[[1L]]))
    }
    actual <- summarize_with_margins(
      data,
      frame(dplyr::pick("x")),
      later = .data$z * 2L,
      scalar(dplyr::pick("x")),
      out = packed(dplyr::pick("x")),
      .by = "fixed", .grouping = rollup("g"), .margin_label = NULL
    )
    expect_identical(
      names(actual),
      c("fixed", "g", "z", "later", 'scalar(dplyr::pick("x"))', "out")
    )
    expect_identical(actual$z, c(3L, 3L, 6L))
    expect_identical(actual$later, c(6L, 6L, 12L))
    expect_identical(actual[['scalar(dplyr::pick("x"))']], c(3L, 3L, 6L))
    expect_s3_class(actual$out, "data.frame")
    expect_identical(actual$out$z, c(3L, 3L, 6L))
    expect_identical(counter$frame, 3L)
    expect_identical(counter$scalar, 3L)
    expect_identical(counter$packed, 3L)
  }
}

test_that("plain frames retain expansion, naming, packing and evaluation", {
  check_uniform_local_types(identity)
})

test_that("tibbles retain expansion, naming, packing and evaluation", {
  check_uniform_local_types(tibble::as_tibble)
})

test_that("eager tables retain expansion, naming, packing and evaluation", {
  skip_if_suggest_absent("data.table")
  check_uniform_local_types(data.table::as.data.table)
})

test_that("original unspecified types remain compatible with later values", {
  scalar <- function(x) if (x == 1L) NA else "ok"
  frame <- function(x) data.frame(z = scalar(x))
  for (rows in list(1:2, 2:1)) {
    data <- data.frame(g = 1:2, x = 1:2)[rows, ]
    expected <- dplyr::summarise(data, scalar(x), frame(x), .by = g)
    actual <- summarize_with_margins(data, scalar(x), frame(x), .by = g)
    expect_identical(names(actual), c("g", "scalar(x)", "z"))
    expect_identical(actual[["scalar(x)"]], expected[["scalar(x)"]])
    expect_identical(actual$z, expected$z)
  }
})
