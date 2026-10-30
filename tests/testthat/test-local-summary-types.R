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
  error <- expect_error(
    summarize_with_margins(
      data, mixed(dplyr::pick(x)), .grouping = grouping_set("g")
    ),
    "Can.t combine"
  )
  expect_match(
    conditionMessage(error),
    "Can't combine `mixed(dplyr::pick(x))`", fixed = TRUE
  )
})

# Exercise the public verb with each input class in a separate test so only
# the data.table case depends on an optional backend (AGENTS.md).
check_mixed_local_types <- function(convert) {
  non_frames <- list(
    integer = 3L, double = 3, character = "3", logical = TRUE,
    list = list(3L), matrix = matrix(3L, nrow = 1L),
    date = as.Date("2026-01-01"),
    missing_integer = NA_integer_, missing_double = NA_real_,
    missing_character = NA_character_
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

test_that("an unspecified group expands with an unnamed frame summary", {
  data <- data.frame(g = c("a", "b", "b"), x = 1:3)
  mixed <- function(x) if (length(x) == 1L) NA else data.frame(z = sum(x))
  expected <- dplyr::summarise(data, mixed(x), .by = g)
  actual <- summarize_with_margins(data, mixed(x), .by = g)
  expect_identical(actual, expected)
})

check_unspecified_frames <- function(convert) {
  for (rows in list(1:3, c(2L, 3L, 1L))) {
    data <- convert(data.frame(
      fixed = "f", g = c("a", "b", "b"), x = 1:3
    )[rows, ])
    counter <- new.env(parent = emptyenv())
    counter$calls <- 0L
    mixed <- function(x) {
      counter$calls <- counter$calls + 1L
      if (length(x) == 1L) NA else convert(data.frame(z = sum(x)))
    }
    summary_env <- rlang::env(mixed = mixed)
    for (expr in list(quote(mixed(x)), quote(mixed(dplyr::pick(x)$x)))) {
      summary <- rlang::new_quosure(expr, env = summary_env)
      for (role in c("by", "included", "fixed", "rollup")) {
        fixed <- if (role == "fixed") "fixed" else character()
        keys <- c(fixed, "g")
        expected <- dplyr::summarise(data, !!summary, .by = dplyr::all_of(keys))
        grouping <- if (role == "rollup") rollup("g") else grouping_set("g")
        if (role == "rollup") {
          total <- dplyr::summarise(data, !!summary)
          total$g <- "Total"
          total <- dplyr::select(total, dplyr::all_of(c(keys, "z")))
          expected <- dplyr::bind_rows(expected, total)
        }
        counter$calls <- 0L
        actual <- if (role == "by") {
          summarize_with_margins(data, !!summary, .by = "g")
        } else {
          summarize_with_margins(
            data, !!summary, .by = dplyr::all_of(fixed), .grouping = grouping
          )
        }
        expect_identical(actual, expected)
        expect_identical(counter$calls, if (role == "rollup") 3L else 2L)
      }

      expected <- dplyr::summarise(data, packed = !!summary, .by = "g")
      counter$calls <- 0L
      actual <- summarize_with_margins(data, packed = !!summary, .by = "g")
      expect_identical(actual, expected)
      expect_identical(counter$calls, 2L)
    }
  }
}

test_that("plain frames expand unspecified/frame summaries in either order", {
  check_unspecified_frames(identity)
})

test_that("tibbles expand unspecified/frame summaries in either order", {
  check_unspecified_frames(tibble::as_tibble)
})

test_that("eager tables expand unspecified/frame summaries in either order", {
  skip_if_suggest_absent("data.table")
  check_unspecified_frames(data.table::as.data.table)
})

test_that("homogeneous unspecified summaries retain their assigned names", {
  data <- data.frame(g = c("a", "b", "b"), x = 1:3)
  missing <- function(x) NA
  expected <- dplyr::summarise(
    data, missing(x), missing(dplyr::pick(x)$x), .by = g
  )
  actual <- summarize_with_margins(
    data, missing(x), missing(dplyr::pick(x)$x), .by = g
  )
  expect_identical(actual, expected)
})
