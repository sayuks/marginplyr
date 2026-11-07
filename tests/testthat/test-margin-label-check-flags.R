# Public consumers of the shared observed Margin label collision check.
label_check_verbs <- list(
  summarize = function(data, ...) {
    summarize_with_margins(data, n = dplyr::n(), .grouping = rollup(g), ...)
  },
  expand = function(data, ...) {
    expand_with_margins(data, .grouping = rollup(g), ...)
  },
  nest = function(data, ...) {
    nest_with_margins(data, .grouping = rollup(g), ...)
  },
  nest_by = function(data, ...) {
    nest_by_with_margins(data, .grouping = rollup(g), ...)
  }
)

test_that("accepted TRUE label checks retain their choice under S3 negation", {
  rlang::local_bindings(
    `!.margin_label_check_flag` = function(x) TRUE,
    .env = asNamespace("base")$.__S3MethodsTable__.
  )
  flag <- structure(TRUE, class = "margin_label_check_flag")
  data <- data.frame(g = "Total", value = 1L)

  for (verb in label_check_verbs) {
    expect_error(
      verb(data, .check_margin_label = flag),
      "already present",
      class = "marginplyr_error"
    )
  }
})

test_that("label checks preserve defaults and ordinary attributed choices", {
  data <- data.frame(g = "Total", value = 1L)
  true_choices <- list(TRUE, c(named = TRUE), matrix(TRUE), I(TRUE))
  false_choices <- list(FALSE, c(named = FALSE), matrix(FALSE), I(FALSE))

  for (verb in label_check_verbs) {
    expect_error(verb(data), "already present", class = "marginplyr_error")
    for (check in true_choices) {
      expect_error(
        verb(data, .check_margin_label = check),
        "already present",
        class = "marginplyr_error"
      )
    }
    for (check in false_choices) {
      result <- verb(data, .check_margin_label = check, .id = "set")
      expect_identical(result$g, c("Total", "Total"))
      expect_identical(result$set, 1:2)
    }
  }
})

test_that("accepted FALSE label checks retain their opt-out under negation", {
  rlang::local_bindings(
    `!.margin_label_check_flag` = function(x) TRUE,
    .env = asNamespace("base")$.__S3MethodsTable__.
  )
  flag <- structure(FALSE, class = "margin_label_check_flag")
  data <- data.frame(g = "Total", value = 1L)

  for (verb in label_check_verbs) {
    result <- verb(data, .check_margin_label = flag, .id = "set")
    expect_identical(result$g, c("Total", "Total"))
    expect_identical(result$set, 1:2)
  }
})

test_that("invalid logical label checks remain refused at admission", {
  rlang::local_bindings(
    `!.margin_label_rejected_flag` = function(x) FALSE,
    .env = asNamespace("base")$.__S3MethodsTable__.
  )
  invalid <- list(
    NULL, logical(), NA, 1, "TRUE", c(TRUE, FALSE), list(TRUE),
    structure(FALSE, class = "margin_label_rejected_flag")
  )
  data <- data.frame(g = "a", value = 1L)

  for (verb in label_check_verbs) {
    for (check in invalid) {
      expect_error(
        verb(data, .check_margin_label = check),
        "must be a logical scalar",
        class = "marginplyr_error"
      )
    }
  }
})

test_that("declared collisions do not depend on the observed label check", {
  data <- data.frame(g = factor("a", levels = c("a", "Total")), value = 1L)
  checks <- list(TRUE, FALSE, c(named = FALSE), matrix(FALSE), I(FALSE))
  for (verb in label_check_verbs) {
    for (check in checks) {
      expect_error(
        verb(data, .check_margin_label = check),
        "already a factor level",
        class = "marginplyr_error"
      )
    }
  }
})

test_that("typed-missing labels remain outside observed collisions", {
  data <- data.frame(g = NA_character_, value = 1L)
  for (verb in label_check_verbs) {
    for (label in list(NULL, NA_character_)) {
      result <- verb(data, .margin_label = label, .check_margin_label = TRUE)
      expect_identical(result$g, c(NA_character_, NA_character_))
    }
  }
})

test_that("an accepted classed TRUE nesting keep choice retains the keys", {
  rlang::local_bindings(
    `!.margin_label_check_flag` = function(x) TRUE,
    .env = asNamespace("base")$.__S3MethodsTable__.
  )
  flag <- structure(TRUE, class = "margin_label_check_flag")
  data <- data.frame(g = "a", value = 1L)
  for (verb in label_check_verbs[c("nest", "nest_by")]) {
    result <- verb(data, .keep = flag)
    for (cell in result$data) {
      expect_identical(names(cell), c("g", "value"))
      expect_identical(cell$g, "a")
    }
  }
})

test_that("lazy label checks preserve their opt-in default and accepted TRUE", {
  skip_if_suggest_absent("dtplyr")
  rlang::local_bindings(
    `!.margin_label_check_flag` = function(x) TRUE,
    .env = asNamespace("base")$.__S3MethodsTable__.
  )
  flag <- structure(TRUE, class = "margin_label_check_flag")
  data <- dtplyr::lazy_dt(data.frame(g = "Total", value = 1L))

  for (verb in label_check_verbs) {
    for (args in list(list(), list(.check_margin_label = FALSE))) {
      result <- dplyr::collect(do.call(verb, c(list(data, .id = "set"), args)))
      expect_identical(result$g, c("Total", "Total"))
      expect_identical(result$set, 1:2)
    }
    expect_error(
      verb(data, .check_margin_label = flag),
      "already present",
      class = "marginplyr_error"
    )
  }
})

expect_stored_factor_levels <- function(as_source) {
  rlang::local_bindings(
    levels.margin_classed_factor = function(x) {
      structure(NextMethod(), class = "margin_classed_levels")
    },
    as.character.margin_classed_levels = function(x, ...) {
      toupper(unclass(x))
    },
    .env = asNamespace("base")$.__S3MethodsTable__.
  )

  plain <- ordered(c("north", NA, "south"), c("south", "north", "unused"))
  classed <- structure(plain, class = c("margin_classed_factor", class(plain)))
  for (key in list(plain, classed)) {
    data <- tibble::tibble(g = key, row = 1:3)
    before <- serialize(data, NULL)
    source <- as_source(data)
    expand <- function(label, checked, position = "last") {
      result <- dplyr::collect(label_check_verbs$expand(
        source, .margin_label = label, .check_margin_label = checked,
        .margin_label_position = position, .id = "set"
      ))
      result[order(result$set, result$row), ]
    }
    for (checked in c(FALSE, TRUE)) {
      for (label in c("south", "unused")) {
        expect_error(
          expand(label, checked), "already a factor level",
          class = "marginplyr_error"
        )
      }

      first <- expand("SOUTH", checked, "first")
      expect_identical(
        first$g,
        structure(
          c(3L, NA_integer_, 2L, 1L, 1L, 1L),
          levels = c("SOUTH", "south", "north", "unused"),
          class = c("ordered", "factor")
        )
      )
      last <- expand("SOUTH", checked)
      expect_identical(
        last$g,
        structure(
          c(2L, NA_integer_, 1L, 4L, 4L, 4L),
          levels = c("south", "north", "unused", "SOUTH"),
          class = c("ordered", "factor")
        )
      )

      for (label in list(NULL, NA_character_)) {
        missing <- expand(label, checked, "first")
        expect_true(is.ordered(missing$g))
        expect_identical(
          attr(missing$g, "levels", exact = TRUE), c("south", "north", "unused")
        )
        expect_identical(as.integer(missing$g), c(2L, NA, 1L, NA, NA, NA))
        expect_identical(missing$set, rep(1:2, each = 3L))
        expect_identical(missing$row, rep(1:3, 2L))
      }
    }
    expect_identical(dplyr::collect(source), data)
    expect_identical(serialize(data, NULL), before)
  }
}

test_that("local expansions check stored factor level strings", {
  expect_stored_factor_levels(identity)
})

test_that("dtplyr expansions check stored factor level strings", {
  skip_if_suggest_absent("dtplyr")
  expect_stored_factor_levels(dtplyr::lazy_dt)
})
