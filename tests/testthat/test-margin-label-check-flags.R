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
