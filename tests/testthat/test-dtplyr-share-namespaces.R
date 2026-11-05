test_that("dtplyr shares retain displayed ratios under namespace dispatch", {
  skip_if_suggest_absent("dtplyr")
  root <- new.env(parent = globalenv())
  root$source <- data.frame(g = c("a", "b"), x = c(1, 3))
  input <- evalq(dtplyr::lazy_dt(source), root)
  query <- summarize_with_margins(
    input, value = sum(x), ratio = share_of_total(value),
    .grouping = rollup(g), .sort = "last"
  )
  root$`::` <- function(pkg, name) {
    package <- as.character(substitute(pkg))
    member <- as.character(substitute(name))
    if (identical(package, "base") && identical(member, "as.double")) {
      return(function(x) rep(10, length(x)))
    }
    getExportedValue(package, member)
  }
  result <- dplyr::collect(query)
  expect_identical(result$value, c(1, 3, 4))
  expect_identical(result$ratio, c(0.25, 0.75, 1))
})

test_that("dtplyr validates share sources without caller namespace callbacks", {
  skip_if_suggest_absent("dtplyr")
  for (across_source in c(FALSE, TRUE)) {
    root <- new.env(parent = globalenv())
    root$source <- data.frame(g = c("a", "b"), x = c(1, 3))
    input <- evalq(dtplyr::lazy_dt(source), root)
    ordinary <- if (across_source) {
      rlang::quos(dplyr::across(x, sum, .names = "value"))
    } else {
      rlang::quos(value = sum(x))
    }
    query <- summarize_with_margins(
      input, !!!ordinary, ratio = share_of_total(value),
      .grouping = rollup(g), .sort = "last"
    )
    root$`:::` <- function(pkg, name) {
      package <- as.character(substitute(pkg))
      member <- as.character(substitute(name))
      if (identical(package, "marginplyr") && member %in% c(
        "check_dtplyr_share_source", "check_dtplyr_share_scalar",
        "dtplyr_share_input_name", "new_share_validation_mapping"
      )) {
        stop("SOURCE_NAMESPACE_INTERCEPT")
      }
      getFromNamespace(member, package)
    }
    result <- dplyr::collect(query)
    expect_identical(result$value, c(1, 3, 4))
    expect_identical(result$ratio, c(0.25, 0.75, 1))
  }
})

test_that("dtplyr shares select their displayed denominator without dispatch", {
  skip_if_suggest_absent("dtplyr")
  root <- new.env(parent = globalenv())
  root$source <- data.frame(g = c("a", "b"), x = c(1, 3))
  input <- evalq(dtplyr::lazy_dt(source), root)
  query <- summarize_with_margins(
    input, value = sum(x), ratio = share_of_parent(value),
    .grouping = rollup(g), .sort = "last"
  )
  root$`:::` <- function(pkg, name) {
    package <- as.character(substitute(pkg))
    member <- as.character(substitute(name))
    if (identical(package, "marginplyr") &&
          identical(member, "dtplyr_share_denominator")) {
      return(function(value, identifier, target) 10)
    }
    getFromNamespace(member, package)
  }
  result <- dplyr::collect(query)
  expect_identical(result$value, c(1, 3, 4))
  expect_identical(result$ratio, c(0.25, 0.75, 1))
})
