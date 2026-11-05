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

test_that("caller-authored namespace expressions keep root lookup", {
  skip_if_suggest_absent("dtplyr")
  for (operator in c("::", ":::")) {
    for (timing in c("early", "late")) {
      root <- new.env(parent = globalenv())
      root$source <- data.frame(g = c("a", "b"), x = c(1, 3))
      input <- evalq(dtplyr::lazy_dt(source), root)
      observed <- new.env(parent = emptyenv())
      observed$dispatch <- 0L
      callback <- function(pkg, name) {
        package <- as.character(substitute(pkg))
        member <- as.character(substitute(name))
        matched <- if (operator == "::") {
          identical(package, "base") && identical(member, "as.double")
        } else {
          identical(package, "marginplyr") &&
            identical(member, "dtplyr_share_denominator")
        }
        if (matched) {
          observed$dispatch <- observed$dispatch + 1L
          return(if (operator == "::") {
            function(x) x + 4
          } else {
            function(value, identifier, target) sum(value) + 4
          })
        }
        if (operator == "::") {
          getExportedValue(package, member)
        } else {
          getFromNamespace(member, package)
        }
      }
      if (timing == "early") assign(operator, callback, root)
      ordinary <- if (operator == "::") {
        quote(base::as.double(sum(x)))
      } else {
        # Construct the caller's private qualification without an R CMD check
        # NOTE about the package referring to its own private function.
        rlang::call2(
          rlang::call2(":::", quote(marginplyr),
                       quote(dtplyr_share_denominator)),
          quote(x), quote(g), "a"
        )
      }
      query <- summarize_with_margins(
        input, value = !!ordinary, ratio = share_of_total(value),
        .grouping = rollup(g), .sort = "last"
      )
      expect_identical(observed$dispatch, 0L)
      if (timing == "late") assign(operator, callback, root)
      result <- dplyr::collect(query)
      expect_identical(observed$dispatch, 3L)
      expect_identical(result$value, c(5, 7, 8))
      expect_identical(result$ratio, c(0.625, 0.875, 1))
    }
  }
})

test_that("multi-level Parent shares bind generated membership dispatch", {
  skip_if_suggest_absent("dtplyr")
  root <- new.env(parent = globalenv())
  root$source <- data.frame(g = c("a", "a"), h = c("x", "y"), x = c(1, 3))
  input <- evalq(dtplyr::lazy_dt(source), root)
  query <- summarize_with_margins(
    input, value = sum(x), ratio = share_of_parent(value),
    .grouping = rollup(g, h), .sort = "none"
  )
  no_share <- summarize_with_margins(
    input, value = sum(x), .grouping = rollup(g, h), .sort = "none"
  )
  root$`::` <- function(pkg, name) {
    package <- as.character(substitute(pkg))
    member <- as.character(substitute(name))
    if (identical(package, "base") && identical(member, "%in%")) {
      stop("MEMBERSHIP_NAMESPACE_INTERCEPT")
    }
    getExportedValue(package, member)
  }
  expect_identical(dplyr::collect(no_share)$value, c(1, 3, 4, 4))
  result <- dplyr::collect(query)
  expect_identical(result$value, c(1, 3, 4, 4))
  expect_identical(result$ratio, c(0.25, 0.75, 1, 1))
})

test_that("deferred shares isolate early and late namespace dispatchers", {
  skip_if_suggest_absent("dtplyr")
  cases <- expand.grid(
    helper = c("share_of_parent", "share_of_total"),
    fixed = c(FALSE, TRUE), across_source = c(FALSE, TRUE),
    timing = c("early", "late"), operator = c("::", ":::"),
    mode = c("count", "error", "corrupt"), stringsAsFactors = FALSE
  )
  for (index in seq_len(nrow(cases))) {
    case <- cases[index, ]
    info <- paste(unlist(case), collapse = "/")
    root <- new.env(parent = globalenv())
    root$source <- data.frame(
      p = c("A", "A", NA, NA), g = rep(c("a", "b"), 2), x = c(1, 3, 2, 6)
    )
    before <- serialize(root$source, NULL)
    observed <- new.env(parent = emptyenv())
    observed$dispatch <- 0L
    observed$summaries <- 0L
    root$measured_sum <- function(x) {
      observed$summaries <- observed$summaries + 1L
      sum(x)
    }
    input <- evalq(dtplyr::lazy_dt(source), root)
    callback <- function(pkg, name) {
      package <- as.character(substitute(pkg))
      member <- as.character(substitute(name))
      matched <- if (case$operator == "::") {
        identical(package, "base") && identical(member, "as.double")
      } else {
        identical(package, "marginplyr") &&
          identical(member, "dtplyr_share_denominator")
      }
      if (matched) {
        observed$dispatch <- observed$dispatch + 1L
        if (case$mode == "error") stop("NAMESPACE_INTERCEPT")
        if (case$mode == "corrupt") {
          return(if (case$operator == "::") {
            function(x) rep(10, length(x))
          } else {
            function(value, identifier, target) 10
          })
        }
      }
      if (case$operator == "::") {
        getExportedValue(package, member)
      } else {
        getFromNamespace(member, package)
      }
    }
    if (case$timing == "early") assign(case$operator, callback, root)
    ordinary <- if (case$across_source) {
      rlang::quos(dplyr::across(x, measured_sum, .names = "value"))
    } else {
      rlang::quos(value = measured_sum(x))
    }
    ordinary <- lapply(ordinary, rlang::quo_set_env, env = root)
    query <- summarize_with_margins(
      input, !!!ordinary, ratio = !!rlang::call2(case$helper, quote(value)),
      .by = !!if (case$fixed) quote(p) else NULL,
      .grouping = rollup(g), .sort = "last"
    )
    no_share <- summarize_with_margins(
      input, !!!ordinary, .by = !!if (case$fixed) quote(p) else NULL,
      .grouping = rollup(g), .sort = "last"
    )
    expect_true(inherits(query, "dtplyr_step"), info = info)
    expect_identical(observed$summaries, 0L, info = info)
    expect_identical(observed$dispatch, 0L, info = info)
    expect_identical(serialize(root$source, NULL), before, info = info)
    if (case$timing == "late") assign(case$operator, callback, root)
    baseline <- dplyr::collect(no_share)
    expect_identical(observed$dispatch, 0L, info = info)
    result <- dplyr::collect(query)
    expect_identical(observed$dispatch, 0L, info = info)
    expect_identical(result$value, baseline$value, info = info)
    expect_identical(result$value, if (case$fixed) {
      c(1, 3, 4, 2, 6, 8)
    } else {
      c(3, 9, 12)
    }, info = info)
    expect_identical(result$ratio, rep(c(0.25, 0.75, 1),
                                       if (case$fixed) 2 else 1), info = info)
    expect_identical(
      names(result), c(if (case$fixed) "p", "g", "value", "ratio"), info = info
    )
    rm(list = case$operator, envir = root)
    expect_identical(dplyr::collect(query), result, info = info)
    expect_identical(serialize(root$source, NULL), before, info = info)
  }
})

test_that("namespace dispatch cannot admit invalid deferred share sources", {
  skip_if_suggest_absent("dtplyr")
  for (helper in c("share_of_parent", "share_of_total")) {
    for (across_source in c(FALSE, TRUE)) {
      for (timing in c("early", "late")) {
        for (invalid in c("type", "cardinality")) {
          root <- new.env(parent = globalenv())
          root$source <- data.frame(g = c("a", "b"), x = c(1, 3))
          before <- serialize(root$source, NULL)
          input <- evalq(dtplyr::lazy_dt(source), root)
          observed <- new.env(parent = emptyenv())
          observed$dispatch <- 0L
          callback <- function(pkg, name) {
            package <- as.character(substitute(pkg))
            member <- as.character(substitute(name))
            if (identical(package, "marginplyr") && member %in% c(
              "check_dtplyr_share_source", "check_dtplyr_share_scalar"
            )) {
              observed$dispatch <- observed$dispatch + 1L
              return(function(value, ...) 1)
            }
            getFromNamespace(member, package)
          }
          if (timing == "early") root$`:::` <- callback
          direct <- if (invalid == "type") quote(TRUE) else quote(c(1, 2))
          across <- if (invalid == "type") quote(~ TRUE) else quote(~ c(1, 2))
          ordinary <- if (across_source) {
            rlang::quos(dplyr::across(x, !!across, .names = "value"))
          } else {
            rlang::quos(value = !!direct)
          }
          query <- summarize_with_margins(
            input, !!!ordinary, ratio = !!rlang::call2(helper, quote(value)),
            .grouping = rollup(g), .sort = "last"
          )
          expect_s3_class(query, "dtplyr_step")
          expect_identical(observed$dispatch, 0L)
          if (timing == "late") root$`:::` <- callback
          error <- expect_error(dplyr::collect(query), class = if (
            invalid == "type"
          ) "marginplyr_error" else "marginplyr_share_cardinality_error")
          expect_identical(error$share_output, "ratio")
          expect_identical(error$source_summary, "value")
          expect_identical(rlang::call_name(conditionCall(error)),
                           "summarize_with_margins")
          expect_identical(observed$dispatch, 0L)
          expect_identical(serialize(root$source, NULL), before)
        }
      }
    }
  }
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
      input, !!!ordinary, extra = sum(x) + 1, ratio = share_of_total(value),
      .grouping = rollup(g), .sort = "last"
    )
    root$`:::` <- function(pkg, name) {
      package <- as.character(substitute(pkg))
      member <- as.character(substitute(name))
      if (identical(package, "marginplyr") && member %in% c(
        "check_dtplyr_share_source", "check_dtplyr_share_scalar",
        "dtplyr_share_input_name", "new_share_validation_mapping",
        "check_dtplyr_share_ordinary", "assert_dtplyr_grouped_keys"
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
