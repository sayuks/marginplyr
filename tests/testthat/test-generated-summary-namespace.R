test_that("generated local selections ignore caller namespace operators", {
  data <- data.frame(x = 1:3, y = 10:12)
  cases <- list(
    list("::", "base", "quote", function(...) as.name("y")),
    list("::", "tidyselect", "all_of", function(...) "y"),
    list(":::", "marginplyr", "local_summary_selection", function(...) "y")
  )
  for (case in cases) {
    hits <- 0L
    caller <- rlang::env()
    caller[[case[[1L]]]] <- function(pkg, name) {
      pkg <- as.character(substitute(pkg))
      name <- as.character(substitute(name))
      if (identical(pkg, case[[2L]]) && identical(name, case[[3L]])) {
        hits <<- hits + 1L
        return(case[[4L]])
      }
      get(name, envir = asNamespace(pkg))
    }
    summary <- rlang::new_quosure(quote(sum(pick(x))), caller)
    actual <- summarize_with_margins(data, answer = !!summary)
    expect_identical(actual$answer, 6L)
    expect_identical(hits, 0L)
  }
})

test_that("caller-authored namespace calls keep ordinary R lookup", {
  data <- data.frame(x = 1:3)
  for (operator in c("::", ":::")) {
    hits <- 0L
    caller <- rlang::env()
    caller[[operator]] <- function(pkg, name) {
      pkg <- as.character(substitute(pkg))
      name <- as.character(substitute(name))
      if (identical(pkg, "base") && identical(name, "sum")) {
        hits <<- hits + 1L
        return(function(...) sum(...) + 10L)
      }
      get(name, envir = asNamespace(pkg))
    }
    ordinary_call <- rlang::call2(
      rlang::call2(operator, quote(base), quote(sum)), quote(x)
    )
    summary <- rlang::new_quosure(
      rlang::expr(sum(pick(x)) + !!ordinary_call), caller
    )
    actual <- summarize_with_margins(data, answer = !!summary)
    expect_identical(actual$answer, 22L)
    expect_identical(hits, 1L)
  }
})

test_that("nested bare selections execute their package-owned helper", {
  data <- data.frame(x = 1:3)
  expressions <- rlang::exprs(
    pick = (function() sum(pick(x)))(),
    across = (function() sum(across(x, sum)))(),
    if_any = (function() sum(if_any(x, ~ .x > 0L)))(),
    if_all = (function() sum(if_all(x, ~ .x > 0L)))()
  )
  expected <- c(pick = 6L, across = 6L, if_any = 3L, if_all = 3L)
  for (helper in names(expressions)) {
    hits <- 0L
    caller <- rlang::env()
    caller[[helper]] <- function(...) 88L
    caller[["::"]] <- function(pkg, name) {
      pkg <- as.character(substitute(pkg))
      name <- as.character(substitute(name))
      if (identical(pkg, "dplyr") && identical(name, helper)) {
        hits <<- hits + 1L
        return(function(...) 99L)
      }
      getExportedValue(pkg, name)
    }
    summary <- rlang::new_quosure(expressions[[helper]], caller)
    actual <- summarize_with_margins(data, answer = !!summary)
    expect_identical(actual$answer, expected[[helper]])
    expect_identical(hits, 0L)
  }
})

test_that("generated nested data masks ignore caller namespace operators", {
  data <- data.frame(x = 1:3)
  hits <- 0L
  caller <- rlang::env()
  caller[[":::"]] <- function(pkg, name) {
    pkg <- as.character(substitute(pkg))
    name <- as.character(substitute(name))
    if (identical(pkg, "marginplyr") && identical(name, "nested_mask_thunk")) {
      hits <<- hits + 1L
      return(function(...) function() data.frame(z = 99L))
    }
    get(name, envir = asNamespace(pkg))
  }
  summary <- rlang::new_quosure(
    quote(sum(dplyr::mutate(data.frame(z = x), z = z)$z)), caller
  )
  actual <- summarize_with_margins(data, answer = !!summary)
  expect_identical(actual$answer, 6L)
  expect_identical(hits, 0L)
})

test_that("forwarded local across ignores caller namespace operators", {
  data <- data.frame(x = 1:3)
  wrapper <- function(verb, ...) {
    `:::` <- function(pkg, name) {
      pkg <- as.character(substitute(pkg))
      name <- as.character(substitute(name))
      if (identical(pkg, "marginplyr")) {
        stop("Caller namespace operator reached a generated selection.")
      }
      get(name, envir = asNamespace(pkg))
    }
    verb(data, packed = dplyr::across(..1, sum))
  }
  expected <- wrapper(dplyr::summarise, x)
  expect_identical(expected$packed$x, 6L)
  expect_identical(wrapper(summarize_with_margins, x), expected)
})

test_that("forwarded local pick ignores caller namespace operators", {
  data <- data.frame(x = 1:3)
  wrapper <- function(verb) {
    `:::` <- function(pkg, name) {
      pkg <- as.character(substitute(pkg))
      name <- as.character(substitute(name))
      if (identical(pkg, "marginplyr")) {
        stop("Caller namespace operator reached a generated selection.")
      }
      get(name, envir = asNamespace(pkg))
    }
    verb(data, answer = (function(...) sum(dplyr::pick(..1)))(x))
  }
  expected <- wrapper(dplyr::summarise)
  expect_identical(expected$answer, 6L)
  expect_identical(wrapper(summarize_with_margins), expected)
})

test_that("planned selector warnings ignore caller namespace operators", {
  data <- data.frame(g = c("a", "b"), x = c(1L, 3L))
  `:::` <- function(pkg, name) {
    pkg <- as.character(substitute(pkg))
    name <- as.character(substitute(name))
    if (identical(pkg, "marginplyr") &&
          identical(name, "replay_local_selector_warnings")) {
      stop("Caller namespace operator reached generated warning replay.")
    }
    get(name, envir = asNamespace(pkg))
  }
  output_name <- function() "unused"
  choose <- function() {
    warning("selector warning")
    "x"
  }
  expect_warning(actual <- summarize_with_margins(
    data,
    dplyr::across(dplyr::all_of(choose()), sum, .names = output_name()),
    ordinary = sum(x),
    dplyr::across(dplyr::starts_with("ordinary"), share_of_total,
                  .names = "share"),
    .grouping = rollup(g)
  ), "selector warning")
  expect_identical(actual$unused, c(1L, 3L, 4L))
  expect_identical(actual$share, c(0.25, 0.75, 1))
})

test_that("planned across functions ignore caller namespace operators", {
  data <- data.frame(g = c("a", "b"), x = c(1L, 3L))
  `:::` <- function(pkg, name) {
    pkg <- as.character(substitute(pkg))
    name <- as.character(substitute(name))
    if (identical(pkg, "marginplyr") &&
          identical(name, "first_local_across_fns")) {
      return(function(...) function(...) 99L)
    }
    get(name, envir = asNamespace(pkg))
  }
  calls <- 0L
  factory <- function() {
    calls <<- calls + 1L
    function(x) sum(x)
  }
  output_name <- function() "unused"
  actual <- summarize_with_margins(
    data, dplyr::across(x, factory(), .names = output_name()),
    ordinary = sum(x),
    dplyr::across(dplyr::starts_with("ordinary"), share_of_total,
                  .names = "share"),
    .grouping = rollup(g)
  )
  expect_identical(actual$unused, c(1L, 3L, 4L))
  expect_identical(actual$share, c(0.25, 0.75, 1))
  expect_identical(calls, 2L)
})
