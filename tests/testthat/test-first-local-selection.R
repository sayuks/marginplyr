test_that("a first named across retains forwarded numbered dots", {
  data <- tibble::tibble(x = 1:3)
  wrapper <- function(verb, ...) {
    verb(data, packed = dplyr::across(..1, sum))
  }

  expected <- wrapper(dplyr::summarise, x)
  expect_identical(expected$packed$x, 6L)
  expect_identical(wrapper(summarize_with_margins, x), expected)
})

test_that("a forwarded missing first selector retains dplyr's default", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  data <- tibble::tibble(x = 1:3)
  for (helper in c("across", "if_any", "if_all")) {
    for (spelling in c("..1", "..2", "..01", "..+1", ".. 1", "...")) {
      fn <- if (helper == "across") sum else function(x) sum(x) > 0
      expression <- rlang::call2(helper, rlang::sym(spelling), fn,
                                 .ns = "dplyr")
      wrapper <- function(verb, ...) {
        summary <- rlang::new_quosure(expression, environment())
        verb(data, packed = !!summary)
      }
      invoke <- function(verb) {
        if (spelling == "..2") {
          rlang::inject(wrapper(verb, unused, !!rlang::missing_arg()))
        } else {
          rlang::inject(wrapper(verb, !!rlang::missing_arg()))
        }
      }
      expected <- invoke(dplyr::summarise)
      if (helper == "across") {
        expect_identical(expected$packed$x, 6L)
      } else {
        expect_identical(expected$packed, TRUE)
      }
      expect_identical(invoke(summarize_with_margins), expected,
                       info = paste(helper, spelling))
    }
  }
})

test_that("first named selections retain supported forwarded dots spellings", {
  data <- tibble::tibble(g = c("a", "b"), x = c(3L, 4L))
  across <- dplyr::across
  pick <- dplyr::pick
  if_any <- dplyr::if_any
  if_all <- dplyr::if_all
  helpers <- c("across", "pick", "if_any", "if_all")
  for (helper in helpers) {
    # dplyr's direct pick expansion does not resolve numbered promises.
    spellings <- if (helper == "pick") {
      "..."
    } else {
      c("..1", "..2", "..01", "..+1", ".. 1", "...")
    }
    for (qualified in c(FALSE, TRUE)) {
      head <- if (qualified) rlang::call2("::", quote(dplyr),
                                          rlang::sym(helper)) else helper
      for (spelling in spellings) {
        selector <- rlang::sym(spelling)
        expression <- if (helper == "pick") {
          rlang::call2(head, selector)
        } else {
          fn <- if (helper == "across") sum else function(x) sum(x) > 0
          rlang::call2(head, selector, fn)
        }
        wrapper <- function(verb, ...) {
          summary <- rlang::new_quosure(expression, environment())
          verb(data, packed = !!summary, .by = g)
        }
        run <- function(verb) {
          if (spelling == "..2") wrapper(verb, unused, x) else wrapper(verb, x)
        }
        expected <- run(dplyr::summarise)
        expect_identical(run(summarize_with_margins), expected,
                         info = paste(helper, qualified, spelling))
      }
    }
  }
})

test_that("first forwarded selectors keep their per-group force counts", {
  data <- tibble::tibble(g = c("a", "a", "b"), x = 1:3)
  run <- function(mode, spelling) {
    forced <- integer()
    select <- function() {
      forced <<- c(forced, dplyr::n())
      "x"
    }
    wrapper <- function(verb, grain, ...) {
      expression <- rlang::call2("across", rlang::sym(spelling), quote(sum),
                                 .ns = "dplyr")
      summary <- rlang::new_quosure(expression, environment())
      switch(grain,
             whole = verb(data, packed = !!summary),
             fixed = verb(data, packed = !!summary, .by = g),
             rollup = verb(data, packed = !!summary, .grouping = rollup(g)))
    }
    invoke <- function(verb, grain) {
      if (spelling == "..2") {
        wrapper(verb, grain, stop("unused dot"), select())
      } else {
        wrapper(verb, grain, select())
      }
    }
    values <- if (mode == "dplyr") {
      c(invoke(dplyr::summarise, "fixed")$packed$x,
        invoke(dplyr::summarise, "whole")$packed$x)
    } else {
      invoke(summarize_with_margins, "rollup")$packed$x
    }
    list(values = values, forced = forced)
  }
  for (spelling in c("..1", "..2", "..01", "..+1", ".. 1", "...")) {
    expected <- run("dplyr", spelling)
    expect_identical(expected$values, c(3L, 3L, 6L))
    expect_identical(expected$forced, c(2L, 1L, 3L))
    expect_identical(run("margin", spelling), expected, info = spelling)
  }
})

test_that("missing first numbered selectors retain ordinary diagnostics", {
  data <- tibble::tibble(x = 1:3)
  root_error <- function(expr) {
    error <- expect_error(expr)
    while (!is.null(error$parent)) error <- error$parent
    list(class = class(error), message = conditionMessage(error))
  }
  for (helper in c("across", "if_any", "if_all")) {
    for (spelling in c("..1", "..2", "..01", "..+1", ".. 1")) {
      expression <- rlang::call2(helper, rlang::sym(spelling), quote(sum),
                                 .ns = "dplyr")
      wrapper <- function(verb, ...) {
        summary <- rlang::new_quosure(expression, environment())
        verb(data, packed = !!summary)
      }
      invoke <- function(verb) {
        if (spelling == "..2") wrapper(verb, x) else wrapper(verb)
      }
      expected <- root_error(invoke(dplyr::summarise))
      expect_match(expected$message, "the ... list contains fewer than",
                   fixed = TRUE)
      expect_identical(root_error(invoke(summarize_with_margins)), expected,
                       info = paste(helper, spelling))
    }
  }
})

test_that("unknown first selected columns retain ordinary selection errors", {
  data <- tibble::tibble(x = 1:3)
  root_error <- function(expr) {
    error <- expect_error(expr)
    while (!is.null(error$parent)) error <- error$parent
    list(class = class(error), message = conditionMessage(error))
  }
  expected <- root_error(dplyr::summarise(
    data, packed = dplyr::across(absent, sum)
  ))
  expect_identical(expected$class[[1L]], "vctrs_error_subscript_oob")
  expect_match(expected$message, "Column `absent` doesn't exist", fixed = TRUE)
  for (spelling in c("absent", "..1", "..2", "..01", "..+1", ".. 1", "...")) {
    expression <- rlang::call2("across", rlang::sym(spelling), quote(sum),
                               .ns = "dplyr")
    wrapper <- function(...) {
      summary <- rlang::new_quosure(expression, environment())
      summarize_with_margins(data, packed = !!summary)
    }
    actual <- if (spelling == "..2") {
      root_error(wrapper(unused, absent))
    } else {
      root_error(wrapper(absent))
    }
    expect_identical(actual, expected, info = spelling)
  }
})
