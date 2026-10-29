# Exercise dispatch through the public selection consumers; the direct typed
# tidyselect result and literal source rows are independent expectations.
expect_dispatched_selection <- function(data, selection, counter) {
  expected <- names(tidyselect::eval_select(
    selection, tibble::tibble(region = c("east", "west"), amount = 1:2)
  ))
  expect_identical(expected, "region")

  counter$n <- 0L
  expect_identical(
    inspect_grouping(data, .by = !!selection, .format = "list")$fixed,
    list("region")
  )
  expect_identical(counter$n, 1L)

  for (constructor in c("grouping_set", "grouping_sets", "rollup", "cube",
                        "grouping_spec")) {
    spec_call <- if (constructor == "grouping_sets") {
      rlang::call2(constructor, selection, rlang::call2("grouping_set"))
    } else {
      rlang::call2(constructor, selection)
    }
    spec <- rlang::eval_tidy(spec_call)
    counter$n <- 0L
    plan <- inspect_grouping(data, .grouping = spec, .format = "list")
    included <- if (constructor %in% c("rollup", "cube", "grouping_sets")) {
      list("region", character())
    } else {
      list("region")
    }
    expect_identical(plan$included, included)
    expect_identical(counter$n, 1L)
  }

  counter$n <- 0L
  summary <- summarize_with_margins(data, n = dplyr::n(), .by = !!selection)
  expect_identical(
    dplyr::collect(summary),
    tibble::tibble(region = c("east", "west"), n = c(1L, 1L))
  )
  expect_identical(counter$n, 1L)

  for (verb in c("nest_with_margins", "nest_by_with_margins")) {
    counter$n <- 0L
    result <- rlang::eval_tidy(rlang::call2(verb, data, .by = selection))
    result <- dplyr::ungroup(dplyr::collect(result))
    expect_identical(result$region, c("east", "west"))
    expect_identical(
      lapply(result$data, tibble::as_tibble),
      list(tibble::tibble(amount = 1L), tibble::tibble(amount = 2L))
    )
    expect_identical(counter$n, 1L)
  }
}

check_implicit_dispatch <- function(data) {
  shapes <- list(
    matrix = matrix(1L), array = array(1L, c(1L, 1L, 1L)),
    character = "value", numeric = 1, integer = 1L, logical = TRUE
  )
  for (generic in c("format", "unique", "sort")) {
    for (shape in names(shapes)) {
      counter <- rlang::env(n = 0L)
      env <- rlang::env(selector = shapes[[shape]], counter = counter)
      method <- rlang::new_function(
        alist(x = , ... = ),
        quote({
          counter$n <- counter$n + 1L
          names(tidyselect::peek_data())[
            vapply(tidyselect::peek_data(), is.character, logical(1))
          ]
        }),
        env = env
      )
      env[[paste0(generic, ".", shape)]] <- method
      expect_false(is.object(env$selector))
      env$explicit <- structure(env$selector, class = class(env$selector))
      env$opaque <- rlang::new_function(
        alist(), rlang::call2("do.call", generic, quote(list(selector))),
        env = env
      )
      arguments <- list(
        rlang::call2(generic, quote(selector)),
        rlang::call2(generic, quote(explicit)),
        quote(opaque())
      )
      for (argument in arguments) {
        selection <- rlang::new_quosure(
          rlang::call2("all_of", argument, .ns = "tidyselect"), env
        )
        expect_dispatched_selection(data, selection, counter)
      }
    }
  }
}

check_injected_dispatch <- function(data) {
  counter <- rlang::env(n = 0L)
  selector <- structure(1L, class = "margin_type_selector")
  cast_selector <- function(x, to, ...) {
    counter$n <- counter$n + 1L
    which(vapply(tidyselect::peek_data(), is.character, logical(1)))
  }
  selector_ptype <- function(x, y, ...) integer()
  registerS3method("vec_cast", "integer.margin_type_selector",
                   cast_selector,
                   envir = asNamespace("vctrs"))
  registerS3method("vec_ptype2", "integer.margin_type_selector",
                   selector_ptype,
                   envir = asNamespace("vctrs"))
  registerS3method("vec_ptype2", "margin_type_selector.integer",
                   selector_ptype,
                   envir = asNamespace("vctrs"))
  selections <- list(
    rlang::quo(!!selector),
    rlang::quo(c(!!selector)),
    rlang::quo(tidyselect::all_of(selector))
  )
  for (selection in selections) {
    expect_dispatched_selection(data, selection, counter)
  }
}

test_that("implicit S3 dispatch selects typed local Grouping columns", {
  check_implicit_dispatch(
    tibble::tibble(region = c("east", "west"), amount = 1:2)
  )
})

test_that("implicit S3 dispatch selects typed dtplyr Grouping columns", {
  skip_if_suggest_absent("dtplyr")
  check_implicit_dispatch(
    dtplyr::lazy_dt(tibble::tibble(region = c("east", "west"), amount = 1:2))
  )
})

test_that("injected selectors retain vctrs dispatch on local inputs", {
  check_injected_dispatch(
    tibble::tibble(region = c("east", "west"), amount = 1:2)
  )
})

test_that("injected selectors retain vctrs dispatch on dtplyr inputs", {
  skip_if_suggest_absent("dtplyr")
  check_injected_dispatch(
    dtplyr::lazy_dt(tibble::tibble(region = c("east", "west"), amount = 1:2))
  )
})
