test_that("summary selectors retain R's numeric dots spellings", {
  pick <- dplyr::pick
  across <- dplyr::across
  if_any <- dplyr::if_any
  if_all <- dplyr::if_all
  fixture <- data.frame(g = c("a", "a", "b"), x = 1:3, y = 4:6)
  inputs <- list(fixture, tibble::as_tibble(fixture))
  if (suggest_available("data.table")) {
    inputs <- c(inputs, list(data.table::as.data.table(fixture)))
  }
  references <- c("..1", "..2", "..01", "..001", "..02", "..+1", ".. 1",
                  "..\t1", ".. +02")
  for (helper in c("pick", "across", "if_any", "if_all")) {
    for (qualified in c(FALSE, TRUE)) {
      for (reference in references) {
        call <- rlang::call2(helper, as.name(reference),
                             .ns = if (qualified) "dplyr" else NULL)
        if (helper != "pick") {
          call$.fns <- if (helper == "across") quote(sum) else quote(~ .x > 1)
        }
        expression <- rlang::expr((function(...) sum(!!call))(x, y))
        for (data in inputs) {
          for (context in c("none", "fixed", "rollup")) {
            expected <- if (context == "none") {
              dplyr::summarise(data, value = !!expression)
            } else {
              detail <- dplyr::summarise(data, value = !!expression, .by = g)
              if (context == "fixed") detail else dplyr::bind_rows(
                detail, dplyr::mutate(
                  dplyr::summarise(data, value = !!expression), g = "Total"
                )
              )
            }
            actual <- switch(
              context,
              none = summarize_with_margins(data, value = !!expression),
              fixed = summarize_with_margins(
                data, value = !!expression, .by = g
              ),
              rollup = summarize_with_margins(
                data, value = !!expression, .grouping = rollup(g)
              )
            )
            expect_identical(as.data.frame(actual), as.data.frame(expected),
                             info = paste(helper, qualified, reference,
                                          class(data)[[1L]], context))
          }
        }
      }
    }
  }
})

test_that("numeric dots preserve dynamic unpacking factory boundaries", {
  across <- dplyr::across
  data <- data.frame(g = c("a", "a", "b"), x = 1:3)
  inputs <- list(data, tibble::as_tibble(data))
  if (suggest_available("data.table")) {
    inputs <- c(inputs, list(data.table::as.data.table(data)))
  }
  run <- function(verb, reference, qualified, unpack, context) {
    calls <- 0L
    factory <- function() {
      calls <<- calls + 1L
      offset <- calls
      function(x) data.frame(value = sum(x) + offset)
    }
    call <- rlang::call2("across", quote(x), as.name(reference),
                         .unpack = quote(unpack),
                         .ns = if (qualified) "dplyr" else NULL)
    body <- switch(
      context,
      none = rlang::expr(verb(data, !!call)),
      fixed = rlang::expr(verb(data, !!call, .by = g)),
      rollup = rlang::expr(dplyr::bind_rows(
        verb(data, !!call, .by = g),
        dplyr::mutate(verb(data, !!call), g = "Total")
      ))
    )
    if (context == "rollup" && identical(verb, summarize_with_margins)) {
      body <- rlang::expr(verb(data, !!call, .grouping = rollup(g)))
    }
    wrapper <- rlang::new_function(alist(... = ), body, environment())
    list(result = as.data.frame(wrapper(factory(), factory())), calls = calls)
  }
  control <- run(dplyr::summarise, "..1", TRUE, TRUE, "fixed")
  expect_identical(control$calls, 2L)
  expect_identical(control$result$x_value, c(5L, 4L))
  for (data in inputs) {
    for (reference in c("..1", "..2", "..01", "..001", "..02", "..+1", ".. 1",
                        "..\t1")) {
      for (qualified in c(FALSE, TRUE)) {
        for (unpack in list(TRUE, "{inner}_{outer}", FALSE)) {
          for (context in c("none", "fixed", "rollup")) {
            expected <- run(dplyr::summarise, reference, qualified,
                            unpack, context)
            actual <- run(summarize_with_margins, reference, qualified,
                          unpack, context)
            expect_identical(actual, expected, info = paste(
              reference, qualified, unpack, context, class(data)[[1L]]
            ))
          }
        }
      }
    }
  }
})

test_that("invalid dots and ordinary names retain dplyr selection lookup", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  data <- data.frame(x = 1:3, y = 4:6)
  root_error <- function(expr) {
    cnd <- expect_error(expr)
    while (!is.null(cnd$parent)) cnd <- cnd$parent
    list(class = class(cnd), message = conditionMessage(cnd))
  }
  for (helper in c("pick", "across", "if_any", "if_all")) {
    for (reference in c("..0", "..-1", "..3", "..03", "..1x", "..1 ")) {
      call <- rlang::call2(helper, as.name(reference), .ns = "dplyr")
      if (helper != "pick") call$.fns <- quote(sum)
      expression <- rlang::expr((function(...) sum(!!call))(x, y))
      expected <- root_error(dplyr::summarise(data, value = !!expression))
      actual <- root_error(summarize_with_margins(data, value = !!expression))
      expect_identical(actual, expected, info = paste(helper, reference))
    }
  }
  columns <- "y"
  rlang::env_bind(rlang::current_env(), !!!stats::setNames(list("y"), "..1x"))
  expressions <- rlang::exprs(
    (function(...) sum(dplyr::pick(..01)))(.env$columns),
    (function(...) sum(dplyr::pick(..02)))(stop("unused"), x),
    sum(dplyr::pick(..1x))
  )
  for (expression in expressions) {
    expect_identical(
      summarize_with_margins(data, value = !!expression),
      dplyr::summarise(data, value = !!expression)
    )
  }
  data[["..1x"]] <- 7:9
  expect_identical(
    summarize_with_margins(data, value = sum(dplyr::pick(..1x)))$value, 24L
  )
})

test_that("numeric dots are captured only when their summary executes", {
  data <- data.frame(g = c("a", "a", "b"), x = 1:3)
  run <- function(verb) {
    state <- rlang::env(events = character())
    columns <- function() {
      state$events <- c(state$events, "columns")
      "x"
    }
    result <- verb(data, value = (function(...) {
      state$events <- c(state$events, "body")
      sum(dplyr::pick(..01))
    })(columns()), .by = g)
    list(result = result, events = state$events)
  }
  expected <- run(dplyr::summarise)
  expect_identical(expected$events, rep(c("body", "columns"), 2L))
  expect_identical(run(summarize_with_margins), expected)
})
