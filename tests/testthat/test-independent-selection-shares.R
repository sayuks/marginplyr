expect_local_selection_shares <- function(data, naming, indirect) {
  pattern <- if (indirect) {
    rlang::new_quosure(
      quote(dplyr::starts_with(prefix)), rlang::env(prefix = "total")
    )
  } else {
    quote(dplyr::starts_with("total"))
  }
  for (multiple in c(FALSE, TRUE)) {
    grouping <- if (multiple) rollup(dplyr::all_of("g")) else grouping_set()
    helpers <- c("share_of_total", if (multiple) "share_of_parent")
    for (helper in helpers) {
      for (callback in c(FALSE, TRUE)) {
        run <- function(shared) {
          state <- new.env(parent = emptyenv())
          state$contexts <- integer()
          state$choose <- function() {
            state$contexts <- c(state$contexts, dplyr::n())
            if (dplyr::n() == 0L) "x" else "y"
          }
          selection <- if (callback) {
            quote(dplyr::all_of(state$choose()))
          } else {
            quote(dplyr::all_of({
              state$contexts <- c(state$contexts, dplyr::n())
              if (dplyr::n() == 0L) "x" else "y"
            }))
          }
          ordinary <- list(
            rlang::expr(dplyr::across(!!selection, sum, .names = !!naming)),
            total = quote(sum(y))
          )
          if (shared) {
            result <- summarize_with_margins(
              data, !!!ordinary,
              dplyr::across(!!pattern, !!rlang::sym(helper),
                            .names = "share"),
              .grouping = grouping
            )
          } else if (multiple) {
            result <- dplyr::bind_rows(
              dplyr::summarise(data, !!!ordinary, .by = dplyr::all_of("g")),
              dplyr::mutate(dplyr::summarise(data, !!!ordinary), g = "Total")
            )
          } else {
            result <- dplyr::summarise(data, !!!ordinary)
          }
          list(result = as.data.frame(result), contexts = state$contexts)
        }
        control <- run(FALSE)
        actual <- run(TRUE)
        expect_identical(actual$contexts, control$contexts)
        expect_length(actual$contexts, if (multiple) 2L else 1L)
        expect_identical(actual$result[names(control$result)], control$result)
        ordinary_name <- setdiff(names(control$result), c("g", "total"))[[1L]]
        expect_identical(actual$result[[ordinary_name]],
                         if (multiple) c(3L, 3L, 6L) else 6L)
        expect_equal(actual$result$share,
                     if (multiple) c(11 / 18, 7 / 18, 1) else 1)
        expect_type(actual$result$share, "double")
      }
    }
  }
}

expect_local_share_patterns <- function(data) {
  for (naming in list(
    "unused", "{.col}_unused", quote(paste0("un", "used")),
    quote(paste0("unused_", dplyr::n()))
  )) {
    for (indirect in c(FALSE, TRUE)) {
      expect_local_selection_shares(data, naming, indirect)
    }
  }
}

test_that("independent selection shares preserve local selector evaluation", {
  data <- data.frame(g = c("a", "a", "b"), x = 1:3, y = c(5, 6, 7))
  expect_local_share_patterns(data)
  expect_local_share_patterns(tibble::as_tibble(data))
  expect_local_share_patterns(structure(
    data, class = c("local_frame", "data.frame")
  ))
})

test_that("independent selection shares preserve eager data.table selectors", {
  skip_if_suggest_absent("data.table")
  data <- data.table::data.table(
    g = c("a", "a", "b"), x = 1:3, y = c(5, 6, 7)
  )
  expect_local_share_patterns(data)
})

test_that("independent selection shares preserve geometry-key selectors", {
  skip_if_suggest_absent("sf")
  data <- sf::st_as_sf(data.frame(
    g = c("a", "a", "b"), x = 1:3, y = c(5, 6, 7), lon = 0, lat = 0
  ), coords = c("lon", "lat"))
  contexts <- integer()
  choose <- function() {
    contexts <<- c(contexts, dplyr::n())
    "x"
  }
  ordinary <- rlang::quos(
    dplyr::across(dplyr::all_of(choose()), sum, .names = "unused"),
    total = sum(y)
  )
  control <- dplyr::summarise(data, !!!ordinary, .by = geometry)
  expected_contexts <- contexts
  contexts <- integer()
  result <- summarize_with_margins(
    data, !!!ordinary,
    dplyr::across(dplyr::starts_with("total"), share_of_total,
                  .names = "share"),
    .by = geometry
  )
  expect_identical(contexts, expected_contexts)
  expect_length(contexts, 1L)
  expect_identical(result$unused, control$unused)
  expect_identical(result$total, control$total)
  expect_identical(result$geometry, control$geometry)
  expect_identical(result$share, 1)
})

test_that("name patterns leave a stateful first selector in each branch", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), y = c(10, 30))
  for (selection in rlang::exprs(
    dplyr::starts_with("total"), dplyr::ends_with("otal"),
    dplyr::contains("ota"), dplyr::matches("^total$")
  )) {
    calls <- 0L
    choose <- function() {
      calls <<- calls + 1L
      if (calls == 1L) "x" else "y"
    }
    result <- summarize_with_margins(
      data, dplyr::across(dplyr::all_of(choose()), sum, .names = "unused"),
      total = sum(x),
      dplyr::across(!!selection, share_of_total, .names = "share"),
      .grouping = rollup(g)
    )
    expect_identical(calls, 2L)
    expect_identical(result$unused, c(1, 3, 40))
    expect_identical(result$total, c(1, 3, 4))
    expect_identical(result$share, c(0.25, 0.75, 1))
  }
})

test_that("first selectors retain dplyr's selection data context", {
  data <- data.frame(g = c("a", "a", "b"), x = 1:3, y = c(5, 6, 7))
  run <- function(shared) {
    observed <- new.env(parent = emptyenv())
    observed$classes <- character()
    choose <- function() {
      proxy <- tidyselect::peek_data()
      observed$classes <- c(observed$classes, class(proxy))
      if (is.data.frame(proxy)) "x" else "y"
    }
    ordinary <- rlang::quos(
      dplyr::across(dplyr::all_of(choose()), sum, .names = "unused"),
      total = sum(y)
    )
    result <- if (shared) {
      summarize_with_margins(
        data, !!!ordinary,
        dplyr::across(dplyr::starts_with("total"), share_of_total,
                      .names = "share"),
        .grouping = rollup(g)
      )
    } else {
      dplyr::bind_rows(
        dplyr::summarise(data, !!!ordinary, .by = g),
        dplyr::mutate(dplyr::summarise(data, !!!ordinary), g = "Total")
      )
    }
    list(result = result, classes = observed$classes)
  }
  control <- run(FALSE)
  actual <- run(TRUE)
  expect_identical(actual$classes, control$classes)
  expect_identical(actual$result[names(control$result)], control$result)
  expect_identical(actual$result$unused, c(11, 7, 18))
  expect_equal(actual$result$share, c(11 / 18, 7 / 18, 1))
})

test_that("first selectors follow functions beside selection shares", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), y = c(10, 30))
  run <- function(shared) {
    events <- character()
    ready <- FALSE
    factory <- function() {
      events <<- c(events, paste0("function:", dplyr::n()))
      ready <<- TRUE
      sum
    }
    choose <- function() {
      events <<- c(events, paste0("selector:", dplyr::n()))
      if (ready) "x" else "y"
    }
    ordinary <- rlang::quos(
      dplyr::across(dplyr::all_of(choose()), factory(), .names = "unused"),
      total = sum(x)
    )
    result <- if (shared) {
      summarize_with_margins(
        data, !!!ordinary,
        dplyr::across(dplyr::starts_with("total"), share_of_total,
                      .names = "share"),
        .grouping = rollup(g)
      )
    } else {
      dplyr::bind_rows(
        dplyr::summarise(data, !!!ordinary, .by = g),
        dplyr::mutate(dplyr::summarise(data, !!!ordinary), g = "Total")
      )
    }
    list(result = result, events = events)
  }
  control <- run(FALSE)
  actual <- run(TRUE)
  expect_identical(actual$events, control$events)
  expect_identical(actual$events,
                   rep(c("function:0", "selector:0"), 2L))
  expect_identical(actual$result[names(control$result)], control$result)
  expect_identical(actual$result$unused, c(1, 3, 4))
  expect_identical(actual$result$share, c(0.25, 0.75, 1))
})

test_that("cached first functions retain the local summary mask", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), y = c(10, 30))
  for (fn in rlang::exprs(
    if (dplyr::n() == 0L) function(x) sum(y) else function(x) -1,
    if (dplyr::n() == 0L) ~sum(y) else ~-1,
    list(a = function(x) sum(y))
  )) {
    result <- summarize_with_margins(
      data, dplyr::across(x, !!fn, .names = "unused"), total = sum(x),
      dplyr::across(dplyr::starts_with("total"), share_of_total,
                    .names = "share"),
      .grouping = rollup(g)
    )
    expect_identical(result$unused, c(10, 30, 40))
    expect_identical(result$share, c(0.25, 0.75, 1))
  }
})

test_that("function evaluation does not promote unresolved share sources", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  for (explicit in c(FALSE, TRUE)) {
    caller <- rlang::env(rlang::current_env(), calls = 0L)
    delayedAssign("fn", {
      caller$calls <- caller$calls + 1L
      sum
    }, assign.env = caller)
    ordinary <- rlang::new_quosure(
      quote(dplyr::across(x, fn, .names = "s")), caller
    )
    if (explicit) {
      expect_error(summarize_with_margins(
        data, !!ordinary,
        dplyr::across(s, share_of_total, .names = "share"),
        .grouping = rollup(g)
      ), "unknown", class = "marginplyr_error")
      expect_identical(caller$calls, 0L)
    } else {
      result <- summarize_with_margins(
        data, !!ordinary,
        dplyr::across(dplyr::starts_with("s"), share_of_total,
                      .names = "share"),
        .grouping = rollup(g)
      )
      expect_identical(names(result), c("g", "s"))
      expect_identical(result$s, c(1, 3, 4))
      expect_identical(caller$calls, 1L)
    }
  }
})

test_that("selection shares retain matching across sources and naming", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), y = c(10, 30))
  for (dynamic in c(FALSE, TRUE)) {
    for (helper in c("share_of_total", "share_of_parent")) {
      reads <- 0L
      template <- function() {
        reads <<- reads + 1L
        "total_{.col}"
      }
      naming <- if (dynamic) quote(template()) else "total_{.col}"
      result <- summarize_with_margins(
        data, dplyr::across(c(x, y), sum, .names = !!naming),
        dplyr::across(dplyr::starts_with("total"), !!rlang::sym(helper),
                      .names = "{.col}_share"),
        .grouping = rollup(g)
      )
      expect_identical(reads, as.integer(dynamic))
      expect_identical(result$total_x, c(1, 3, 4))
      expect_identical(result$total_y, c(10, 30, 40))
      expect_identical(result$total_x_share, c(0.25, 0.75, 1))
      expect_identical(result$total_y_share, c(0.25, 0.75, 1))
    }
  }
})

test_that("independent selection shares retain source and collision guards", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  for (helper in c("share_of_total", "share_of_parent")) {
    share <- rlang::expr(dplyr::across(
      dplyr::starts_with("total"), !!rlang::sym(helper), .names = "share"
    ))
    for (source in rlang::exprs(as.character(sum(x)), c(1, 2))) {
      expect_error(summarize_with_margins(
        data, dplyr::across(x, sum, .names = "unused"),
        total = !!source, !!share, .grouping = rollup(g)
      ), "scalar", class = "marginplyr_error")
    }
    packed <- summarize_with_margins(
      data, total = dplyr::across(x, sum), !!share, .grouping = rollup(g)
    )
    expect_false("share" %in% names(packed))
    expect_s3_class(packed$total, "data.frame")
    expect_error(summarize_with_margins(
      data, dplyr::across(
        dplyr::all_of(if (dplyr::n() == 0L) "x" else character()),
        sum, .names = "share"
      ),
      total = sum(x), !!share, .grouping = rollup(g)
    ), "conflicts with", class = "marginplyr_error")
    expect_error(summarize_with_margins(
      data, dplyr::across(
        dplyr::all_of(if (dplyr::n() == 0L) "x" else character()),
        sum, .names = "unused"
      ),
      total = unused + 1,
      dplyr::across(c(dplyr::starts_with("total"), total),
                    !!rlang::sym(helper), .names = "share"),
      .grouping = rollup(g)
    ), "not available", class = "marginplyr_error")
    dependent <- summarize_with_margins(
      data, dplyr::across(x, sum, .names = "unused"),
      total = unused + 1, !!share, .grouping = rollup(g)
    )
    expect_false("share" %in% names(dependent))
    duplicated <- summarize_with_margins(
      data, dplyr::across(x, sum, .names = "total"),
      total = sum(x), !!share, .grouping = rollup(g)
    )
    expect_false("share" %in% names(duplicated))

    explicit <- rlang::expr(dplyr::across(
      c(dplyr::starts_with("total"), total), !!rlang::sym(helper),
      .names = "share"
    ))
    expect_error(summarize_with_margins(
      data, total = dplyr::across(x, sum), !!explicit,
      .grouping = rollup(g)
    ), "not available", class = "marginplyr_error")
    expect_error(summarize_with_margins(
      data, dplyr::across(x, sum, .names = "unused"),
      total = unused + 1, !!explicit, .grouping = rollup(g)
    ), "not available", class = "marginplyr_error")
    expect_error(summarize_with_margins(
      data, dplyr::across(x, sum, .names = "total"),
      total = sum(x), !!explicit, .grouping = rollup(g)
    ), "more than once", class = "marginplyr_error")
  }
})

test_that("escaped across names remain selection share sources", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), y = c(10, 30))
  choose <- function() "x"
  result <- summarize_with_margins(
    data, dplyr::across(dplyr::all_of(choose()), sum, .names = "total}}"),
    `other_total}` = sum(y),
    dplyr::across(dplyr::ends_with("total}"), share_of_total,
                  .names = "{.col}_share"),
    .grouping = rollup(g)
  )
  expect_identical(result[["total}_share"]], c(0.25, 0.75, 1))
  expect_identical(result[["other_total}_share"]], c(0.25, 0.75, 1))
})

test_that("selection shares preserve repeated selector warnings", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  run <- function(pattern, phase) {
    warnings <- list()
    choose <- function() {
      if (phase == "selector") warning("selector warning")
      "x"
    }
    factory <- function() {
      if (phase == "function") warning("function warning")
      sum
    }
    share <- if (pattern) {
      rlang::quos(dplyr::across(
        dplyr::starts_with("total"), share_of_total, .names = "share"
      ))
    } else {
      rlang::quos(share = share_of_total(total))
    }
    withCallingHandlers(summarize_with_margins(
      data, dplyr::across(dplyr::all_of(choose()), factory(),
                          .names = "unused"),
      total = sum(x), !!!share, .grouping = rollup(g)
    ), warning = function(cnd) {
      warnings[[length(warnings) + 1L]] <<- cnd
      invokeRestart("muffleWarning")
    })
    warnings
  }
  for (phase in c("selector", "function")) {
    control <- run(FALSE, phase)
    actual <- run(TRUE, phase)
    expect_length(actual, 1L)
    expect_identical(class(actual[[1L]]), class(control[[1L]]))
    expect_identical(conditionMessage(actual[[1L]]),
                     conditionMessage(control[[1L]]))
    expect_match(conditionMessage(actual[[1L]]), "1 further grouping set")
  }
})

test_that("a selector error survives warning conversion during planning", {
  original_options <- options(warn = 2L)
  on.exit(options(original_options), add = TRUE)
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  failure <- rlang::error_cnd("selector_failure", message = "selector failure")
  calls <- 0L
  choose <- function() {
    calls <<- calls + 1L
    warning("before failure")
    stop(failure)
  }
  call <- quote(summarize_with_margins(
    data, dplyr::across(dplyr::all_of(choose()), sum, .names = "unused"),
    total = sum(x),
    dplyr::across(dplyr::starts_with("total"), share_of_total,
                  .names = "share"),
    .grouping = rollup(g)
  ))
  error <- tryCatch(eval(call), error = identity)
  expect_s3_class(error, "rlang_error")
  expect_match(conditionMessage(error), "selector failure", fixed = TRUE)
  expect_identical(error$call, call)
  cause <- error
  while (!is.null(cause$parent)) cause <- cause$parent
  expect_identical(cause, failure)
  expect_identical(calls, 1L)
})

test_that("pattern prediction respects caller bindings and explicit vars", {
  data <- data.frame(x = c(1, 3), y = c(10, 30))
  for (active in c(FALSE, TRUE)) {
    ready <- FALSE
    caller <- rlang::env(rlang::current_env())
    binding <- function() {
      stopifnot(ready)
      function(...) "unused"
    }
    if (active) {
      makeActiveBinding("starts_with", binding, caller)
    } else {
      delayedAssign("starts_with", binding(), assign.env = caller)
    }
    share <- rlang::new_quosure(quote(dplyr::across(
      starts_with("total"), share_of_total, .names = "share"
    )), caller)
    result <- summarize_with_margins(
      data,
      dplyr::across(dplyr::all_of({
        ready <<- TRUE
        "x"
      }), sum, .names = "unused"),
      total = sum(y), !!share
    )
    expect_identical(result$share, 1)
  }
  expect_error(summarize_with_margins(
    data, dplyr::across(x, sum, .names = "unused"), total = sum(y),
    dplyr::across(dplyr::starts_with("total", vars = "total"),
                  share_of_total, .names = "unused")
  ), "conflicts with", class = "marginplyr_error")
})
