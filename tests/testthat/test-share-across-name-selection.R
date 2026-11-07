test_that("ordinary across name helpers and set difference supply shares", {
  data <- data.frame(g = c("a", "b"), y = c(10, 30), x = c(1, 3))
  cols <- "x"
  for (selection in rlang::exprs(
    dplyr::one_of("x"), tidyselect::one_of(cols), x / y, 1 + 1, 3 - 1
  )) {
    ordinary <- rlang::quos(
      prior = sum(y),
      dplyr::across(!!selection, sum, .names = "sum_{.col}")
    )
    for (helper in rlang::syms(c("share_of_parent", "share_of_total"))) {
      direct <- rlang::quos(share = (!!helper)(sum_x))
      pattern <- rlang::quos(dplyr::across(
        dplyr::starts_with("sum_"), !!helper, .names = "share"
      ))
      for (request in list(direct, pattern)) {
        result <- summarize_with_margins(
          data, !!!ordinary, !!!request, .grouping = rollup(g)
        )
        expect_identical(names(result), c("g", "prior", "sum_x", "share"))
        expect_identical(result$sum_x, c(1, 3, 4))
        expect_identical(result$share, c(0.25, 0.75, 1))
      }
    }
  }
})

test_that("ordinary across share sources retain order and group evaluation", {
  data <- data.frame(
    g = c("a", "b"), x = c(1, 3), y = c(10, 30), z = c(8, 2)
  )
  for (selection in rlang::exprs(
    c(one_of(c("y", "z", "x")), -z, z),
    c(c(y, z, x) / z, z),
    c(c(y, z, x) & !z, z)
  )) {
    calls <- integer()
    measure <- function(value) {
      calls <<- c(calls, dplyr::n())
      sum(value)
    }
    ordinary <- rlang::quos(
      prior = sum(y),
      dplyr::across(!!selection, measure, .names = "sum_{.col}")
    )
    control <- summarize_with_margins(
      data, !!!ordinary, .grouping = rollup(g)
    )
    ordinary_calls <- calls
    calls <- integer()
    actual <- summarize_with_margins(
      data, !!!ordinary, dplyr::across(
        dplyr::starts_with("sum_"), share_of_total, .names = "{.col}_share"
      ),
      .grouping = rollup(g)
    )

    expect_identical(calls, ordinary_calls)
    expect_identical(calls, rep(c(1L, 1L, 2L), each = 3L))
    expect_identical(actual[names(control)], control)
    expect_identical(names(actual), c(
      "g", "prior", "sum_y", "sum_x", "sum_z",
      "sum_y_share", "sum_x_share", "sum_z_share"
    ))
    expect_identical(actual$sum_y, c(10, 30, 40))
    expect_identical(actual$sum_x, c(1, 3, 4))
    expect_identical(actual$sum_z, c(8, 2, 10))
    expect_identical(actual$sum_z_share, c(0.8, 0.2, 1))
  }
})

test_that("ordinary across name selections still refuse ineligible sources", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), y = c(10, 30))
  fns <- list(total = sum)
  for (helper in rlang::syms(c("share_of_parent", "share_of_total"))) {
    expect_error(summarize_with_margins(
      data, prior = sum(y),
      dplyr::across(c(prior, x) / x, sum, .names = "sum_{.col}"),
      share = (!!helper)(sum_prior), .grouping = rollup(g)
    ), "depends on earlier summary alias `prior`", class = "marginplyr_error")
    expect_error(summarize_with_margins(
      data, prior = sum(y),
      dplyr::across(dplyr::one_of("x"), sum, .names = "sum_{.col}"),
      sum_x = sum(y), share = (!!helper)(sum_x), .grouping = rollup(g)
    ), "defined exactly once", class = "marginplyr_error")
    expect_error(summarize_with_margins(
      data, prior = sum(y),
      dplyr::across(dplyr::one_of("x"), fns, .names = "{.col}_{.fn}"),
      share = (!!helper)(x_total), .grouping = rollup(g)
    ), "unknown preceding ordinary summary", class = "marginplyr_error")
  }
})

test_that("ordinary one_of arguments stay in their execution context", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), y = c(10, 30))
  calls <- integer()
  choose <- function() {
    calls <<- c(calls, dplyr::n())
    "x"
  }
  ordinary <- rlang::quos(
    prior = sum(y),
    dplyr::across(dplyr::one_of(choose()), sum, .names = "sum_{.col}")
  )
  expect_error(summarize_with_margins(
    data, !!!ordinary, share = share_of_total(sum_x), .grouping = rollup(g)
  ), "unknown preceding ordinary summary", class = "marginplyr_error")
  expect_identical(calls, integer())

  control <- summarize_with_margins(data, !!!ordinary, .grouping = rollup(g))
  ordinary_calls <- calls
  calls <- integer()
  actual <- summarize_with_margins(
    data, !!!ordinary, share = share_of_total(prior), .grouping = rollup(g)
  )
  expect_identical(calls, ordinary_calls)
  expect_identical(calls, c(0L, 0L))
  expect_identical(actual[names(control)], control)
})

test_that("ordinary one_of leaves warning and deferred bindings to execution", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), y = c(10, 30))
  reads <- 0L
  read_cols <- function() {
    reads <<- reads + 1L
    "x"
  }
  delayedAssign("promise_cols", read_cols())
  makeActiveBinding("active_cols", read_cols, environment())
  missing_cols <- c("x", "absent")

  for (columns in rlang::exprs(promise_cols, active_cols, missing_cols)) {
    ordinary <- rlang::quos(
      prior = sum(y),
      dplyr::across(dplyr::one_of(!!columns), sum, .names = "sum_{.col}")
    )
    expect_no_warning(expect_error(summarize_with_margins(
      data, !!!ordinary, share = share_of_total(sum_x), .grouping = rollup(g)
    ), "unknown preceding ordinary summary", class = "marginplyr_error"))
    expect_identical(reads, 0L)
  }

  run <- function(shares) {
    warnings <- character()
    result <- withCallingHandlers(summarize_with_margins(
      data, prior = sum(y),
      dplyr::across(dplyr::one_of(missing_cols), sum, .names = "sum_{.col}"),
      !!!shares, .grouping = rollup(g)
    ), warning = function(cnd) {
      warnings <<- c(warnings, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    })
    list(result = result, warnings = warnings)
  }
  control <- run(list())
  actual <- run(rlang::quos(share = share_of_total(prior)))
  expect_match(
    control$warnings[[1L]], "Unknown columns: `absent`", fixed = TRUE
  )
  expect_identical(actual$warnings, control$warnings)
  expect_identical(actual$result[names(control$result)], control$result)
})

check_first_across_storage <- function(convert) {
  rlang::local_bindings(
    as.list.margin_across_input = function(x, ...) {
      if (nrow(x) > 0L && identical(attr(x, "view"), "refuse")) {
        stop("Positive-row list conversion is unavailable.")
      }
      list(g = x$g, x = as.character(x$x), y = seq_along(x$y))
    },
    .env = asNamespace("base")$.__S3MethodsTable__.
  )
  fixture <- data.frame(g = c("a", "b", "a"), x = 1:3, y = letters[1:3])
  fixture <- convert(fixture)
  ordinary <- rlang::quos(
    dplyr::across(
      tidyselect::where(is.numeric), length, .names = "count_{.col}"
    ),
    total = sum(.data$x)
  )
  for (view in c("alter", "refuse")) {
    data <- structure(
      fixture, class = c("margin_across_input", class(fixture)), view = view
    )
    expected <- dplyr::summarize(data, !!!ordinary)
    expect_identical(as.list(expected), list(count_x = 3L, total = 6L))

    for (grouping in rlang::quos(NULL, rollup(dplyr::all_of("g")))) {
      rolled_up <- !rlang::quo_is_null(grouping)
      helpers <- c("share_of_total", if (rolled_up) "share_of_parent")
      for (helper in rlang::syms(helpers)) {
        request <- rlang::call2(helper, quote(total))
        direct <- summarize_with_margins(
          data, !!!ordinary, share = !!request,
          .grouping = !!grouping, .sort = "last"
        )
        selected <- summarize_with_margins(
          data, !!!ordinary, dplyr::across(
            dplyr::starts_with("total"), !!helper, .names = "share"
          ),
          .grouping = !!grouping, .sort = "last"
        )
        expect_identical(as.list(selected), as.list(direct))
        expect_identical(
          as.list(direct),
          if (rolled_up) {
            list(g = c("a", "b", "Total"), count_x = c(2L, 1L, 3L),
                 total = c(4L, 2L, 6L), share = c(4 / 6, 2 / 6, 1))
          } else {
            c(as.list(expected), list(share = 1))
          }
        )
      }
    }
  }
}

test_that("first across reads stored columns beside local share selections", {
  check_first_across_storage(identity)
  check_first_across_storage(tibble::as_tibble)
})

test_that("first across reads stored eager table columns beside shares", {
  skip_if_suggest_absent("data.table")
  check_first_across_storage(data.table::as.data.table)
})
