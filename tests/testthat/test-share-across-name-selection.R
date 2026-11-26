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

test_that("first across share sources resume ordinary input selection", {
  data <- tibble::tibble(g = "a", x = 1, y = 10)
  for (share in rlang::syms(c("share_of_parent", "share_of_total"))) {
    state <- rlang::env(calls = 0L)
    choose <- function() {
      state$calls <- state$calls + 1L
      if (state$calls == 1L) "x" else "y"
    }
    actual <- summarize_with_margins(
      data, dplyr::across(dplyr::all_of(choose()), sum, .names = "measure"),
      p = (!!share)(measure), .grouping = rollup(g)
    )
    expect_identical(actual$measure, c(1, 10))
    expect_identical(actual$p, c(0.1, 1))
    expect_identical(state$calls, 2L)
  }
})

test_that("parenthesized share sources resume selection within each group", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), y = c(10, 30))
  for (form in c("head", "call")) {
    state <- rlang::env(calls = 0L)
    choose <- function() {
      state$calls <- state$calls + 1L
      if (state$calls == 1L) "x" else "y"
    }
    ordinary <- rlang::expr(dplyr::across(
      dplyr::all_of(choose()), sum, .names = "measure"
    ))
    if (form == "head") ordinary[[1L]] <- rlang::call2("(", ordinary[[1L]])
    if (form == "call") ordinary <- rlang::call2("(", ordinary)
    actual <- summarize_with_margins(
      data, !!ordinary, p = share_of_total(measure), .grouping = rollup(g)
    )
    expect_identical(actual$measure, c(1, 30, 40))
    expect_identical(actual$p, c(1 / 40, 30 / 40, 1))
    expect_identical(state$calls, 3L)
  }
})

test_that("resumed source selections cannot disappear into input columns", {
  data <- data.frame(g = "a", x = 1, measure = 9)
  state <- rlang::env(calls = 0L)
  choose <- function() {
    state$calls <- state$calls + 1L
    if (state$calls == 1L) "x" else character()
  }
  expect_error(summarize_with_margins(
    data, dplyr::across(dplyr::all_of(choose()), sum, .names = "measure"),
    p = share_of_total(measure), .grouping = rollup(g)
  ), "source summary `measure` to return exactly one",
  class = "marginplyr_share_cardinality_error")
})

# Separate dplyr grains provide the oracle for selector timing and values.
check_source_selection_grains <- function(convert) {
  data <- convert(data.frame(
    fixed = rep(c("u", "v"), each = 2L),
    g = rep(c("a", "b"), 2L),
    x = c(1, 3, 2, 6), y = c(10, 30, 20, 60)
  ))
  before <- serialize(data, NULL)
  for (form in c("bare", "head", "call")) {
    for (duplicate in c(FALSE, TRUE)) {
      run <- function(margin, share = NULL, sort = "none", pattern = FALSE) {
        state <- rlang::env(calls = 0L)
        choose <- function() {
          state$calls <- state$calls + 1L
          if (state$calls == 1L) "x" else "y"
        }
        expr <- rlang::expr(dplyr::across(
          dplyr::all_of(choose()), sum, .names = "measure"
        ))
        if (form == "head") expr[[1L]] <- rlang::call2("(", expr[[1L]])
        if (form == "call") expr <- rlang::call2("(", expr)
        if (margin) {
          request <- if (pattern) {
            rlang::quos(dplyr::across(
              dplyr::starts_with("measure"), !!share, .names = "p"
            ))
          } else {
            rlang::quos(p = (!!share)(!!rlang::sym("measure")))
          }
          result <- summarize_with_margins(
            data, !!expr, !!!request, .by = "fixed",
            .grouping = if (duplicate) rollup("g", "g") else rollup("g"),
            .duplicates = "keep", .id = "set", .sort = sort
          )
        } else {
          detail <- function(set) {
            dplyr::mutate(
              dplyr::summarise(data, !!expr, .by = c("fixed", "g")), set = set
            )
          }
          grains <- list(detail(1L))
          if (duplicate) grains <- c(grains, list(detail(2L)))
          total <- dplyr::mutate(
            dplyr::summarise(data, !!expr, .by = "fixed"),
            g = "Total", set = length(grains) + 1L
          )
          result <- dplyr::bind_rows(!!!grains, total)
        }
        result <- dplyr::arrange(result, .data$set, .data$fixed, .data$g)
        list(result = as.list(result), calls = state$calls)
      }
      expected <- run(FALSE)
      for (share in rlang::syms(c("share_of_parent", "share_of_total"))) {
        for (sort in c("none", "first", "last")) {
          for (pattern in c(FALSE, TRUE)) {
            actual <- run(TRUE, share, sort, pattern)
            info <- paste(form, duplicate, share, sort, pattern)
            expect_identical(actual$calls, expected$calls, info = info)
            expect_identical(actual$result[names(expected$result)],
                             expected$result, info = info)
            ratios <- if (form == "bare") {
              c(1 / 40, 3 / 40, 2 / 80, 6 / 80)
            } else {
              c(1 / 40, 30 / 40, 20 / 80, 60 / 80)
            }
            if (duplicate) ratios <- c(ratios, c(0.25, 0.75, 0.25, 0.75))
            expect_identical(actual$result$p, c(ratios, 1, 1), info = info)
            expect_identical(names(actual$result),
                             c("fixed", "g", "set", "measure", "p"))
          }
        }
      }
    }
  }
  expect_identical(serialize(data, NULL), before)
}

test_that("source selectors preserve fixed grains and Margin orders", {
  check_source_selection_grains(identity)
  check_source_selection_grains(tibble::as_tibble)
})

test_that("eager table source selectors preserve ordinary grain values", {
  skip_if_suggest_absent("data.table")
  check_source_selection_grains(data.table::as.data.table)
})

test_that("parenthesized resumed selector warnings retain repetition", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  choose <- function() {
    warning("parenthesized selector warning")
    "x"
  }
  warnings <- character()
  expr <- rlang::call2("(", rlang::expr(dplyr::across(
    dplyr::all_of(choose()), sum, .names = "measure"
  )))
  actual <- withCallingHandlers(summarize_with_margins(
    data, !!expr, p = share_of_total(measure), .grouping = rollup(g)
  ), warning = function(cnd) {
    warnings <<- c(warnings, conditionMessage(cnd))
    invokeRestart("muffleWarning")
  })
  expect_identical(actual$measure, c(1, 3, 4))
  expect_length(warnings, 1L)
  expect_match(warnings, "1 further grouping set", fixed = TRUE)
})

test_that("unresolved later and wrapped sources retain static admission", {
  data <- data.frame(g = "a", x = 1)
  state <- rlang::env(calls = 0L)
  choose <- function() {
    state$calls <- state$calls + 1L
    "x"
  }
  ordinary <- rlang::quo(dplyr::across(
    dplyr::all_of(choose()), sum, .names = "measure"
  ))
  for (share in rlang::syms(c("share_of_parent", "share_of_total"))) {
    expect_error(summarize_with_margins(
      data, sentinel = 7L, !!ordinary, p = (!!share)(measure),
      .grouping = rollup(g)
    ), "unknown preceding ordinary summary", class = "marginplyr_error")
    expect_error(summarize_with_margins(
      data, identity(!!ordinary), p = (!!share)(measure),
      .grouping = rollup(g)
    ), "unknown preceding ordinary summary", class = "marginplyr_error")
  }
  expect_identical(state$calls, 0L)
})

# A closure's constant argument reaches it as a value once R has compiled the
# caller and as a promise before then; a promise whose expression is that
# constant runs no caller code, so the two read alike (#1773).
constant_argument_cases <- rlang::exprs(
  all_of = dplyr::across(dplyr::all_of(arg), sum, .names = "{.col}_t"),
  any_of = dplyr::across(dplyr::any_of(arg), sum, .names = "{.col}_t"),
  one_of = dplyr::across(dplyr::one_of(arg), sum, .names = "{.col}_t"),
  number = dplyr::across(dplyr::num_range("x", arg), sum, .names = "{.col}_t"),
  ignore_case = dplyr::across(
    dplyr::starts_with("X", ignore.case = arg), sum, .names = "{.col}_t"
  ),
  names = dplyr::across(x, sum, .names = arg),
  null_names = dplyr::across(x, sum, .names = arg),
  null_fns = dplyr::across(x, arg, .names = "{.col}_t"),
  unpack = dplyr::across(x, sum, .names = "{.col}_t", .unpack = arg)
)
constant_argument_values <- list(
  all_of = "x", any_of = "x", one_of = "x", number = 1, ignore_case = TRUE,
  names = "{.col}_t", null_names = NULL, null_fns = NULL, unpack = FALSE
)

test_that("a constant argument names a later across() on its first call", {
  data <- data.frame(
    g = c("a", "a", "b"), x = c(1, 2, 5), x1 = c(1, 2, 5), y = c(10, 20, 30)
  )
  outcome <- function(call) {
    tryCatch(round(call()$share, 3), error = conditionMessage)
  }
  for (label in names(constant_argument_cases)) {
    source <- switch(label,
      null_names = quote(x), number = quote(x1_t), quote(x_t)
    )
    verb <- rlang::expr(summarize_with_margins(
      data, n = dplyr::n(), !!constant_argument_cases[[label]],
      share = share_of_total(!!source), .grouping = rollup(g)
    ))
    report <- rlang::new_function(rlang::pairlist2(arg = ), verb)
    caller <- rlang::new_function(NULL, rlang::expr(
      report(!!constant_argument_values[[label]])
    ))
    first <- outcome(caller)
    expect_identical(outcome(caller), first, info = label)
    expect_identical(outcome(caller), first, info = label)
    if (label == "null_fns") {
      expect_match(first, "unknown preceding ordinary summary", info = label)
    } else {
      expect_equal(first, c(0.375, 0.625, 1), info = label)
    }

    lazy <- rlang::new_function(NULL, rlang::expr({
      delayedAssign("arg", !!constant_argument_values[[label]])
      !!verb
    }))
    expect_identical(outcome(lazy), first, info = label)
  }
})

# A caller's own argument is a promise the planner does not force, so a later
# `across()` that reads one names no output before the call runs (#1773).
test_that("an unevaluated argument read by a later across() is named", {
  data <- data.frame(g = c("a", "a", "b"), x = c(1, 2, 5), y = c(10, 20, 30))
  cols <- c("x", "y")
  direct <- function(data, measures) {
    summarize_with_margins(
      data, n = dplyr::n(),
      dplyr::across(dplyr::all_of(measures), sum, .names = "{.col}_t"),
      share = share_of_total(x_t), .grouping = rollup(g)
    )
  }
  columnwise <- function(data, measures) {
    summarize_with_margins(
      data, n = dplyr::n(),
      dplyr::across(dplyr::all_of(measures), sum, .names = "{.col}_t"),
      dplyr::across(x_t, share_of_total, .names = "share"),
      .grouping = rollup(g)
    )
  }
  remedy <- paste0(
    "Evaluate `measures` before the call, as `force(measures)` does, or ",
    "write that `across()` before the other summaries."
  )
  refused <- expect_error(
    direct(data, cols), "unknown preceding ordinary summary `x_t`",
    class = "marginplyr_error"
  )
  expect_match(conditionMessage(refused), "reads `measures`", fixed = TRUE)
  expect_match(conditionMessage(refused), remedy, fixed = TRUE)
  refused <- expect_error(
    columnwise(data, cols), class = "marginplyr_share_source_unknown_error"
  )
  expect_match(conditionMessage(refused), remedy, fixed = TRUE)

  forced <- function(data, measures) {
    force(measures)
    summarize_with_margins(
      data, n = dplyr::n(),
      dplyr::across(dplyr::all_of(measures), sum, .names = "{.col}_t"),
      share = share_of_total(x_t), .grouping = rollup(g)
    )
  }
  reordered <- function(data, measures) {
    summarize_with_margins(
      data, dplyr::across(dplyr::all_of(measures), sum, .names = "{.col}_t"),
      n = dplyr::n(), share = share_of_total(x_t), .grouping = rollup(g)
    )
  }
  expect_equal(forced(data, cols)$share, c(0.375, 0.625, 1))
  expect_equal(reordered(data, cols)$share, c(0.375, 0.625, 1))
})

test_that("an unevaluated `.fns` argument is named with its one remedy", {
  data <- data.frame(g = c("a", "a", "b"), x = c(1, 2, 5))
  report <- function(data, fn) {
    summarize_with_margins(
      data, dplyr::across(x, fn, .names = "{.col}_t"),
      share = share_of_total(x_t), .grouping = rollup(g)
    )
  }
  refused <- expect_error(report(data, sum), class = "marginplyr_error")
  message <- conditionMessage(refused)
  expect_match(message, "reads `fn`", fixed = TRUE)
  expect_match(
    message, "Evaluate `fn` before the call, as `force(fn)` does.",
    fixed = TRUE
  )
  expect_no_match(message, "before the other summaries", fixed = TRUE)
  forced <- function(data, fn) {
    force(fn)
    summarize_with_margins(
      data, dplyr::across(x, fn, .names = "{.col}_t"),
      share = share_of_total(x_t), .grouping = rollup(g)
    )
  }
  expect_equal(forced(data, sum)$share, c(0.375, 0.625, 1))
})

test_that("the unevaluated-binding hint needs a later across() reading one", {
  data <- data.frame(g = c("a", "a", "b"), x = c(1, 2, 5), y = c(10, 20, 30))
  cols <- "x"
  hint <- "before the call"
  unrelated <- expect_error(summarize_with_margins(
    data, n = dplyr::n(), share = share_of_total(absent),
    .grouping = rollup(g)
  ), "unknown preceding ordinary summary `absent`")
  expect_no_match(conditionMessage(unrelated), hint, fixed = TRUE)
  first <- function(data, measures) {
    summarize_with_margins(
      data, dplyr::across(dplyr::all_of(measures), sum, .names = "{.col}_t"),
      n = dplyr::n(), share = share_of_total(absent), .grouping = rollup(g)
    )
  }
  first_refused <- expect_error(first(data, cols), "summary `absent`")
  expect_no_match(conditionMessage(first_refused), hint, fixed = TRUE)
  global <- expect_error(summarize_with_margins(
    data, n = dplyr::n(),
    dplyr::across(dplyr::all_of(cols), sum, .names = "{.col}_t"),
    share = share_of_total(absent), .grouping = rollup(g)
  ), "summary `absent`")
  expect_no_match(conditionMessage(global), hint, fixed = TRUE)
  after <- function(data, measures) {
    summarize_with_margins(
      data, n = dplyr::n(), share = share_of_total(absent),
      dplyr::across(dplyr::all_of(measures), sum, .names = "{.col}_t"),
      .grouping = rollup(g)
    )
  }
  after_refused <- expect_error(after(data, cols), "summary `absent`")
  expect_no_match(conditionMessage(after_refused), hint, fixed = TRUE)
})

# The share planner reads `.fns` as one function or a literal `list()`; any
# other spelling produces columns the planner has not proven (#1772).
test_that("an unread `.fns` hints at a literal list()", {
  data <- data.frame(g = c("a", "a", "b"), x = c(1, 2, 5), y = c(10, 20, 30))
  hint <- "Write its functions as a literal `list()`"
  fns <- list(s = sum, m = mean)
  for (across in rlang::exprs(
    dplyr::across(c(x, y), c(s = sum, m = mean)),
    dplyr::across(c(x, y), base::c(s = sum, m = mean)),
    dplyr::across(c(x, y), rlang::list2(s = sum, m = mean)),
    dplyr::across(c(x, y), fns)
  )) {
    label <- rlang::expr_deparse(across)
    direct <- expect_error(summarize_with_margins(
      data, !!across, sh = share_of_total(y_s), .grouping = rollup(g)
    ), "unknown preceding ordinary summary `y_s`", info = label)
    expect_match(conditionMessage(direct), hint, fixed = TRUE, info = label)
    columnwise <- expect_error(summarize_with_margins(
      data, !!across, dplyr::across(y_s, share_of_total, .names = "sh"),
      .grouping = rollup(g)
    ), class = "marginplyr_share_source_unknown_error", info = label)
    expect_match(
      conditionMessage(columnwise), hint, fixed = TRUE, info = label
    )
    # A source that is a column of the input gets that hint instead.
    column <- expect_error(summarize_with_margins(
      data, n = sum(x), !!across, sh = share_of_total(y),
      .grouping = rollup(g)
    ), "`y` is a column of the input", info = label)
    expect_no_match(conditionMessage(column), hint, fixed = TRUE, info = label)
  }

  listed <- summarize_with_margins(
    data, dplyr::across(c(x, y), list(s = sum, m = mean)),
    sh = share_of_total(y_s), .grouping = rollup(g)
  )
  expect_equal(listed$sh, c(0.5, 0.5, 1))

  unrelated <- expect_error(summarize_with_margins(
    data, dplyr::across(c(x, y), list(s = sum)), sh = share_of_total(y_m),
    .grouping = rollup(g)
  ), "unknown preceding ordinary summary `y_m`")
  expect_no_match(conditionMessage(unrelated), hint, fixed = TRUE)
  after <- expect_error(summarize_with_margins(
    data, sh = share_of_total(y_s), dplyr::across(c(x, y), c(s = sum)),
    .grouping = rollup(g)
  ), "summary `y_s`")
  expect_no_match(conditionMessage(after), hint, fixed = TRUE)
})

test_that("an unread `.fns` hints at list() on DuckDB", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  data <- dplyr::copy_to(con, data.frame(
    g = c("a", "a", "b"), x = c(1, 2, 5), y = c(10, 20, 30)
  ), "unread_fns")
  refused <- expect_error(summarize_with_margins(
    data, dplyr::across(c(x, y), c(s = sum, m = mean)),
    sh = share_of_total(y_s), .grouping = rollup(g)
  ), "unknown preceding ordinary summary `y_s`")
  expect_match(
    conditionMessage(refused), "Write its functions as a literal `list()`",
    fixed = TRUE
  )
  listed <- dplyr::collect(summarize_with_margins(
    data, dplyr::across(c(x, y), list(s = sum, m = mean)),
    sh = share_of_total(y_s), .grouping = rollup(g)
  ))
  expect_equal(listed$sh[match(c("a", "b", "Total"), listed$g)], c(0.5, 0.5, 1))
})
