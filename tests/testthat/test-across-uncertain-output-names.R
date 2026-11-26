test_that("a bound across list does not invent an identifier collision", {
  data <- data.frame(group = c("a", "b"), x = c(1, 2))
  fns <- list(total = sum, avg = mean)

  bound <- summarize_with_margins(
    data,
    dplyr::across(x, fns, .names = "{.col}_{.fn}"),
    .grouping = grouping_set(group),
    .id = "x_1"
  )
  literal <- summarize_with_margins(
    data,
    dplyr::across(x, list(total = sum, avg = mean),
                  .names = "{.col}_{.fn}"),
    .grouping = grouping_set(group),
    .id = "x_1"
  )

  expect_identical(names(bound), c("group", "x_1", "x_total", "x_avg"))
  expect_equal(bound, literal)
  expect_equal(bound$x_total, c(1, 2))
  expect_equal(bound$x_avg, c(1, 2))
  expect_equal(bound$x_1, c(1L, 1L))
})

test_that("default across names stay unknown for a bound function list", {
  data <- data.frame(group = c("a", "b"), x = c(1, 2))
  fns <- list(total = sum, avg = mean)

  bound <- summarize_with_margins(
    data, dplyr::across(x, fns),
    .grouping = grouping_set(group), .id = "x_1"
  )
  literal <- summarize_with_margins(
    data, dplyr::across(x, list(total = sum, avg = mean)),
    .grouping = grouping_set(group), .id = "x_1"
  )

  expect_equal(bound, literal)
  expect_identical(names(bound), c("group", "x_1", "x_total", "x_avg"))
})

test_that("a function list can shadow a familiar function name", {
  data <- data.frame(group = c("a", "b"), x = c(1, 2))
  result <- local({
    sum <- list(total = base::sum, avg = base::mean)
    summarize_with_margins(
      data, dplyr::across(x, sum, .names = "{.col}_{.fn}"),
      .grouping = grouping_set(group), .id = "x_1"
    )
  })

  expect_identical(names(result), c("group", "x_1", "x_total", "x_avg"))
})

test_that("a caller-bound list function determines across output names", {
  data <- data.frame(g = c("a", "b"), x = c(1, 2))
  list <- function(...) base::list(total = sum)

  actual <- summarize_with_margins(
    data,
    dplyr::across(x, list(avg = mean), .names = "{.col}_{.fn}"),
    .grouping = grouping_set(g), .id = "x_avg"
  )

  expect_identical(names(actual), c("g", "x_avg", "x_total"))
  expect_identical(actual$x_avg, c(1L, 1L))
  expect_equal(actual$x_total, c(1, 2))
})

test_that("qualified base list keeps its own function entries", {
  data <- data.frame(g = c("a", "b"), x = c(1, 2))
  list <- function(...) base::list(total = sum)

  actual <- summarize_with_margins(
    data,
    dplyr::across(x, base::list(avg = mean),
                  .names = "{.col}_{.fn}"),
    .grouping = grouping_set(g), .id = "x_total"
  )

  expect_identical(names(actual), c("g", "x_total", "x_avg"))
  expect_equal(actual$x_avg, c(1, 2))
})

test_that("caller-bound list outputs still protect identifiers and groups", {
  data <- data.frame(g = c("a", "b"), x = c(1, 2))
  list <- function(...) base::list(total = sum)

  id_error <- expect_error(summarize_with_margins(
    data,
    dplyr::across(x, list(avg = mean), .names = "{.col}_{.fn}"),
    .grouping = grouping_set(g), .id = "x_total"
  ), "`.id`.*`x_total`.*conflicts with a summary output")
  expect_s3_class(id_error, "marginplyr_error")

  group_data <- data.frame(x_total = c("a", "b"), x = c(1, 2))
  group_error <- expect_error(summarize_with_margins(
    group_data,
    dplyr::across(x, list(avg = mean), .names = "{.col}_{.fn}"),
    .grouping = grouping_set(x_total)
  ), "cannot overwrite grouping column.*`x_total`")
  expect_s3_class(group_error, "marginplyr_error")
})

test_that("list prediction does not force a delayed callee binding", {
  data <- data.frame(g = c("a", "b"), x = c(1, 2))
  counter <- new.env(parent = emptyenv())
  counter$binding <- 0L
  counter$body <- 0L
  delayedAssign("list", {
    counter$binding <- counter$binding + 1L
    function(...) {
      counter$body <- counter$body + 1L
      base::list(total = sum)
    }
  })

  actual <- summarize_with_margins(
    data,
    dplyr::across(x, list(avg = mean), .names = "{.col}_{.fn}"),
    .grouping = grouping_set(g), .id = "x_avg"
  )
  expect_identical(counter$binding, 1L)
  expect_identical(counter$body, 1L)
  expect_identical(names(actual), c("g", "x_avg", "x_total"))
})

test_that("list prediction does not invoke an active callee binding", {
  data <- data.frame(g = c("a", "b"), x = c(1, 2))
  counter <- new.env(parent = emptyenv())
  counter$binding <- 0L
  counter$body <- 0L
  actual <- local({
    makeActiveBinding("list", function() {
      counter$binding <- counter$binding + 1L
      function(...) {
        counter$body <- counter$body + 1L
        base::list(total = sum)
      }
    }, environment())
    summarize_with_margins(
      data,
      dplyr::across(x, list(avg = mean), .names = "{.col}_{.fn}"),
      .grouping = grouping_set(g), .id = "x_avg"
    )
  })

  expect_identical(counter$binding, 1L)
  expect_identical(counter$body, 1L)
  expect_identical(names(actual), c("g", "x_avg", "x_total"))
})

test_that("actual bound-list collisions are refused after execution", {
  data <- data.frame(group = c("a", "b"), x = c(1, 2))
  fns <- list(total = sum, avg = mean)
  group_data <- data.frame(x_total = c("a", "b"), x = c(1, 2))

  id_error <- expect_error(summarize_with_margins(
    data, dplyr::across(x, fns, .names = "{.col}_{.fn}"),
    .grouping = grouping_set(group), .id = "x_total"
  ), "`.id`.*`x_total`.*conflicts with a summary output")
  expect_s3_class(id_error, "marginplyr_error")

  group_error <- expect_error(summarize_with_margins(
    group_data, dplyr::across(x, fns, .names = "{.col}_{.fn}"),
    .grouping = grouping_set(x_total)
  ), "cannot overwrite grouping column.*`x_total`")
  expect_s3_class(group_error, "marginplyr_error")

  internal_fns <- list(total = sum)
  internal_fns[["..marginplyr_sort_1"]] <- mean
  internal_error <- expect_error(summarize_with_margins(
    data, dplyr::across(x, internal_fns, .names = "{.fn}"),
    .grouping = rollup(group), .sort = "last"
  ), "summary output names conflict with internal grouping columns")
  expect_match(conditionMessage(internal_error), "..marginplyr_sort_1",
               fixed = TRUE)
  expect_false(grepl("`.id`", conditionMessage(internal_error), fixed = TRUE))
})

test_that("known across shapes still name their actual outputs", {
  data <- data.frame(group = c("a", "b"), x = c(1, 2))

  single <- expect_error(summarize_with_margins(
    data, dplyr::across(x, base::sum, .names = "{.col}_{.fn}"),
    .grouping = grouping_set(group), .id = "x_1"
  ), "`.id`.*`x_1`.*conflicts with a summary output")
  literal <- expect_error(summarize_with_margins(
    data, dplyr::across(x, list(total = sum), .names = "{.col}_{.fn}"),
    .grouping = grouping_set(group), .id = "x_total"
  ), "`.id`.*`x_total`.*conflicts with a summary output")

  expect_s3_class(single, "marginplyr_error")
  expect_s3_class(literal, "marginplyr_error")
})

test_that("an internal namespace spelling can name a known function", {
  parsed <- parse_across_arguments(rlang::parse_expr(
    "dplyr::across(x, base:::sum)"
  ))

  expect_identical(known_across_function_names(parsed, environment()), "1")
})

test_that("unavailable and non-function namespace names stay unknown", {
  unavailable <- parse_across_arguments(rlang::parse_expr(
    "dplyr::across(x, marginplyr_missing_607::fun)"
  ))
  non_function <- parse_across_arguments(quote(dplyr::across(x, base::letters)))

  expect_identical(
    known_across_function_names(unavailable, environment()), character()
  )
  expect_false(isNamespaceLoaded("marginplyr_missing_607"))
  expect_identical(
    known_across_function_names(non_function, environment()), character()
  )
})

test_that("prediction does not evaluate a function-list expression", {
  data <- data.frame(group = c("a", "b"), x = c(1, 2))
  calls <- 0L
  make_fns <- function() {
    calls <<- calls + 1L
    list(total = sum, avg = mean)
  }

  result <- summarize_with_margins(
    data,
    dplyr::across(x, make_fns(), .names = "{.col}_{.fn}"),
    .grouping = grouping_set(group), .id = "x_1"
  )
  expect_identical(calls, 1L)
  expect_identical(names(result), c("group", "x_1", "x_total", "x_avg"))
})

test_that("prediction does not force a delayed function-list binding", {
  data <- data.frame(group = c("a", "b"), x = c(1, 2))
  counter <- new.env(parent = emptyenv())
  counter$calls <- 0L
  delayedAssign("fns", {
    counter$calls <- counter$calls + 1L
    list(total = sum, avg = mean)
  })

  result <- summarize_with_margins(
    data, dplyr::across(x, fns, .names = "{.col}_{.fn}"),
    .grouping = grouping_set(group), .id = "x_1"
  )

  expect_identical(counter$calls, 1L)
  expect_identical(names(result), c("group", "x_1", "x_total", "x_avg"))
})

test_that("prediction does not call an active function-list binding", {
  data <- data.frame(group = c("a", "b"), x = c(1, 2))
  counter <- new.env(parent = emptyenv())
  counter$calls <- 0L
  result <- local({
    makeActiveBinding("fns", function() {
      counter$calls <- counter$calls + 1L
      list(total = sum, avg = mean)
    }, environment())
    summarize_with_margins(
      data, dplyr::across(x, fns, .names = "{.col}_{.fn}"),
      .grouping = grouping_set(group), .id = "x_1"
    )
  })

  expect_identical(counter$calls, 1L)
  expect_identical(names(result), c("group", "x_1", "x_total", "x_avg"))
})

test_that("a bound function list keeps a simulated SQL result lazy", {
  data <- data.frame(group = c("a", "b"), x = c(1, 2))
  remote <- dbplyr::tbl_lazy(data, con = dbplyr::simulate_postgres())
  fns <- list(total = sum, avg = mean)

  result <- summarize_with_margins(
    remote, dplyr::across(x, fns, .names = "{.col}_{.fn}"),
    .grouping = grouping_set(group), .id = "x_1"
  )

  expect_s3_class(result, "tbl_lazy")
  expect_identical(colnames(result), c("group", "x_1", "x_total", "x_avg"))
})

test_that("unknown ordinary outputs are not admitted as share sources", {
  data <- data.frame(group = c("a", "b"), x = c(1, 2))
  fns <- list(total = sum, avg = mean)

  error <- expect_error(summarize_with_margins(
    data,
    dplyr::across(x, fns, .names = "{.col}_{.fn}"),
    share = share_of_parent(x_total),
    .grouping = rollup(group)
  ), "unknown preceding ordinary summary `x_total`")
  expect_s3_class(error, "marginplyr_error")
})

test_that("a delayed single function keeps its share source name", {
  data <- data.frame(group = c("a", "b"), x = c(1, 2))
  summarize <- function(delayed) {
    if (delayed) {
      delayedAssign("fn", function(value) sum(value))
    } else {
      fn <- function(value) sum(value)
    }
    summarize_with_margins(
      data,
      dplyr::across(x, fn, .names = "{.col}_{.fn}"),
      share = share_of_parent(x_1),
      .grouping = rollup(group),
      .margin_label = NULL
    )
  }

  delayed <- summarize(TRUE)
  eager <- summarize(FALSE)
  expect_equal(delayed, eager)
  expect_identical(names(delayed), c("group", "x_1", "share"))
  expect_equal(delayed$share, c(1 / 3, 2 / 3, 1))
})

test_that("a globally bound single function keeps its share source name", {
  run_global_call <- function() {
    assign("marginplyr_test_fn_607", sum, envir = globalenv())
    on.exit(rm("marginplyr_test_fn_607", envir = globalenv()))
    eval(quote(summarize_with_margins(
      data.frame(group = c("a", "b"), x = c(1, 2)),
      dplyr::across(x, marginplyr_test_fn_607, .names = "{.col}_{.fn}"),
      share = share_of_parent(x_1),
      .grouping = rollup(group),
      .margin_label = NULL
    )), envir = globalenv())
  }

  result <- run_global_call()
  expect_identical(names(result), c("group", "x_1", "share"))
  expect_equal(result$share, c(1 / 3, 2 / 3, 1))
})

authored_namespace_fixture <- function(operator, entrance, functions,
                                       .env = parent.frame()) {
  force(functions)
  target <- if (entrance == "value") "sum" else "list"
  fixture <- rlang::env(hits = 0L)
  caller <- rlang::new_environment(parent = .env)
  caller[[operator]] <- function(pkg, name) {
    package <- as.character(substitute(pkg))
    member <- as.character(substitute(name))
    if (package == "base" && member == target) {
      fixture$hits <- fixture$hits + 1L
      return(if (entrance == "value") {
        functions
      } else {
        function(...) functions
      })
    }
    get(member, envir = asNamespace(package))
  }
  qualified <- rlang::call2(operator, quote(base), as.name(target))
  fixture$fns <- if (entrance == "value") {
    qualified
  } else {
    rlang::call2(qualified, fake = quote(mean))
  }
  fixture$caller <- caller
  fixture
}

for (input_kind in c("data.frame", "tibble", "data.table")) {
  test_that(paste("authored namespace across shapes respect", input_kind), {
    if (input_kind == "data.table") skip_if_suggest_absent("data.table")
    for (operator in c("::", ":::")) {
      for (entrance in c("value", "list")) {
        for (count in c(1L, 2L)) {
          for (role in c("id", "fixed", "dimension")) {
            protected <- if (entrance == "value") "answer_1" else "answer_fake"
            data <- stats::setNames(
              data.frame(key = c("a", "b"), x = c(1L, 3L)),
              c(protected, "x")
            )
            data <- switch(input_kind,
              tibble = tibble::as_tibble(data),
              data.table = data.table::as.data.table(data),
              data
            )
            before <- serialize(data, NULL)
            functions <- if (count == 1L) {
              list(s = sum)
            } else {
              list(s = sum, m = max)
            }
            fixture <- authored_namespace_fixture(operator, entrance, functions)
            authored <- rlang::new_quosure(rlang::expr(
              dplyr::across(x, !!fixture$fns, .names = "answer_{.fn}")
            ), fixture$caller)
            literal <- rlang::quo(dplyr::across(
              x, !!functions, .names = "answer_{.fn}"
            ))
            run <- function(dot) {
              switch(role,
                id = summarize_with_margins(
                  dplyr::select(data, "x"), !!dot, .id = protected
                ),
                fixed = summarize_with_margins(
                  data, !!dot, .by = dplyr::all_of(protected)
                ),
                dimension = summarize_with_margins(
                  data, !!dot, .grouping = rollup(dplyr::all_of(protected)),
                  .margin_label = NULL
                )
              )
            }
            expected <- if (role == "id") {
              dplyr::summarise(dplyr::select(data, "x"), !!authored)
            } else {
              dplyr::summarise(data, !!authored, .by = dplyr::all_of(protected))
            }
            if (role == "id") {
              expected <- dplyr::mutate(
                expected, !!protected := 1L, .before = 1L
              )
            } else if (role == "dimension") {
              total <- dplyr::summarise(data, !!authored)
              total <- dplyr::mutate(
                total, !!protected := NA_character_, .before = 1L
              )
              expected <- dplyr::bind_rows(expected, total)
            }
            expected_hits <- fixture$hits
            fixture$hits <- 0L
            expect_identical(run(literal), expected)
            expect_identical(fixture$hits, 0L)
            expect_identical(run(authored), expected)
            expect_identical(fixture$hits, expected_hits)
            expect_identical(serialize(data, NULL), before)
          }
        }
      }
    }
  })
}

test_that("authored namespace outputs keep static share provenance", {
  data <- data.frame(g = c("a", "b"), x = c(1L, 3L))
  for (operator in c("::", ":::")) {
    for (entrance in c("value", "list")) {
      for (grouped in c(FALSE, TRUE)) {
        fixture <- authored_namespace_fixture(
          operator, entrance, list(s = sum, m = max)
        )
        ordinary <- rlang::new_quosure(
          rlang::expr(dplyr::across(x, !!fixture$fns)), fixture$caller
        )
        run <- function(dot, selection) {
          summarize_with_margins(
            data, !!dot,
            dplyr::across(!!selection, share_of_total, .names = "{.col}_p"),
            .grouping = if (grouped) rollup(g) else NULL,
            .margin_label = NULL
          )
        }
        expect_error(run(ordinary, quote(x_s)), "unknown summary `x_s`",
                     class = "marginplyr_share_source_unknown_error")
        expect_identical(fixture$hits, 0L)
        result <- run(ordinary, quote(dplyr::starts_with("x")))
        expect_identical(fixture$hits, if (grouped) 2L else 1L)
        expect_identical(names(result), c(if (grouped) "g", "x_s", "x_m"))
        expect_identical(result$x_s, if (grouped) c(1L, 3L, 4L) else 4L)
        expect_identical(result$x_m, if (grouped) c(1L, 3L, 3L) else 3L)
        literal <- rlang::quo(dplyr::across(x, list(s = sum, m = max)))
        control <- run(literal, quote(dplyr::starts_with("x")))
        expect_identical(control[names(result)], result)
        expect_equal(control$x_s_p, if (grouped) c(0.25, 0.75, 1) else 1)
        expect_equal(control$x_m_p, if (grouped) c(1 / 3, 1, 1) else 1)
      }
    }
  }
})

test_that("authored namespace outputs retain actual collision protection", {
  for (operator in c("::", ":::")) {
    for (entrance in c("value", "list")) {
      fixture <- authored_namespace_fixture(
        operator, entrance, list(s = sum, m = max)
      )
      dot <- rlang::new_quosure(rlang::expr(
        dplyr::across(x, !!fixture$fns, .names = "answer_{.fn}")
      ), fixture$caller)
      data <- data.frame(answer_s = c("a", "b"), x = c(1L, 3L))
      before <- serialize(data, NULL)
      expect_error(summarize_with_margins(
        dplyr::select(data, x), !!dot, .id = "answer_s"
      ), "conflicts with a summary output", class = "marginplyr_error")
      expect_error(summarize_with_margins(
        data, !!dot, .by = answer_s
      ), "cannot overwrite grouping column", class = "marginplyr_error")
      expect_error(summarize_with_margins(
        data, !!dot, .grouping = rollup(answer_s)
      ), "cannot overwrite grouping column", class = "marginplyr_error")
      expect_identical(serialize(data, NULL), before)
    }
  }
})

# A constant `.names` template after an output that reuses an input name sees
# that column once, as `dplyr::summarise()` does (#1584).
across_after_overwrite_cases <- list(
  named = rlang::quos(
    v = sum(v), dplyr::across(v, ~ .x / 2, .names = "half")
  ),
  unnamed = rlang::quos(
    dplyr::across(v, sum), dplyr::across(v, ~ .x / 2, .names = "half")
  ),
  several = rlang::quos(
    v = sum(v), w = max(w),
    dplyr::across(v, ~ .x / 2, .names = "half"),
    dplyr::across(w, ~ .x / 2, .names = "half_w")
  )
)

for (input_kind in c("data.frame", "tibble", "data.table")) {
  test_that(paste("a constant .names follows an overwrite on", input_kind), {
    if (input_kind == "data.table") skip_if_suggest_absent("data.table")
    data <- data.frame(g = c("a", "a", "b"), v = 1:3, w = c(4, 6, 5))
    input <- switch(input_kind,
      tibble = tibble::as_tibble(data),
      data.table = data.table::as.data.table(data),
      data
    )
    for (case in names(across_after_overwrite_cases)) {
      dots <- across_after_overwrite_cases[[case]]
      by_g <- dplyr::summarise(data, !!!dots, .by = g)
      total <- dplyr::summarise(data, !!!dots)
      expected <- dplyr::bind_rows(
        by_g, dplyr::mutate(total, g = "Total", .before = 1L)
      )
      rolled <- summarize_with_margins(input, !!!dots, .grouping = rollup(g))
      expect_equal(
        as.data.frame(rolled), as.data.frame(expected), info = case
      )
      fixed <- summarize_with_margins(input, !!!dots, .by = g)
      expect_equal(as.data.frame(fixed), as.data.frame(by_g), info = case)
    }
  })
}

test_that("a constant .names that yields too few names counts real outputs", {
  data <- data.frame(g = c("a", "a", "b"), v = 1:3, w = c(4, 6, 5))
  expect_error(
    summarize_with_margins(
      data, v = sum(v),
      dplyr::across(c(v, w), ~ .x / 2, .names = "half"),
      .grouping = rollup(g)
    ),
    "produced 1 for 2 outputs"
  )
})

# A literal empty `.fns` list produces no column on any backend, so the
# planner predicts none (#1795). SQLite's dialect cannot type a share source,
# so its shares are taken without the source check. Each backend runs in a
# test of its own, so no test requires more than one member of
# `optional_backends()`.
empty_fns_input <- function() {
  data.frame(g = c("a", "a", "b"), x = c(1, 2, 5), y = c(10, 20, 30))
}

empty_fns_acrosses <- function() {
  empty <- list()
  spellings <- list(
    literal = quote(list()),
    qualified = quote(base::list()),
    parenthesized = quote((list())),
    spliced = rlang::expr(list(!!!empty))
  )
  lapply(spellings, function(fns) {
    across <- quote(dplyr::across(x, NULL))
    across[[3L]] <- fns
    across
  })
}

empty_fns_rows <- function(query) {
  result <- as.data.frame(dplyr::collect(query))
  result[order(is.na(result[[1L]]), result[[1L]]), , drop = FALSE]
}

expect_empty_fns_outputs <- function(make_input, ...) {
  acrosses <- empty_fns_acrosses()
  rolled <- rollup(dplyr::all_of("g"))
  for (label in names(acrosses)) {
    across <- acrosses[[label]]
    input <- make_input(empty_fns_input())
    direct <- expect_error(summarize_with_margins(
      input, !!across, s = !!quote(share_of_total(x_)), .grouping = rolled,
      ...
    ), "unknown preceding ordinary summary `x_`", info = label)
    expect_s3_class(direct, "marginplyr_error")
    expect_error(summarize_with_margins(
      input, !!across,
      dplyr::across(dplyr::all_of("x_"), share_of_parent, .names = "p"),
      .grouping = rolled, ...
    ), class = "marginplyr_share_source_unknown_error", info = label)

    genuine <- empty_fns_rows(summarize_with_margins(
      input, !!across, x_ = !!quote(sum(x)),
      s = !!quote(share_of_total(x_)), .grouping = rolled,
      .margin_label = NULL, ...
    ))
    expect_identical(names(genuine), c("g", "x_", "s"), info = label)
    expect_equal(genuine$x_, c(3, 5, 8), info = label)
    expect_equal(genuine$s, c(0.375, 0.625, 1), info = label)

    identified <- empty_fns_rows(summarize_with_margins(
      input, !!across, m = dplyr::n(), .grouping = rolled, .id = "x_",
      .margin_label = NULL
    ))
    expect_identical(names(identified), c("g", "x_", "m"), info = label)
    expect_equal(identified$m, c(2, 1, 3), info = label)

    keyed <- make_input(dplyr::rename(empty_fns_input(), x_ = "g"))
    dimension <- empty_fns_rows(summarize_with_margins(
      keyed, !!across, m = dplyr::n(),
      .grouping = rollup(dplyr::all_of("x_")), .margin_label = NULL
    ))
    expect_identical(names(dimension), c("x_", "m"), info = label)
    expect_equal(dimension$m, c(2, 1, 3), info = label)
    fixed <- empty_fns_rows(summarize_with_margins(
      keyed, !!across, m = dplyr::n(), .by = dplyr::all_of("x_")
    ))
    expect_identical(names(fixed), c("x_", "m"), info = label)
    expect_equal(fixed$m, c(2, 1), info = label)
  }
}
test_that("an empty `.fns` list names no output on a data frame", {
  expect_empty_fns_outputs(identity)
})

test_that("an empty `.fns` list names no output on a data.table", {
  skip_if_suggest_absent("data.table")
  expect_empty_fns_outputs(data.table::as.data.table)
})

test_that("an empty `.fns` list names no output on dtplyr", {
  skip_if_suggest_absent("dtplyr")
  expect_empty_fns_outputs(dtplyr::lazy_dt)
})

test_that("an empty `.fns` list names no output on DuckDB", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_empty_fns_outputs(function(data) {
    dplyr::copy_to(con, data, overwrite = TRUE)
  })
})

test_that("an empty `.fns` list names no output on SQLite", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_empty_fns_outputs(function(data) {
    dplyr::copy_to(con, data, overwrite = TRUE)
  }, .check_share_source = FALSE)
})

# An export of an attached or imported package is an unforced lazy-load
# promise until something evaluates it, and the installed-package test
# environment forces its own copies (#1685), so these fixtures install
# packages of their own and load them afresh for every call. `lazyacrossa`
# exports `n_distinct()` and shares its output from its own namespace;
# `lazyacrossb` imports it and does the same from its imports environment
# (#1793).
lazy_load_fixture <- new.env(parent = emptyenv())

lazy_load_fixture_lib <- function() {
  if (!is.null(lazy_load_fixture$lib)) {
    return(lazy_load_fixture$lib)
  }
  root <- tempfile("lazyacross")
  dir.create(root)
  lib <- file.path(root, "lib")
  dir.create(lib)
  share_call <- paste0(
    "marginplyr::summarize_with_margins(data, ",
    "dplyr::across(c(x, y), n_distinct, .names = '{.col}_n'), ",
    "s = marginplyr::share_of_total(y_n), ",
    ".grouping = marginplyr::rollup(g))"
  )
  write_fixture <- function(name, namespace, code, imports = character()) {
    pkg <- file.path(root, name)
    dir.create(file.path(pkg, "R"), recursive = TRUE)
    writeLines(c(
      paste("Package:", name), "Version: 0.0.1", "Title: Fixture",
      "Description: Fixture package.", "License: MIT",
      # `R CMD check --as-cran` sets `_R_CHECK_INSTALL_DEPENDS_`, under which
      # INSTALL lazy-loads with only DESCRIPTION's declared dependencies
      # visible, so an importFrom() needs its Imports entry.
      if (length(imports)) paste("Imports:", imports),
      paste0("Authors@R: person('A', 'B', email = 'a@example.com', ",
             "role = c('aut', 'cre'))")
    ), file.path(pkg, "DESCRIPTION"))
    writeLines(namespace, file.path(pkg, "NAMESPACE"))
    writeLines(code, file.path(pkg, "R", "fixture.R"))
    status <- system2(
      file.path(R.home("bin"), "R"),
      c("CMD", "INSTALL", "--no-byte-compile", "-l", shQuote(lib),
        shQuote(pkg)),
      stdout = TRUE, stderr = TRUE, env = paste0("R_LIBS=", shQuote(lib))
    )
    if (!is.null(attr(status, "status"))) {
      stop(paste(status, collapse = "\n"))
    }
  }
  write_fixture(
    "lazyacrossa", c("export(n_distinct)", "export(own_share)"),
    c("n_distinct <- function(x) length(unique(x))",
      paste0("own_share <- function(data) ", share_call))
  )
  write_fixture(
    "lazyacrossb",
    c("importFrom(lazyacrossa, n_distinct)", "export(imported_share)"),
    paste0("imported_share <- function(data) ", share_call),
    imports = "lazyacrossa"
  )
  lazy_load_fixture$lib <- lib
  lib
}

# Runs `code` with both fixtures freshly loaded from their lazy-load
# databases, so no binding of theirs has been forced yet.
with_fresh_lazy_load_fixture <- function(code) {
  lib <- lazy_load_fixture_lib()
  unload <- function() {
    if ("package:lazyacrossa" %in% search()) {
      detach("package:lazyacrossa")
    }
    for (name in c("lazyacrossb", "lazyacrossa")) {
      if (isNamespaceLoaded(name)) unloadNamespace(name)
    }
  }
  unload()
  on.exit(unload(), add = TRUE)
  # A literal name here reads to R CMD check's package-usage scan as an
  # undeclared dependency; the fixture is installed above into a temporary
  # library.
  fixture <- "lazyacrossb"
  loadNamespace(fixture, lib.loc = c(lib, .libPaths()))
  code()
}

expect_lazy_load_share <- function(result) {
  result <- as.data.frame(dplyr::collect(result))
  result <- result[match(c("a", "b", "Total"), result$g), , drop = FALSE]
  expect_identical(names(result), c("g", "x_n", "y_n", "s"))
  expect_equal(result$y_n, c(2, 1, 3))
  expect_equal(result$s, c(2 / 3, 1 / 3, 1))
}

expect_lazy_load_names <- function(make_input) {
  data <- data.frame(g = c("a", "a", "b"), x = c(1, 2, 5), y = c(10, 20, 30))
  with_fresh_lazy_load_fixture(function() {
    attachNamespace("lazyacrossa")
    attached <- as.environment("package:lazyacrossa")
    expect_true(rlang::env_binding_are_lazy(attached, "n_distinct"))
    caller <- rlang::new_environment(list(data = make_input(data)),
                                     parent = globalenv())
    expect_lazy_load_share(eval(quote(marginplyr::summarize_with_margins(
      data, dplyr::across(c(x, y), n_distinct, .names = "{.col}_n"),
      s = marginplyr::share_of_total(y_n),
      .grouping = marginplyr::rollup(g)
    )), caller))
  })
  with_fresh_lazy_load_fixture(function() {
    expect_true(rlang::env_binding_are_lazy(
      asNamespace("lazyacrossa"), "n_distinct"
    ))
    expect_lazy_load_share(
      getExportedValue("lazyacrossa", "own_share")(make_input(data))
    )
  })
  with_fresh_lazy_load_fixture(function() {
    imports <- parent.env(asNamespace("lazyacrossb"))
    expect_true(rlang::env_binding_are_lazy(imports, "n_distinct"))
    expect_lazy_load_share(
      getExportedValue("lazyacrossb", "imported_share")(make_input(data))
    )
  })
}

test_that("a lazy-load export names a share source on a data frame", {
  expect_lazy_load_names(identity)
})

test_that("a lazy-load export names a share source on a data.table", {
  skip_if_suggest_absent("data.table")
  expect_lazy_load_names(data.table::as.data.table)
})

test_that("a lazy-load export names a share source on dtplyr", {
  skip_if_suggest_absent("dtplyr")
  expect_lazy_load_names(dtplyr::lazy_dt)
})

test_that("a lazy-load export names a share source on DuckDB", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_lazy_load_names(function(data) {
    dplyr::copy_to(con, data, overwrite = TRUE)
  })
})

test_that("Arrow names a renamed across() of a lazy-load export", {
  skip_if_suggest_absent("arrow")
  data <- data.frame(g = c("a", "a", "b"), x = c(1, 2, 5))
  with_fresh_lazy_load_fixture(function() {
    attachNamespace("lazyacrossa")
    attached <- as.environment("package:lazyacrossa")
    expect_true(rlang::env_binding_are_lazy(attached, "n_distinct"))
    caller <- rlang::new_environment(
      list(data = arrow::arrow_table(data)), parent = globalenv()
    )
    result <- as.data.frame(dplyr::collect(eval(quote(
      marginplyr::summarize_with_margins(
        data, dplyr::across(c(a = x), n_distinct),
        .grouping = marginplyr::rollup(g)
      )
    ), caller)))
    result <- result[match(c("a", "b", "Total"), result$g), , drop = FALSE]
    expect_identical(names(result), c("g", "a"))
    expect_equal(result$a, c(2, 1, 3))
  })
})

test_that("an environment named like a package keeps its promise unread", {
  data <- data.frame(g = c("a", "b"), x = c(1, 2))
  reads <- 0L
  for (name in c("package:lazyacrossfake", "imports:stats", "tools:fake")) {
    imitation <- rlang::new_environment(parent = globalenv())
    attr(imitation, "name") <- name
    delayedAssign("fn", {
      reads <<- reads + 1L
      sum
    }, assign.env = imitation)
    caller <- rlang::new_environment(list(data = data), parent = imitation)
    expect_error(eval(quote(marginplyr::summarize_with_margins(
      data, dplyr::across(x, fn, .names = "{.col}_{.fn}"),
      share = marginplyr::share_of_parent(x_1),
      .grouping = marginplyr::rollup(g)
    )), caller), "unknown preceding ordinary summary `x_1`", info = name)
  }
  expect_identical(reads, 0L)
})
