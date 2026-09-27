test_that("dynamic local frames cannot replace grouping keys", {
  data <- tibble::tibble(fixed = c("p", "p"), g = c("b", "a"), v = 1:2)
  frame <- function(x) {
    stats::setNames(data.frame(sum(x)), "..marginplyr_key_1")
  }
  unpack <- "{inner}"
  summaries <- list(
    rlang::quo(frame(v)),
    rlang::quo(frame(dplyr::pick(v)[[1L]])),
    rlang::quo(dplyr::across(
      v, function(x) frame(x), .unpack = "{inner}"
    )),
    rlang::quo(dplyr::across(
      v, function(x) frame(x), .unpack = unpack
    ))
  )
  for (summary in summaries) {
    for (sorting in c("first", "last")) {
      error <- expect_error(
        rlang::inject(summarize_with_margins(
          data, !!summary, .by = fixed, .grouping = rollup(g),
          .sort = sorting
        )),
        class = "marginplyr_error"
      )
      expect_match(conditionMessage(error), "internal grouping columns")
    }
  }

  one_set <- expect_error(
    summarize_with_margins(
      data, frame(v), .grouping = grouping_set(g)
    ),
    class = "marginplyr_error"
  )
  expect_match(conditionMessage(one_set), "internal grouping columns")
  unpack_calls <- 0L
  dynamic_unpack <- function() {
    unpack_calls <<- unpack_calls + 1L
    "{inner}"
  }
  dynamic <- expect_error(
    summarize_with_margins(
      data,
      dplyr::across(v, frame, .unpack = dynamic_unpack()),
      .grouping = grouping_set(g)
    ),
    class = "marginplyr_error"
  )
  expect_match(conditionMessage(dynamic), "internal grouping columns")
  # dplyr reads a dynamic TRUE unpack while expanding and again in the first
  # group, where the conflicting name is refused.
  expect_identical(unpack_calls, 2L)

  explicit <- summarize_with_margins(
    data, `..marginplyr_key_1` = sum(v), .grouping = grouping_set(g)
  )
  expect_identical(explicit$g, data$g)
  expect_identical(explicit[["..marginplyr_key_1"]], data$v)

  public <- function(x) data.frame(g = sum(x))
  expect_error(
    summarize_with_margins(data, public(v), .grouping = grouping_set(g)),
    class = "marginplyr_error"
  )
  ordinary <- function(x) data.frame(extra = sum(x))
  result <- summarize_with_margins(
    data, ordinary(v), .grouping = grouping_set(g)
  )
  expect_identical(result$g, data$g)
  expect_identical(result$extra, data$v)
  scalar <- summarize_with_margins(
    data, sum(v), .grouping = grouping_set(g)
  )
  expect_named(scalar, c("g", "sum(v)"))
  expect_identical(scalar[["sum(v)"]], data$v)
})

test_that("unrelated shares do not rerun local across naming", {
  run <- function(with_share, grouping) {
    calls <- 0L
    after_calls <- 0L
    name <- function() {
      calls <<- calls + 1L
      "out"
    }
    after_name <- function() {
      after_calls <<- after_calls + 1L
      "after_out"
    }
    data <- tibble::tibble(g = c("a", "b"), v = 1:2)
    result <- if (with_share) {
      summarize_with_margins(
        data, total = sum(v),
        dplyr::across(v, sum, .names = name()),
        p = share_of_total(total), observed = calls,
        dplyr::across(v, mean, .names = after_name()),
        after_observed = after_calls,
        .grouping = grouping
      )
    } else {
      summarize_with_margins(
        data, total = sum(v),
        dplyr::across(v, sum, .names = name()),
        p = 1, observed = calls,
        dplyr::across(v, mean, .names = after_name()),
        after_observed = after_calls,
        .grouping = grouping
      )
    }
    list(result = result, calls = calls, after_calls = after_calls)
  }
  for (grouping in list(grouping_set(), rollup(g))) {
    control <- run(FALSE, grouping)
    actual <- run(TRUE, grouping)
    expect_identical(actual$calls, control$calls)
    expect_identical(actual$after_calls, control$after_calls)
    expect_identical(actual$result$out, control$result$out)
    expect_identical(actual$result$after_out, control$result$after_out)
    expect_identical(actual$result$observed, control$result$observed)
    expect_identical(
      actual$result$after_observed, control$result$after_observed
    )
    expect_identical(names(actual$result), names(control$result))
  }
})

test_that("expanded local frames cannot redefine a share source", {
  data <- tibble::tibble(g = c("a", "b"), v = c(1, 3))
  frame <- function(x) data.frame(total = max(x))
  unpack <- "{inner}"
  for (helper in list(rlang::quo(share_of_parent(total)),
                      rlang::quo(share_of_total(total)))) {
    for (frame_expr in list(
      rlang::quo(tibble::tibble(total = max(v))),
      rlang::quo(frame(v)),
      rlang::quo(dplyr::across(
        v, frame, .unpack = "{inner}"
      )),
      rlang::quo(dplyr::across(
        v, frame, .unpack = unpack
      ))
    )) {
      for (after in c(FALSE, TRUE)) {
        dots <- if (after) {
          list(rlang::quo(sum(v)), helper, frame_expr)
        } else {
          list(rlang::quo(sum(v)), frame_expr, helper)
        }
        names(dots) <- rep("", length(dots))
        names(dots)[[1L]] <- "total"
        names(dots)[[if (after) 2L else 3L]] <- "p"
        error <- expect_error(
          rlang::inject(summarize_with_margins(
            data, !!!dots, .grouping = rollup(g)
          )),
          class = "marginplyr_error",
          info = paste(rlang::as_label(frame_expr),
                       rlang::as_label(helper), after)
        )
        if (!is.null(error)) {
          expect_match(conditionMessage(error), "defined exactly once")
          expect_match(conditionMessage(error), "total")
        }
      }
    }
  }

  dynamic_unpack <- function() "{inner}"
  dynamic_error <- expect_error(
    summarize_with_margins(
      data, total = sum(v),
      dplyr::across(v, frame, .unpack = dynamic_unpack()),
      p = share_of_total(total), .grouping = rollup(g)
    ),
    class = "marginplyr_error"
  )
  expect_match(conditionMessage(dynamic_error), "defined exactly once")

  omitted <- summarize_with_margins(
    data, total = sum(v), tibble::tibble(total = NULL),
    p = share_of_total(total), .grouping = rollup(g)
  )
  expect_equal(omitted$p, c(0.25, 0.75, 1))
  repaired <- summarize_with_margins(
    data, total = sum(v),
    tibble::tibble(total = max(v),
                   .name_repair = function(x) paste0("other_", x)),
    p = share_of_total(total), .grouping = rollup(g)
  )
  expect_named(repaired, c("g", "total", "other_total", "p"))
  expect_equal(repaired$p, c(0.25, 0.75, 1))
  unique <- summarize_with_margins(
    data, total = sum(v), extra = max(v),
    p = share_of_total(total), .grouping = rollup(g)
  )
  expect_equal(unique$p, c(0.25, 0.75, 1))
})

test_that("ordinary local selections see prior summaries beside shares", {
  data <- tibble::tibble(g = c("a", "b"), v = c(1, 3))
  for (grouping in list(grouping_set(), rollup(g))) {
    result <- summarize_with_margins(
      data, total = sum(v),
      dplyr::across(total, identity, .names = "copy_{.col}"),
      p = share_of_total(total),
      dplyr::across(dplyr::starts_with("tot"), identity,
                    .names = "after_{.col}"),
      .grouping = grouping
    )
    expect_identical(result$copy_total, result$total)
    expect_identical(result$after_total, result$total)
    expect_identical(result$p[length(result$p)], 1)
  }
  expect_error(
    summarize_with_margins(
      data, total = sum(v), dplyr::across(unknown, identity),
      p = share_of_total(total), .grouping = rollup(g)
    ),
    "Column `unknown` doesn't exist"
  )
  expect_error(
    summarize_with_margins(
      data, total = sum(v), dplyr::across(total, identity,
                                          .names = "copied"),
      p = share_of_total(copied), .grouping = rollup(g)
    ),
    "depends on earlier summary alias"
  )
})

test_that("dynamic false unpack keeps ordinary function factory evaluation", {
  run <- function(dynamic) {
    calls <- 0L
    make_fn <- function() {
      calls <<- calls + 1L
      offset <- calls
      function(x) x + offset
    }
    data <- tibble::tibble(x = 1)
    result <- if (dynamic) {
      summarize_with_margins(
        data, dplyr::across(x, make_fn(), .unpack = identity(FALSE)),
        .grouping = grouping_set()
      )
    } else {
      summarize_with_margins(
        data, dplyr::across(x, make_fn(), .unpack = FALSE),
        .grouping = grouping_set()
      )
    }
    list(result = result, calls = calls)
  }
  literal <- run(FALSE)
  dynamic <- run(TRUE)
  expect_identical(literal$result$x, 2)
  expect_identical(literal$calls, 1L)
  expect_identical(dynamic, literal)
})

test_that("false unpack bindings retain local group results", {
  skip_if_suggest_absent("data.table")
  data <- tibble::tibble(g = c("a", "a", "b"), x = c(1, 2, 3))
  inputs <- list(data, as.data.frame(data), data.table::as.data.table(data))
  for (input in inputs) {
    run <- function(mode, listed) {
      calls <- 0L
      reads <- 0L
      make_fn <- function() {
        calls <<- calls + 1L
        offset <- calls
        function(x) sum(x) + offset
      }
      binding <- new.env(parent = environment())
      if (mode == "delayed") {
        delayedAssign("unpack", identity(FALSE), assign.env = binding)
      } else if (mode == "active") {
        makeActiveBinding("unpack", function() {
          reads <<- reads + 1L
          FALSE
        }, binding)
      }
      expression <- if (listed) {
        if (mode == "literal") {
          quote(summarize_with_margins(
            input, dplyr::across(x, list(a = make_fn()),
                                 .unpack = FALSE),
            .grouping = grouping_set(g)
          ))
        } else {
          quote(summarize_with_margins(
            input, dplyr::across(x, list(a = make_fn()),
                                 .unpack = unpack),
            .grouping = grouping_set(g)
          ))
        }
      } else if (mode == "literal") {
        quote(summarize_with_margins(
          input, dplyr::across(x, make_fn(), .unpack = FALSE),
          .grouping = grouping_set(g)
        ))
      } else {
        quote(summarize_with_margins(
          input, dplyr::across(x, make_fn(), .unpack = unpack),
          .grouping = grouping_set(g)
        ))
      }
      result <- eval(expression, binding)
      list(result = as.data.frame(result), calls = calls, reads = reads)
    }
    for (listed in c(FALSE, TRUE)) {
      literal <- run("literal", listed)
      delayed <- run("delayed", listed)
      active <- run("active", listed)
      expect_identical(literal$calls, 1L)
      expect_identical(delayed$result, literal$result)
      expect_identical(active$result, literal$result)
      expect_identical(delayed$calls, literal$calls)
      expect_identical(active$calls, literal$calls)
      expect_identical(active$reads, 1L)
    }
  }
})

test_that("unrelated share leaves packed across selection to local dplyr", {
  run <- function(with_share) {
    calls <- 0L
    choose <- function() {
      calls <<- calls + 1L
      if (calls == 1L) "x" else "y"
    }
    data <- tibble::tibble(x = 1, y = 10)
    result <- if (with_share) {
      summarize_with_margins(
        data, total = sum(x),
        packed = dplyr::across(dplyr::all_of(choose()), sum),
        p = share_of_total(total), .grouping = grouping_set()
      )
    } else {
      summarize_with_margins(
        data, total = sum(x),
        packed = dplyr::across(dplyr::all_of(choose()), sum),
        p = 1, .grouping = grouping_set()
      )
    }
    list(result = result, calls = calls)
  }
  control <- run(FALSE)
  actual <- run(TRUE)
  expect_identical(control$calls, 1L)
  expect_identical(actual, control)
  expect_identical(actual$result$packed$x, 1)
})

test_that("packed predicates see prior values beside shares", {
  data <- tibble::tibble(g = c("a", "b"), x = c(1, 2),
                         y = c(-10, -20))
  for (helper in list(rlang::quo(share_of_parent(total)),
                      rlang::quo(share_of_total(total)))) {
    actual <- rlang::inject(summarize_with_margins(
      data, total = sum(x),
      packed = dplyr::across(dplyr::where(~ all(.x > 0)), sum),
      p = !!helper, .grouping = rollup(g)
    ))
    expect_named(actual$packed, c("x", "total"))
    expect_identical(actual$packed$x, c(1, 2, 3))
    expect_identical(actual$packed$total, c(1, 2, 3))
    expect_equal(actual$p, c(1 / 3, 2 / 3, 1))
  }
})

test_that("packed selectors use active bindings beside shares", {
  data <- tibble::tibble(g = c("a", "b"), x = c(1, 2))
  for (helper in list(rlang::quo(share_of_parent(total)),
                      rlang::quo(share_of_total(total)))) {
    run <- function(with_share) {
      reads <- 0L
      binding <- new.env(parent = environment())
      makeActiveBinding("selected", function() {
        reads <<- reads + 1L
        "total"
      }, binding)
      result <- if (with_share) {
        evalq(rlang::inject(summarize_with_margins(
          data, total = sum(x),
          before = dplyr::across(dplyr::all_of(selected), identity),
          p = !!helper,
          after = dplyr::across(dplyr::all_of(selected), identity),
          .grouping = rollup(g)
        )), binding)
      } else {
        evalq(summarize_with_margins(
          data, total = sum(x),
          before = dplyr::across(dplyr::all_of(selected), identity),
          p = 1,
          after = dplyr::across(dplyr::all_of(selected), identity),
          .grouping = rollup(g)
        ), binding)
      }
      list(result = result, reads = reads)
    }
    actual <- run(TRUE)
    control <- run(FALSE)
    expect_identical(actual$reads, control$reads)
    expect_identical(actual$result$before$total, actual$result$total)
    expect_identical(actual$result$after$total, actual$result$total)
    expect_equal(actual$result$p, c(1 / 3, 2 / 3, 1))
  }
})

test_that("dynamic false unpack keeps factory counts beside shares", {
  run <- function(dynamic) {
    calls <- 0L
    factory <- function() {
      calls <<- calls + 1L
      offset <- calls
      function(x) sum(x) + offset
    }
    data <- tibble::tibble(g = c("a", "b"), x = c(1, 2))
    result <- if (dynamic) {
      summarize_with_margins(
        data, total = sum(x),
        dplyr::across(x, factory(), .names = "other",
                      .unpack = identity(FALSE)),
        p = share_of_total(total), .grouping = rollup(g)
      )
    } else {
      summarize_with_margins(
        data, total = sum(x),
        dplyr::across(x, factory(), .names = "other", .unpack = FALSE),
        p = share_of_total(total), .grouping = rollup(g)
      )
    }
    list(result = result, calls = calls)
  }
  expect_identical(run(TRUE), run(FALSE))
})

test_that("first packed selection uses the local summary mask beside a share", {
  data <- tibble::tibble(x = 1)
  expected <- dplyr::summarise(
    data,
    packed = dplyr::across(
      dplyr::all_of(if (dplyr::n() == 1L) "x" else character()), sum
    ),
    total = sum(x)
  )
  actual <- summarize_with_margins(
    data,
    packed = dplyr::across(
      dplyr::all_of(if (dplyr::n() == 1L) "x" else character()), sum
    ),
    total = sum(x), share = share_of_total(total)
  )
  expect_identical(actual$packed, expected$packed)
  expect_identical(actual$total, expected$total)
  expect_identical(actual$share, 1)
  expect_type(actual$share, "double")
})

test_that("packed selectors keep their summary context beside shares", {
  skip_if_suggest_absent("data.table")
  data <- tibble::tibble(g = c("a", "b", "b"), x = c(1, 10, 20))
  inputs <- list(data, as.data.frame(data), data.table::as.data.table(data))
  for (input in inputs) {
    for (helper in list(rlang::expr(share_of_parent(total)),
                        rlang::expr(share_of_total(total)))) {
      for (first in c(TRUE, FALSE)) {
        for (mode in c("direct", "callback", "active")) {
          run <- function(with_share) {
            reads <- 0L
            choose <- function() {
              reads <<- reads + 1L
              if (dplyr::n() > 0L) "x" else "missing"
            }
            binding <- new.env(parent = environment())
            makeActiveBinding("selected", function() {
              reads <<- reads + 1L
              if (dplyr::n() > 0L) "x" else "missing"
            }, binding)
            selector <- switch(
              mode,
              direct = rlang::expr(dplyr::all_of(
                if (dplyr::n() > 0L) "x" else "missing"
              )),
              callback = rlang::expr(dplyr::all_of(choose())),
              active = rlang::expr(dplyr::all_of(selected))
            )
            packed <- rlang::expr(dplyr::across(!!selector, sum))
            dots <- if (first) {
              list(packed = packed, total = rlang::expr(sum(x)))
            } else {
              list(total = rlang::expr(sum(x)), packed = packed)
            }
            dots$share <- if (with_share) helper else rlang::expr(1)
            result <- eval(rlang::expr(summarize_with_margins(
              input, !!!dots, .grouping = rollup(g)
            )), binding)
            list(result = as.data.frame(result), reads = reads)
          }
          control <- run(FALSE)
          actual <- run(TRUE)
          expect_identical(actual$reads, control$reads)
          expect_identical(actual$result$packed, control$result$packed)
          expect_identical(actual$result$total, control$result$total)
          expect_named(actual$result$packed, "x")
          expect_identical(actual$result$packed$x, c(1, 30, 31))
          expect_equal(actual$result$share, c(1 / 31, 30 / 31, 1))
        }
      }
    }
  }
})

test_that("a first packed selection stays ineligible as a share source", {
  data <- tibble::tibble(x = 1)
  error <- expect_error(summarize_with_margins(
    data,
    packed = dplyr::across(
      dplyr::all_of(if (dplyr::n() == 1L) "x" else character()), sum
    ),
    share = share_of_total(packed)
  ), class = "marginplyr_error")
  expect_match(conditionMessage(error), "named `across\\(\\)` packs")
})

test_that("dynamic unpack keeps dplyr's function evaluation context", {
  grouped <- tibble::tibble(g = c("a", "b", "b"), x = c(1, 10, 20))
  factory <- function(offset) {
    force(offset)
    function(x) data.frame(value = sum(x) + offset)
  }
  expected <- dplyr::summarise(
    dplyr::group_by(grouped, g),
    dplyr::across(x, factory(dplyr::n()), .unpack = identity(TRUE)),
    .groups = "drop"
  )
  actual <- summarize_with_margins(
    grouped, dplyr::across(x, factory(dplyr::n()),
                           .unpack = identity(TRUE)),
    .grouping = grouping_set(g)
  )
  expect_identical(expected$x_value, c(2, 32))
  expect_identical(actual, expected)

  one <- tibble::tibble(x = 1)
  scalar_factory <- function(offset) {
    force(offset)
    function(x) sum(x) + offset
  }
  expected_false <- dplyr::summarise(
    one, dplyr::across(x, scalar_factory(dplyr::n()),
                       .unpack = identity(FALSE))
  )
  actual_false <- summarize_with_margins(
    one, dplyr::across(x, scalar_factory(dplyr::n()),
                       .unpack = identity(FALSE))
  )
  expect_identical(expected_false$x, 1)
  expect_identical(actual_false, expected_false)
})

test_that("dynamic unpack matches ordinary dplyr across local frame classes", {
  skip_if_suggest_absent("data.table")
  data <- tibble::tibble(g = c("a", "b", "b"), x = c(1, 10, 20))
  inputs <- list(data, as.data.frame(data), data.table::as.data.table(data))
  run <- function(input, margin, unpack_value, mode, frame, listed) {
    factory_calls <- 0L
    unpack_reads <- 0L
    factory <- function(offset) {
      factory_calls <<- factory_calls + 1L
      force(offset)
      if (frame) {
        function(x) data.frame(value = sum(x) + offset)
      } else {
        function(x) sum(x) + offset
      }
    }
    binding <- new.env(parent = environment())
    binding$unpack_value <- unpack_value
    if (mode == "bound") {
      binding$unpack <- unpack_value
    } else if (mode == "delayed") {
      delayedAssign("unpack", {
        unpack_reads <<- unpack_reads + 1L
        unpack_value
      }, assign.env = binding, eval.env = binding)
    } else if (mode == "active") {
      makeActiveBinding("unpack", function() {
        unpack_reads <<- unpack_reads + 1L
        unpack_value
      }, binding)
    }
    unpack_expr <- switch(
      mode,
      literal = unpack_value,
      expression = rlang::expr(identity(unpack_value)),
      bound = rlang::sym("unpack"),
      delayed = rlang::sym("unpack"),
      active = rlang::sym("unpack")
    )
    fns <- if (listed) {
      rlang::expr(list(a = factory(dplyr::n()),
                       b = factory(dplyr::n())))
    } else {
      rlang::expr(factory(dplyr::n()))
    }
    across <- rlang::call2(
      "across", rlang::sym("x"), fns,
      .unpack = unpack_expr, .ns = "dplyr"
    )
    result <- if (margin) {
      eval(rlang::expr(summarize_with_margins(
        input, !!across, .grouping = grouping_set(g)
      )), binding)
    } else {
      eval(rlang::expr(dplyr::summarise(
        dplyr::group_by(input, g), !!across, .groups = "drop"
      )), binding)
    }
    list(result = as.data.frame(result),
         factory_calls = factory_calls, unpack_reads = unpack_reads)
  }
  for (input in inputs) {
    for (unpack_value in list(FALSE, TRUE, "{inner}_{outer}")) {
      for (mode in c("literal", "expression", "bound", "delayed", "active")) {
        for (frame in c(FALSE, TRUE)) {
          for (listed in c(FALSE, TRUE)) {
            expected <- run(input, FALSE, unpack_value, mode, frame, listed)
            actual <- run(input, TRUE, unpack_value, mode, frame, listed)
            expect_identical(
              actual, expected,
              info = paste(class(input)[[1L]], unpack_value, mode,
                           frame, listed)
            )
          }
        }
      }
    }
  }
})

test_that("dynamic false unpack preserves inline function environments", {
  skip_if_suggest_absent("data.table")
  data <- tibble::tibble(g = c("a", "b", "b"), x = c(1, 10, 20))
  inputs <- list(data, as.data.frame(data), data.table::as.data.table(data))
  functions <- list(
    rlang::expr(~ sum(.x) + dplyr::n()),
    rlang::expr(function(.x) sum(.x) + dplyr::n()),
    rlang::expr(list(a = ~ sum(.x), b = ~ sum(.x) + dplyr::n()))
  )
  for (input in inputs) {
    for (fns in functions) {
      across <- rlang::call2(
        "across", rlang::sym("x"), fns,
        .unpack = rlang::expr(identity(FALSE)), .ns = "dplyr"
      )
      expected <- eval(rlang::expr(dplyr::summarise(
        dplyr::group_by(input, g), !!across, .groups = "drop"
      )))
      actual <- eval(rlang::expr(summarize_with_margins(
        input, !!across, .grouping = grouping_set(g)
      )))
      expect_identical(as.data.frame(actual), as.data.frame(expected))
    }
  }
})

test_that("dynamic unpack leaves unusual across arguments to dplyr", {
  data <- tibble::tibble(x = 1:2)
  expected_error <- expect_error(dplyr::summarise(
    data, dplyr::across(x, 1L, .unpack = identity(FALSE))
  ))
  actual_error <- expect_error(summarize_with_margins(
    data, dplyr::across(x, 1L, .unpack = identity(FALSE))
  ))
  expect_identical(conditionMessage(actual_error),
                   conditionMessage(expected_error))

  expected <- dplyr::summarise(
    data, dplyr::across(x, sum, .names = 1L, .unpack = identity(FALSE))
  )
  actual <- summarize_with_margins(
    data, dplyr::across(x, sum, .names = 1L, .unpack = identity(FALSE))
  )
  expect_identical(actual, expected)
})

test_that("dynamic unpack keeps branch scope beside unrelated Total shares", {
  skip_if_suggest_absent("data.table")
  data <- tibble::tibble(g = c("a", "b", "b"), x = c(1, 10, 20))
  inputs <- list(data, as.data.frame(data), data.table::as.data.table(data))
  specifications <- list(
    rollup(g),
    grouping_sets(grouping_set(g), grouping_set(), grouping_set(g))
  )
  for (input in inputs) {
    for (grouping in specifications) {
      for (unpack_value in list(FALSE, TRUE, "{inner}_{outer}")) {
        run <- function(with_share) {
          factory_calls <- 0L
          unpack_reads <- 0L
          factory <- function(offset) {
            factory_calls <<- factory_calls + 1L
            force(offset)
            function(x) data.frame(value = sum(x) + offset)
          }
          binding <- new.env(parent = environment())
          makeActiveBinding("unpack", function() {
            unpack_reads <<- unpack_reads + 1L
            unpack_value
          }, binding)
          result <- if (with_share) {
            evalq(summarize_with_margins(
              input, dplyr::across(
                x, list(a = factory(dplyr::n())), .unpack = unpack
              ),
              total = sum(x), share = share_of_total(total),
              .grouping = grouping, .duplicates = "keep"
            ), binding)
          } else {
            evalq(summarize_with_margins(
              input, dplyr::across(
                x, list(a = factory(dplyr::n())), .unpack = unpack
              ),
              total = sum(x), share = 1,
              .grouping = grouping, .duplicates = "keep"
            ), binding)
          }
          list(result = as.data.frame(result),
               factory_calls = factory_calls, unpack_reads = unpack_reads)
        }
        control <- run(FALSE)
        actual <- run(TRUE)
        expect_identical(actual$factory_calls, control$factory_calls)
        expect_identical(actual$unpack_reads, control$unpack_reads)
        expect_identical(
          actual$result[names(actual$result) != "share"],
          control$result[names(control$result) != "share"]
        )
        expect_type(actual$result$share, "double")
        expect_equal(
          actual$result$share,
          if (nrow(actual$result) == 5L) {
            c(1 / 31, 30 / 31, 1, 1 / 31, 30 / 31)
          } else {
            c(1 / 31, 30 / 31, 1)
          }
        )
      }
    }
  }
})

test_that("dynamic unpack retains custom glue and invalid diagnostics", {
  data <- tibble::tibble(x = 1)
  frame <- function(x) data.frame(value = sum(x))
  suffix <- "post"
  unpack <- "{inner}_{suffix}"
  expected <- dplyr::summarise(
    data, dplyr::across(x, frame, .unpack = unpack)
  )
  actual <- summarize_with_margins(
    data, dplyr::across(x, frame, .unpack = unpack)
  )
  expect_identical(actual, expected)
  expect_named(actual, "value_post")

  unpack <- 1L
  expected_error <- expect_error(dplyr::summarise(
    data, dplyr::across(x, frame, .unpack = unpack)
  ))
  actual_error <- expect_error(summarize_with_margins(
    data, dplyr::across(x, frame, .unpack = unpack)
  ))
  expect_identical(actual_error$parent$message,
                   expected_error$parent$message)

  for (unpack in c("{outer", "{unknown}")) {
    expected_error <- expect_error(dplyr::summarise(
      data, dplyr::across(x, frame, .unpack = unpack)
    ))
    actual_error <- expect_error(summarize_with_margins(
      data, dplyr::across(x, frame, .unpack = unpack)
    ))
    expect_identical(actual_error$parent$message,
                     expected_error$parent$message)
    expect_identical(conditionMessage(actual_error),
                     conditionMessage(expected_error))
  }

  unpack <- identity(FALSE)
  suffix <- "post"
  name_template <- "{toupper(.col)}_{suffix}"
  expected <- dplyr::summarise(
    data, dplyr::across(x, sum, .names = name_template,
                        .unpack = unpack)
  )
  actual <- summarize_with_margins(
    data, dplyr::across(x, sum, .names = name_template,
                        .unpack = unpack)
  )
  expect_identical(actual, expected)
  for (name_template in c("{.col", "{unknown}")) {
    expected_error <- expect_error(dplyr::summarise(
      data, dplyr::across(x, sum, .names = name_template,
                          .unpack = unpack)
    ))
    actual_error <- expect_error(summarize_with_margins(
      data, dplyr::across(x, sum, .names = name_template,
                          .unpack = unpack)
    ))
    expect_identical(conditionMessage(actual_error),
                     conditionMessage(expected_error))
  }

  grouped <- tibble::tibble(g = "a", x = 1)
  run <- function(margin) {
    name_reads <- 0L
    name <- function() {
      name_reads <<- name_reads + 1L
      "g"
    }
    unpack <- identity(TRUE)
    result <- if (margin) {
      summarize_with_margins(
        grouped, dplyr::across(x, frame, .names = name(),
                               .unpack = unpack),
        .grouping = grouping_set(g)
      )
    } else {
      dplyr::summarise(
        dplyr::group_by(grouped, g),
        dplyr::across(x, frame, .names = name(), .unpack = unpack),
        .groups = "drop"
      )
    }
    list(result = result, name_reads = name_reads)
  }
  expect_identical(run(TRUE), run(FALSE))
})

test_that("changing dynamic unpack still checks packed output names", {
  data <- tibble::tibble(g = c("a", "b"), x = 1:2)
  frame <- function(x) data.frame(value = sum(x))
  run <- function(target, share = FALSE) {
    reads <- 0L
    unpack <- function() {
      reads <<- reads + 1L
      reads == 1L
    }
    name <- function() target
    if (share) {
      summarize_with_margins(
        data, total = sum(x),
        dplyr::across(x, frame, .names = name(), .unpack = unpack()),
        p = share_of_total(total), .grouping = rollup(g)
      )
    } else {
      summarize_with_margins(
        data,
        dplyr::across(x, frame, .names = name(), .unpack = unpack()),
        .grouping = grouping_set(g)
      )
    }
  }
  key <- expect_error(run("..marginplyr_key_1"),
                      class = "marginplyr_error")
  expect_match(conditionMessage(key), "internal grouping columns")
  public <- expect_error(run("g"), class = "marginplyr_error")
  expect_match(conditionMessage(public), "cannot overwrite grouping column")
  id_reads <- 0L
  id_unpack <- function() {
    id_reads <<- id_reads + 1L
    id_reads == 1L
  }
  id <- expect_error(summarize_with_margins(
    data, dplyr::across(x, frame, .names = "set",
                        .unpack = id_unpack()),
    .grouping = grouping_set(g), .id = "set"
  ), class = "marginplyr_error")
  expect_match(conditionMessage(id), "conflicts with a summary output")
  source <- expect_error(run("total", share = TRUE),
                         class = "marginplyr_error")
  expect_match(conditionMessage(source), "defined exactly once")

  source_default <- expect_error(summarize_with_margins(
    data, total = sum(x),
    dplyr::across(total, frame, .unpack = identity(FALSE)),
    p = share_of_total(total), .grouping = rollup(g)
  ), class = "marginplyr_error")
  expect_s3_class(source_default, "marginplyr_error")
  expect_match(conditionMessage(source_default), "defined exactly once")
  expect_false(grepl("local_checked_glue_name", conditionMessage(source_default),
                     fixed = TRUE))

  template <- "{inner}"
  string_collision <- expect_error(summarize_with_margins(
    data, dplyr::across(x, function(z) data.frame(g = sum(z)),
                        .unpack = template),
    .grouping = grouping_set(g)
  ), class = "marginplyr_error")
  expect_s3_class(string_collision, "marginplyr_error")
  expect_match(conditionMessage(string_collision), "cannot overwrite grouping")
  expect_false(grepl("local_checked_glue_name",
                     conditionMessage(string_collision), fixed = TRUE))
})
