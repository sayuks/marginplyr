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
  expect_identical(unpack_calls, 1L)

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
