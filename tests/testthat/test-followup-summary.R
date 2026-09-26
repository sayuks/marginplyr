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
