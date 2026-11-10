test_that("first across selectors retain Grouping context beside shares", {
  data <- tibble::tibble(g = c("a", "b"), x = c(1, 3))
  actual <- summarize_with_margins(
    data,
    dplyr::across(if (grouping_bit(g) == 0L) "x" else "x", sum,
                  .names = "ordinary"),
    total = sum(x),
    dplyr::across(dplyr::starts_with("total"), share_of_total,
                  .names = "{.col}_p"),
    .grouping = rollup(g)
  )
  expect_identical(actual$ordinary, c(1, 3, 4))
  expect_identical(actual$total, c(1, 3, 4))
  expect_identical(actual$total_p, c(0.25, 0.75, 1))
})

test_that("first across name templates retain their Grouping context", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  actual <- summarize_with_margins(
    data,
    dplyr::across(
      x, sum, .names = if (grouping_id(g) == 0L) "ordinary" else "ordinary"
    ),
    total = sum(x),
    dplyr::across(dplyr::starts_with("total"), share_of_parent,
                  .names = "{.col}_p"),
    .grouping = rollup(g)
  )
  expect_identical(actual$ordinary, c(1, 3, 4))
  expect_identical(actual$total_p, c(0.25, 0.75, 1))
})

test_that("Grouping-aware planning preserves explicit default name positions", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  actual <- summarize_with_margins(
    data,
    dplyr::across(.names = NULL,
                  .cols = if (grouping_bit(g) == 0L) "x" else "x", .fns = sum),
    dplyr::across(dplyr::starts_with("x"), share_of_total, .names = "p"),
    .grouping = rollup(g)
  )
  expect_identical(actual$x, c(1, 3, 4))
  expect_identical(actual$p, c(0.25, 0.75, 1))
})

check_first_grouping_across <- function(convert) {
  data <- convert(data.frame(g = c("a", "b"), x = c(1, 3)))
  before <- as.list(data)
  column <- "x"
  template <- "ordinary"
  dimension <- rlang::sym("g")
  for (helper in rlang::syms(c("grouping_bit", "grouping_id"))) {
    for (argument in c("selector", "template")) {
      ordinary <- if (argument == "selector") {
        rlang::quos(dplyr::across(
          if ((!!helper)(!!dimension) == 0L) column else column,
          sum, .names = template
        ))
      } else {
        rlang::quos(dplyr::across(
          dplyr::all_of(column), sum,
          .names = if ((!!helper)(!!dimension) == 0L) template else template
        ))
      }
      for (share in rlang::syms(c("share_of_parent", "share_of_total"))) {
        for (source in c("ordinary", "total")) {
          for (pattern in c(FALSE, TRUE)) {
            request <- if (pattern) {
              rlang::quos(dplyr::across(
                dplyr::starts_with(!!source), !!share, .names = "p"
              ))
            } else {
              rlang::quos(p = (!!share)(!!rlang::sym(source)))
            }
            actual <- summarize_with_margins(
              data, !!!ordinary, total = sum(!!rlang::sym("x")), !!!request,
              .grouping = rollup("g")
            )
            expect_identical(as.list(actual), list(
              g = c("a", "b", "Total"), ordinary = c(1, 3, 4),
              total = c(1, 3, 4), p = c(0.25, 0.75, 1)
            ), info = paste(helper, argument, share, source, pattern))
          }
        }
      }
    }
  }
  expect_identical(as.list(data), before)
}

test_that("local share selection preserves Grouping-aware across arguments", {
  check_first_grouping_across(identity)
  check_first_grouping_across(tibble::as_tibble)
})

test_that("eager tables preserve Grouping-aware across beside shares", {
  skip_if_suggest_absent("data.table")
  check_first_grouping_across(data.table::as.data.table)
})

test_that("independent across selectors resume each grain's Grouping context", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), y = c(10, 30))
  actual <- summarize_with_margins(
    data,
    dplyr::across(
      if (grouping_bit(g) == 0L) c(ordinary = "x") else c(ordinary = "y"),
      sum
    ),
    total = sum(x),
    dplyr::across(dplyr::starts_with("total"), share_of_total, .names = "p"),
    .grouping = rollup(g)
  )
  expect_identical(actual$ordinary, c(1, 3, 40))
  expect_identical(actual$p, c(0.25, 0.75, 1))
})

test_that("Grouping-aware planning preserves caller conditions", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  cause <- rlang::error_cnd("context_cause", message = "context cause")
  failure <- rlang::error_cnd(
    "context_failure", message = "context failure", parent = cause
  )
  for (argument in c("selector", "template")) {
    for (fails in c(FALSE, TRUE)) {
      warnings <- list()
      observe <- function(bit, value) {
        force(bit)
        warning("context warning")
        if (fails) stop(failure)
        value
      }
      ordinary <- if (argument == "selector") {
        rlang::quos(dplyr::across(
          observe(grouping_bit(g), "x"), sum, .names = "ordinary"
        ))
      } else {
        rlang::quos(dplyr::across(
          x, sum, .names = observe(grouping_id(g), "ordinary")
        ))
      }
      result <- tryCatch(withCallingHandlers(summarize_with_margins(
        data, !!!ordinary, total = sum(x),
        dplyr::across(
          dplyr::starts_with("total"), share_of_total, .names = "p"
        ),
        .grouping = rollup(g)
      ), warning = function(cnd) {
        warnings[[length(warnings) + 1L]] <<- cnd
        invokeRestart("muffleWarning")
      }), error = identity)
      expect_length(warnings, if (fails && argument == "selector") 0L else 1L)
      if (length(warnings) > 0L) {
        expect_match(conditionMessage(warnings[[1L]]), "context warning")
      }
      if (fails) {
        while (!inherits(result, "context_failure") &&
                 !is.null(result$parent)) {
          result <- result$parent
        }
        expect_identical(result, failure)
        expect_identical(result$parent, cause)
      } else {
        expect_identical(result$p, c(0.25, 0.75, 1))
        expect_match(conditionMessage(warnings[[1L]]),
                     "1 further grouping set", fixed = TRUE)
      }
    }
  }
})

test_that("Grouping-aware source naming reports its one planned warning", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  warnings <- character()
  template <- function() {
    warning("source naming warning")
    "ordinary"
  }
  actual <- withCallingHandlers(summarize_with_margins(
    data,
    dplyr::across(
      x, sum, .names = if (grouping_id(g) == 0L) template() else template()
    ),
    dplyr::across(dplyr::starts_with("ordinary"), share_of_total, .names = "p"),
    .grouping = rollup(g)
  ), warning = function(cnd) {
    warnings <<- c(warnings, conditionMessage(cnd))
    invokeRestart("muffleWarning")
  })
  expect_identical(actual$ordinary, c(1, 3, 4))
  expect_identical(actual$p, c(0.25, 0.75, 1))
  expect_length(warnings, 1L)
  expect_match(warnings, "source naming warning")
  expect_false(grepl("further grouping set", warnings, fixed = TRUE))
})

test_that("first across planning preserves naming and selector conditions", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), y = c(10, 30))
  cause <- rlang::error_cnd("naming_cause", message = "naming cause")
  failure <- rlang::error_cnd(
    "naming_failure", message = "naming failure", parent = cause
  )
  for (fails in c(FALSE, TRUE)) {
    for (source in c(FALSE, TRUE)) {
      warnings <- list()
      reads <- 0L
      selections <- 0L
      template <- function() {
        reads <<- reads + 1L
        warning("naming warning")
        if (fails) stop(failure)
        if (source) "total" else "unused"
      }
      choose <- function() {
        selections <<- selections + 1L
        warning("selector warning")
        if (selections == 1L) "x" else "y"
      }
      result <- tryCatch(withCallingHandlers(summarize_with_margins(
        data,
        dplyr::across(dplyr::all_of(choose()), sum, .names = template()),
        ordinary = sum(x),
        dplyr::across(
          dplyr::starts_with(if (source) "total" else "ordinary"),
          share_of_total, .names = "share"
        ),
        .grouping = rollup(g)
      ), warning = function(cnd) {
        warnings[[length(warnings) + 1L]] <<- cnd
        invokeRestart("muffleWarning")
      }), error = identity)
      expect_identical(reads, if (fails || source) 1L else 2L)
      expect_length(warnings, 2L)
      messages <- vapply(warnings, conditionMessage, character(1))
      expect_identical(sum(grepl("naming warning", messages)), 1L)
      expect_identical(sum(grepl("selector warning", messages)), 1L)
      if (fails) {
        expect_identical(result, failure)
        expect_identical(result$parent, cause)
        expect_identical(messages, c("selector warning", "naming warning"))
      } else {
        expect_equal(result$share, if (source) c(1 / 40, 3 / 40, 1) else
                       c(0.25, 0.75, 1))
        expect_match(messages[[1L]], "naming warning", fixed = TRUE)
        if (!source) {
          expect_match(messages[[1L]], ".names = template()", fixed = TRUE)
        }
        expect_identical(selections, 2L)
        expect_identical(result[[if (source) "total" else "unused"]],
                         c(1, 3, 40))
        expect_match(messages[[2L]], "1 further grouping set")
      }
    }
  }
})

test_that("nested selection detection unwraps injected expressions", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  selection <- rlang::quo(dplyr::across(x, sum))
  actual <- summarize_with_margins(
    data, identity(!!selection), .grouping = rollup(g)
  )
  expect_identical(actual$x, c(1, 3, 4))
  value <- rlang::quo(1)
  scalar <- summarize_with_margins(
    data, identity(!!value), .grouping = rollup(g)
  )
  expect_identical(scalar[["identity(1)"]], c(1, 1, 1))
})

test_that("ordinary first across templates report repeated warnings once", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  shares <- list(
    rlang::quos(),
    rlang::quos(p = share_of_total(total)),
    rlang::quos(dplyr::across(total, share_of_total, .names = "p")),
    rlang::quos(dplyr::across(
      dplyr::starts_with("total"), share_of_total, .names = "p"
    ))
  )
  for (share in shares) {
    events <- character()
    warnings <- list()
    inputs <- list()
    measure <- function(value) {
      events <<- c(events, "summary")
      inputs[[length(inputs) + 1L]] <<- value
      sum(value)
    }
    template <- function() {
      events <<- c(events, "template")
      warning("context warning")
      "ordinary"
    }
    actual <- withCallingHandlers(summarize_with_margins(
      data,
      dplyr::across(x, ~measure(.x) + grouping_bit(g), .names = template()),
      total = sum(x), !!!share, .grouping = rollup(g)
    ), warning = function(cnd) {
      warnings[[length(warnings) + 1L]] <<- cnd
      events <<- c(events, "report")
      invokeRestart("muffleWarning")
    })
    expect_identical(events, c(
      "template", "summary", "summary", "template", "summary", "report"
    ))
    expect_identical(inputs, list(1, 3, c(1, 3)))
    expect_length(warnings, 1L)
    message <- conditionMessage(warnings[[1L]])
    expect_match(message, "context warning", fixed = TRUE)
    expect_match(message, "1 further grouping set raised this warning.",
                 fixed = TRUE)
    expect_match(message, ".names = template()", fixed = TRUE)
    expect_match(message, "grouping_bit(g)", fixed = TRUE)
    expect_identical(actual$ordinary, c(1, 3, 5))
    expect_identical(actual$total, c(1, 3, 4))
    if (length(share) > 0L) expect_identical(actual$p, c(0.25, 0.75, 1))
  }
})

test_that("ordinary template warnings retain their originating conditions", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  cause <- rlang::error_cnd("template_cause", message = "template cause")
  shares <- list(
    rlang::quos(),
    rlang::quos(p = share_of_total(total)),
    rlang::quos(dplyr::across(total, share_of_total, .names = "p")),
    rlang::quos(dplyr::across(
      dplyr::starts_with("total"), share_of_total, .names = "p"
    ))
  )
  for (share in shares) {
    condition <- rlang::warning_cnd(
      "template_warning", message = "template warning", parent = cause,
      payload = list(value = new.env(parent = emptyenv()))
    )
    warnings <- list()
    template <- function() {
      warning(condition)
      "ordinary"
    }
    withCallingHandlers(summarize_with_margins(
      data,
      dplyr::across(x, ~sum(.x) + grouping_bit(g), .names = template()),
      total = sum(x), !!!share, .grouping = rollup(g)
    ), warning = function(cnd) {
      warnings[[length(warnings) + 1L]] <<- cnd
      invokeRestart("muffleWarning")
    })
    expect_length(warnings, 1L)
    expect_s3_class(warnings[[1L]], "rlang_warning")
    expect_match(conditionMessage(warnings[[1L]]), "template warning",
                 fixed = TRUE)
    # dplyr retains the structured causes behind its aggregate report.
    stored <- Filter(function(warning) {
      identical(warning$parent$payload, condition$payload)
    }, dplyr::last_dplyr_warnings(n = Inf))
    expect_length(stored, 2L)
    for (warning in stored) {
      expect_identical(class(warning$parent), class(condition))
      expect_identical(warning$parent$message, condition$message)
      expect_identical(warning$parent$parent, cause)
      expect_identical(warning$parent$payload, condition$payload)
    }
  }
})

test_that("ordinary template failures retain their custom class and cause", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  cause <- rlang::error_cnd("template_cause", message = "template cause")
  failure <- rlang::error_cnd(
    "template_failure", message = "template failure", parent = cause,
    payload = list(value = 42)
  )
  shares <- list(
    rlang::quos(),
    rlang::quos(p = share_of_total(total)),
    rlang::quos(dplyr::across(total, share_of_total, .names = "p")),
    rlang::quos(dplyr::across(
      dplyr::starts_with("total"), share_of_total, .names = "p"
    ))
  )
  for (share in shares) {
    calls <- 0L
    template <- function() {
      calls <<- calls + 1L
      stop(failure)
    }
    actual <- tryCatch(summarize_with_margins(
      data,
      dplyr::across(x, ~sum(.x) + grouping_bit(g), .names = template()),
      total = sum(x), !!!share, .grouping = rollup(g)
    ), error = identity)
    while (!inherits(actual, "template_failure") && !is.null(actual$parent)) {
      actual <- actual$parent
    }
    expect_identical(actual, failure)
    expect_identical(actual$parent, cause)
    expect_identical(calls, 1L)
  }
})

test_that("ordinary templates retain their warn = 2 failure boundary", {
  rlang::local_options(warn = 2L)
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  shares <- list(
    rlang::quos(),
    rlang::quos(p = share_of_total(total)),
    rlang::quos(dplyr::across(total, share_of_total, .names = "p")),
    rlang::quos(dplyr::across(
      dplyr::starts_with("total"), share_of_total, .names = "p"
    ))
  )
  for (i in seq_along(shares)) {
    calls <- 0L
    template <- function() {
      calls <<- calls + 1L
      warning("template warning")
      "ordinary"
    }
    actual <- tryCatch(summarize_with_margins(
      data,
      dplyr::across(x, ~sum(.x) + grouping_bit(g), .names = template()),
      total = sum(x), !!!shares[[i]], .grouping = rollup(g)
    ), error = identity)
    expect_s3_class(actual, "error")
    expect_match(conditionMessage(actual), "converted from warning",
                 fixed = TRUE)
    expect_identical(calls, if (i == 4L) 1L else 2L)
  }
})

test_that("ordinary templates retain distinct diagnostics and arguments", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  for (distinct_argument in c(FALSE, TRUE)) {
    calls <- 0L
    warnings <- character()
    template <- function() {
      calls <<- calls + 1L
      warning(if (distinct_argument || calls == 1L) "first warning" else
                "second warning")
      "ordinary"
    }
    extra <- if (distinct_argument) {
      rlang::quos(other = {
        warning("first warning")
        sum(x)
      })
    } else {
      rlang::quos()
    }
    actual <- withCallingHandlers(summarize_with_margins(
      data,
      dplyr::across(x, ~sum(.x) + grouping_bit(g), .names = template()),
      total = sum(x),
      dplyr::across(dplyr::starts_with("total"), share_of_total, .names = "p"),
      !!!extra, .grouping = rollup(g)
    ), warning = function(cnd) {
      warnings <<- c(warnings, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    })
    expect_length(warnings, 2L)
    expect_match(warnings[[1L]], "first warning", fixed = TRUE)
    expect_match(warnings[[1L]], ".names = template()", fixed = TRUE)
    if (distinct_argument) {
      expect_match(warnings[[1L]], "1 further grouping set", fixed = TRUE)
      expect_match(warnings[[2L]], "other =", fixed = TRUE)
      expect_match(warnings[[2L]], "first warning", fixed = TRUE)
    } else {
      expect_match(warnings[[2L]], "second warning", fixed = TRUE)
      expect_false(any(grepl("further grouping set", warnings, fixed = TRUE)))
    }
    expect_identical(actual$p, c(0.25, 0.75, 1))
    expect_identical(calls, 2L)
  }
})
