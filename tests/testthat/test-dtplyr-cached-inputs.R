test_that("immutable cached renames survive multiple nesting branches", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(g = c("a", "b", "a"), value = 1:3)
  before <- data.table::copy(source)
  step <- dtplyr::lazy_dt(source) |>
    dplyr::filter(TRUE) |>
    dplyr::compute() |>
    dplyr::rename(amount = value)
  expected_input <- tibble::tibble(g = c("a", "b", "a"), amount = 1:3)
  expect_identical(dplyr::collect(step), expected_input)

  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    for (keep in c(FALSE, TRUE)) {
      for (iteration in 1:2) {
        result <- verb(step, .grouping = rollup(g), .sort = "last",
                       .id = "set", .keep = keep)
        if (identical(verb, nest_with_margins)) {
          expect_s3_class(result, "dtplyr_step")
          expect_identical(source, before)
          query <- result
          result <- dplyr::collect(query)
          expect_identical(dplyr::collect(query), result)
        } else {
          expect_s3_class(result, "rowwise_df")
          expect_identical(dplyr::group_vars(result), c("g", "set"))
        }
        expect_identical(result$g, c("a", "b", "Total"))
        expect_identical(result$set, c(1L, 1L, 2L))
        expect_identical(lapply(result$data, function(x) x$amount),
                         list(c(1L, 3L), 2L, 1:3))
        expect_true(all(vapply(result$data, function(x) {
          identical(names(x), if (keep) c("g", "amount") else "amount")
        }, logical(1))))
        if (keep) {
          expect_identical(lapply(result$data, function(x) x$g),
                           list(c("a", "a"), "b", c("a", "b", "a")))
        }
        expect_identical(source, before)
        expect_identical(dplyr::collect(step), expected_input)
        expect_identical(source, before)
      }
    }
  }
})

cached_margin_result <- function(result) {
  if (inherits(result, "dtplyr_step")) result <- dplyr::collect(result)
  result <- dplyr::ungroup(result)
  if ("data" %in% names(result)) {
    result$data <- lapply(result$data, tibble::as_tibble)
  }
  tibble::as_tibble(result)
}

cached_margin_consumers <- list(
  function(x, spec) {
    nest_with_margins(
      x, .grouping = spec, .sort = "last", .id = "set", .keep = TRUE
    )
  },
  function(x, spec) {
    nest_by_with_margins(
      x, .grouping = spec, .sort = "last", .id = "set", .keep = TRUE
    )
  },
  function(x, spec) {
    expand_with_margins(x, .grouping = spec, .sort = "last", .id = "set")
  },
  function(x, spec) {
    summarize_with_margins(
      x, n = dplyr::n(), total = sum(amount),
      .grouping = spec, .sort = "last", .id = "set"
    )
  }
)

test_that("cached renames preserve rollup, single-set, and recached controls", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(g = c("a", "b", "a"), value = 1:3)
  before <- data.table::copy(source)
  step <- dtplyr::lazy_dt(source) |>
    dplyr::filter(TRUE) |>
    dplyr::compute() |>
    dplyr::rename(amount = value)
  materialized <- tibble::tibble(g = c("a", "b", "a"), amount = 1:3)
  for (spec in list(grouping_set(g), rollup(g),
                    grouping_sets(rollup(g), grouping_set(amount)))) {
    for (consume in cached_margin_consumers) {
      expected <- cached_margin_result(consume(materialized, spec))
      for (input in list(step, dplyr::compute(step), materialized)) {
        result <- consume(input, spec)
        expect_identical(source, before)
        expect_identical(cached_margin_result(result), expected)
        expect_identical(source, before)
      }
    }
  }
})

test_that("cached expansion and portable summary retain repeated occurrences", {
  skip_if_suggest_absent("dtplyr")
  step <- dtplyr::lazy_dt(data.frame(g = c("a", "b", "a"), value = 1:3)) |>
    dplyr::filter(TRUE) |>
    dplyr::compute() |>
    dplyr::rename(amount = value)
  spec <- grouping_sets(rollup(g), rollup(g))
  expansion <- dplyr::collect(expand_with_margins(
    step, .grouping = spec, .duplicates = "keep", .id = "set", .sort = "last"
  ))
  for (set in 1:4) {
    expect_identical(sort(expansion$amount[expansion$set == set]), 1:3)
    expect_identical(expansion$g[expansion$set == set],
                     if (set %% 2L == 1L) c("a", "a", "b")
                     else rep("Total", 3L))
  }
  summary <- dplyr::collect(summarize_with_margins(
    step, n = dplyr::n(), total = sum(amount),
    .grouping = spec, .duplicates = "keep", .id = "set", .sort = "last"
  ))
  expect_identical(summary$g, c("a", "a", "b", "b", "Total", "Total"))
  expect_identical(summary$set, c(1L, 3L, 1L, 3L, 2L, 4L))
  expect_identical(summary$n, c(2L, 2L, 1L, 1L, 3L, 3L))
  expect_identical(summary$total, c(4L, 4L, 2L, 2L, 6L, 6L))
})

test_that("cached graph isolation stays deferred and protects saved bindings", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(g = c("a", "b", "a"), value = 1:3)
  before <- data.table::copy(source)
  seen <- integer()
  probe <- function(x) {
    seen <<- c(seen, length(x))
    min(x) > 0L
  }
  cached <- dtplyr::lazy_dt(source) |>
    dplyr::filter(probe(value)) |>
    dplyr::compute()
  renamed <- dplyr::rename(cached, amount = value)
  ordered <- dplyr::arrange(renamed, dplyr::desc(amount))
  nested <- dplyr::arrange(dplyr::compute(ordered), g)
  materialized <- tibble::tibble(g = c("a", "b", "a"), amount = 1:3)
  cases <- list(
    list(renamed, materialized),
    list(ordered, dplyr::arrange(materialized, dplyr::desc(amount))),
    list(nested, dplyr::arrange(
      dplyr::arrange(materialized, dplyr::desc(amount)), g
    )),
    list(dplyr::left_join(renamed, renamed, by = c("g", "amount")),
         materialized),
    list(dplyr::union_all(renamed, renamed),
         dplyr::union_all(materialized, materialized))
  )
  for (case in cases) {
    for (consume in cached_margin_consumers) {
      seen <- integer()
      expect_silent(result <- consume(case[[1L]], rollup(g)))
      if (inherits(result, "dtplyr_step")) {
        expect_identical(seen, integer())
      } else {
        expect_s3_class(result, "rowwise_df")
        expect_true(length(seen) > 0L && all(seen == 3L))
      }
      expect_identical(source, before)
      expect_identical(cached_margin_result(result),
                       cached_margin_result(consume(case[[2L]], rollup(g))))
      expect_true(length(seen) > 0L && all(seen == 3L))
      expect_identical(source, before)
    }
  }
})

test_that("cached rename isolation does not admit explicit Mutable steps", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(g = c("a", "b", "a"), value = 1:3)
  before <- data.table::copy(source)
  mutable <- dtplyr::lazy_dt(source, immutable = FALSE) |>
    dplyr::filter(TRUE) |>
    dplyr::compute() |>
    dplyr::rename(amount = value)
  for (consume in cached_margin_consumers) {
    expect_error(consume(mutable, rollup(g)),
                 "immutable = TRUE", class = "marginplyr_error")
    expect_identical(source, before)
  }
  expect_identical(
    inspect_grouping(mutable, .grouping = rollup(where(is.character))),
    inspect_grouping(tibble::as_tibble(before), .grouping = rollup(g))
  )
  expect_identical(source, before)
})

test_that("a copy-named immutable cache expands a single grouping set", {
  skip_if_suggest_absent("dtplyr")
  source <- dtplyr::lazy_dt(
    data.frame(g = c("a", "b"), amount = 1:2), immutable = TRUE
  ) |>
    dplyr::filter(amount == 1L)
  cached <- dplyr::compute(source, name = "copy")
  ordinary <- dplyr::compute(source, name = "ordinary_cache")
  expected <- tibble::tibble(g = "a", amount = 1L)

  expect_identical(dplyr::collect(cached), expected)
  result <- expand_with_margins(cached, .margin_label = NULL)
  expect_s3_class(result, "dtplyr_step")
  expect_identical(dplyr::collect(cached), expected)
  collected <- dplyr::collect(result)
  expect_identical(collected, expected)
  expect_identical(collected, dplyr::collect(
    expand_with_margins(ordinary, .margin_label = NULL)
  ))
  reused <- expand_with_margins(
    result, .grouping = grouping_set(where(is.character)),
    .margin_label = NULL
  )
  expect_identical(dplyr::collect(reused), expected)
  expect_identical(dplyr::collect(cached), expected)
})

test_that("reused immutable caches expand multiple sets without caller copy", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(g = c("a", "b"), amount = 1:2)
  before <- data.table::copy(source)
  caller <- new.env(parent = environment())
  copy_calls <- 0L
  row_reads <- 0L
  caller$copy <- function(x) {
    copy_calls <<- copy_calls + 1L
    data.table::copy(x)
  }
  caller$visit <- function(x) {
    row_reads <<- row_reads + length(x)
    rep(TRUE, length(x))
  }
  input <- evalq(
    dtplyr::lazy_dt(source, immutable = TRUE) |> dplyr::filter(visit(amount)),
    caller
  )

  expand_reused_cache <- function(cached) {
    reused <- dplyr::rename(cached, value = amount) |>
      dplyr::compute(name = "reused_cache")
    expand_with_margins(reused, .grouping = rollup(g), .margin_label = NULL,
                        .id = "set")
  }
  cached <- dplyr::compute(input, name = "copy")
  ordinary <- dplyr::compute(input, name = "ordinary_cache")
  expected_input <- tibble::tibble(g = c("a", "b"), amount = 1:2)

  result <- expand_reused_cache(cached)
  expect_s3_class(result, "dtplyr_step")
  # The filter counter detects source reads during query construction.
  expect_identical(row_reads, 0L)
  expect_identical(source, before)
  expect_identical(dplyr::collect(cached), expected_input)

  collected <- dplyr::collect(result)
  expect_identical(collected, tibble::tibble(
    g = c("a", "b", NA_character_, NA_character_),
    set = rep(1:2, each = 2), value = rep(1:2, 2)
  ))
  expect_identical(collected, dplyr::collect(expand_reused_cache(ordinary)))
  expect_identical(dplyr::collect(result), collected)
  expect_identical(dplyr::collect(cached), expected_input)
  expect_identical(source, before)
  # The caller binding returns an equivalent copy, so only the counter
  # detects that it was invoked.
  expect_identical(copy_calls, 0L)
})

test_that("generated copies preserve expansion payload under late namespaces", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr imports data.table; this remains a one-backend test.
  caller <- new.env(parent = environment())
  caller$source <- data.table::data.table(g = c("a", "b"), value = c(13L, 29L))
  before <- serialize(caller$source, NULL)
  input <- evalq(dtplyr::lazy_dt(source), caller)
  result <- expand_with_margins(
    input, .grouping = grouping_set(g), .margin_label = NULL
  )
  calls <- 0L
  caller$`::` <- function(package, name) {
    package <- as.character(substitute(package))
    name <- as.character(substitute(name))
    if (identical(c(package, name), c("data.table", "copy"))) {
      calls <<- calls + 1L
      return(function(x) {
        out <- data.table::copy(x)
        data.table::set(out, j = "value", value = rep(-99L, nrow(out)))
        out
      })
    }
    getExportedValue(package, name)
  }
  expected <- tibble::tibble(g = c("a", "b"), value = c(13L, 29L))
  expect_identical(dplyr::collect(input), expected)
  expect_identical(dplyr::collect(result), expected)
  expect_identical(calls, 0L)
  expect_identical(serialize(caller$source, NULL), before)
})


test_that("generated copies preserve immutable rows under caller namespaces", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr imports data.table; this remains a one-backend test.
  cases <- expand.grid(
    shape = c("root", "filter", "copy_cache", "renamed_cache"),
    family = c("one", "rollup", "repeated"),
    sort = c("none", "first", "last"), timing = c("early", "late"),
    mode = c("count", "empty", "error"), stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(cases))) local({
    case <- cases[i, ]
    label <- paste(unlist(case), collapse = "/")
    caller <- new.env(parent = environment())
    caller$source <- data.table::data.table(g = "a", value = 13L)
    before <- serialize(caller$source, NULL)
    row_reads <- 0L
    caller$visit <- function(x) {
      row_reads <<- row_reads + length(x)
      rep(TRUE, length(x))
    }
    input <- evalq(dtplyr::lazy_dt(source), caller)
    if (case$shape == "filter") {
      input <- dplyr::filter(input, visit(value))
    } else if (case$shape == "copy_cache") {
      input <- dplyr::compute(input, name = "copy")
    } else if (case$shape == "renamed_cache") {
      input <- dplyr::rename(dplyr::compute(input, name = "cache"), key = g)
    }
    key <- if (case$shape == "renamed_cache") "key" else "g"
    column <- rlang::sym(key)
    spec <- switch(case$family,
      one = grouping_set(!!column), rollup = rollup(!!column),
      repeated = grouping_sets(grouping_set(!!column), grouping_set(!!column))
    )
    calls <- 0L
    dispatcher <- function(package, name) {
      package <- as.character(substitute(package))
      name <- as.character(substitute(name))
      if (identical(c(package, name), c("data.table", "copy"))) {
        calls <<- calls + 1L
        if (case$mode == "error") stop("Caller copy namespace ran.")
        if (case$mode == "empty") return(function(x) utils::head(x, 0L))
      }
      getExportedValue(package, name)
    }
    if (case$timing == "early") caller$`::` <- dispatcher
    result <- expand_with_margins(
      input, .grouping = spec, .margin_label = NULL, .id = "set",
      .duplicates = "keep", .sort = case$sort
    )
    expect_identical(row_reads, 0L, info = label)
    expect_identical(calls, 0L, info = label)
    if (case$timing == "late") caller$`::` <- dispatcher
    expected_input <- tibble::tibble(!!key := "a", value = 13L)
    expect_identical(dplyr::collect(input), expected_input, info = label)
    expect_identical(dplyr::collect(dplyr::mutate(input, value = value)),
                     expected_input, info = label)
    expect_identical(dplyr::collect(dplyr::union_all(input, input)),
                     tibble::tibble(!!key := c("a", "a"), value = c(13L, 13L)),
                     info = label)
    expected <- switch(case$family,
      one = tibble::tibble(!!key := "a", set = 1L, value = 13L),
      rollup = tibble::tibble(!!key := c("a", NA_character_), set = 1:2,
                              value = c(13L, 13L)),
      repeated = tibble::tibble(!!key := c("a", "a"), set = 1:2,
                                value = c(13L, 13L))
    )
    collected <- dplyr::collect(result)
    expect_identical(dplyr::arrange(collected, set), expected, info = label)
    expect_identical(calls, 0L, info = label)
    rm("::", envir = caller)
    expect_identical(dplyr::collect(result), collected, info = label)
    expect_identical(dplyr::collect(input), expected_input, info = label)
    expect_identical(serialize(caller$source, NULL), before, info = label)
  })
})

test_that("authored source namespaces retain their caller lookup", {
  skip_if_suggest_absent("dtplyr")
  caller <- new.env(parent = environment())
  caller$source <- data.table::data.table(g = c("a", "b"), value = c(13L, 29L))
  before <- serialize(caller$source, NULL)
  input <- evalq(
    dtplyr::lazy_dt(source) |>
      dplyr::filter(data.table::copy(value) == value),
    caller
  )
  calls <- 0L
  caller$`::` <- function(package, name) {
    package <- as.character(substitute(package))
    name <- as.character(substitute(name))
    if (identical(c(package, name), c("data.table", "copy"))) {
      calls <<- calls + 1L
    }
    getExportedValue(package, name)
  }
  result <- expand_with_margins(
    input, .grouping = rollup(g), .margin_label = NULL, .id = "set"
  )
  expect_identical(calls, 0L)
  expected_input <- tibble::tibble(g = c("a", "b"), value = c(13L, 29L))
  expect_identical(dplyr::collect(input), expected_input)
  expect_identical(calls, 1L)
  calls <- 0L
  expect_identical(dplyr::collect(result), tibble::tibble(
    g = c("a", "b", NA_character_, NA_character_), set = c(1L, 1L, 2L, 2L),
    value = c(13L, 29L, 13L, 29L)
  ))
  expect_identical(calls, 1L)
  expect_identical(serialize(caller$source, NULL), before)
})
