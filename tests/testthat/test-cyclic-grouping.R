test_that("inspection refuses a self-referencing Grouping specification", {
  s <- grouping_sets(s, grouping_set())

  expect_error(
    inspect_grouping(data.frame(region = "E"), .grouping = s),
    "cyclic reference",
    class = "marginplyr_error"
  )
})

test_that("inspection and Margin verbs refuse cyclic nested references", {
  data <- data.frame(region = "E", units = 2)
  specifications <- list(
    local({
      s <- grouping_sets(s, grouping_set())
      s
    }),
    local({
      s <- grouping_set(region)
      s <- grouping_sets(s, grouping_set())
      s
    }),
    local({
      s <- grouping_sets(t, grouping_set())
      t <- grouping_spec(s, grouping_set(region))
      s
    }),
    local({
      s <- grouping_spec((s), grouping_set(region))
      s
    })
  )
  consumers <- list(
    function(s) inspect_grouping(data, .grouping = s),
    function(s) summarize_with_margins(data, total = sum(units), .grouping = s),
    function(s) expand_with_margins(data, .grouping = s),
    function(s) nest_with_margins(data, .grouping = s),
    function(s) nest_by_with_margins(data, .grouping = s)
  )

  for (s in specifications) {
    for (consume in consumers) {
      error <- expect_error(consume(s), class = "marginplyr_error")
      expect_match(conditionMessage(error), "cyclic reference", fixed = TRUE)
      expect_match(conditionMessage(error), "different name", fixed = TRUE)
      expect_match(
        conditionMessage(error),
        "s <- grouping_sets(!!s, grouping_set())",
        fixed = TRUE
      )
    }
  }
})

test_that("independent branches may reuse a nested specification", {
  data <- data.frame(region = "E", store = "A", units = 2)
  shared <- grouping_set(region)
  s <- grouping_sets(
    grouping_spec(shared, grouping_set(store)),
    grouping_sets(shared, grouping_set())
  )

  expect_identical(
    inspect_grouping(data, .grouping = s)$included,
    c("(region, store)", "(region)", "()")
  )
  expect_equal(
    summarize_with_margins(data, total = sum(units), .grouping = s)$total,
    rep(2, 3L)
  )
})

test_that("injecting the previous value permits same-name extension", {
  data <- data.frame(region = c("E", "W"), units = c(1, 2))
  s <- grouping_set(region)
  s <- grouping_sets(!!s, grouping_set())

  expect_identical(
    inspect_grouping(data, .grouping = s)$included,
    c("(region)", "()")
  )
  expect_equal(
    sort(summarize_with_margins(data, total = sum(units), .grouping = s)$total),
    c(1, 2, 3)
  )
})

# Active bindings observe evaluations that a promise would cache. The
# counters assert ADR 0008's one reading per recognized nested argument.
test_that("a cyclic nested argument is read once before refusal", {
  reads <- 0L
  env <- rlang::env(rlang::current_env())
  s <- rlang::eval_bare(quote(grouping_sets(next_spec)), env)
  makeActiveBinding("next_spec", function() {
    reads <<- reads + 1L
    s
  }, env)
  invisible(env$next_spec)
  expect_identical(reads, 1L)
  reads <- 0L

  data <- data.frame(region = "E", units = 2)
  expect_error(inspect_grouping(data, .grouping = s), class = "marginplyr_error")
  expect_identical(reads, 1L)
  reads <- 0L
  expect_error(
    summarize_with_margins(data, total = sum(units), .grouping = s),
    class = "marginplyr_error"
  )
  expect_identical(reads, 1L)
})

test_that("distinct finite specifications with identical contents are accepted", {
  reads <- 0L
  env <- rlang::env(rlang::current_env())
  makeActiveBinding("next_spec", function() {
    reads <<- reads + 1L
    expr <- if (reads < 3L) {
      quote(grouping_sets(next_spec))
    } else {
      quote(grouping_set(region))
    }
    rlang::eval_bare(expr, env)
  }, env)
  s <- rlang::eval_bare(quote(grouping_sets(next_spec)), env)
  data <- data.frame(region = "E", units = 2)

  expect_identical(inspect_grouping(data, .grouping = s)$included, "(region)")
  expect_identical(reads, 3L)
  reads <- 0L
  expect_equal(
    summarize_with_margins(data, total = sum(units), .grouping = s)$total,
    2
  )
  expect_identical(reads, 3L)
})
