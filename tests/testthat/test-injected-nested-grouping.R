# These consumers observe the shared Grouping specification contract through
# inspection and each public Margin verb.
injected_grouping_consumers <- function(data) {
  list(
    function(s) inspect_grouping(data, .grouping = s),
    function(s) summarize_with_margins(data, n = dplyr::n(), .grouping = s),
    function(s) expand_with_margins(data, .grouping = s),
    function(s) nest_with_margins(data, .grouping = s),
    function(s) nest_by_with_margins(data, .grouping = s)
  )
}

test_that("injected nested kinds keep their plan through parentheses", {
  data <- data.frame(region = c("E", "W"), units = c(1, 2))
  consumers <- injected_grouping_consumers(data)
  constructors <- c("grouping_set", "grouping_sets", "rollup", "cube",
                    "grouping_spec")
  parents <- c("grouping_sets", "grouping_spec", "rollup", "cube")

  for (parent in parents) {
    admitted <- if (parent %in% c("rollup", "cube")) {
      "grouping_set"
    } else {
      constructors
    }
    for (constructor in admitted) {
      constructor_call <- rlang::call2(constructor, quote(region))
      bound <- eval(constructor_call)
      for (payload in list(constructor_call, quote(bound))) {
        q <- rlang::new_quosure(payload, rlang::current_env())
        plain <- eval(rlang::call2(parent, q))
        for (depth in c(1L, 3L)) {
          wrapped <- q
          for (i in seq_len(depth)) {
            wrapped <- rlang::call2("(", wrapped)
          }
          s <- eval(rlang::call2(parent, wrapped))
          for (consume in consumers) {
            expect_identical(consume(s), consume(plain))
          }
        }
      }
    }
  }
})

# jarl-ignore unnecessary_parentheses: Embracing with parentheses is under test.
test_that("embracing a nested specification keeps its captured environment", {
  data <- data.frame(region = c("E", "W"), units = c(1, 2))
  forward <- function(x) {
    rlang::env_bind(rlang::current_env(), bound = grouping_set(units))
    grouping_sets((({{ x }})))
  }
  bound <- grouping_set(region)
  expected <- grouping_sets(grouping_set(region))

  for (s in list(forward(bound), forward(grouping_set(region)))) {
    for (consume in injected_grouping_consumers(data)) {
      expect_identical(consume(s), consume(expected))
    }
  }
})

test_that("alternating injected quosures retain the innermost environment", {
  data <- data.frame(region = "E", units = 2)
  inner <- rlang::env(rlang::current_env(), s = grouping_set(region))
  outer <- rlang::env(rlang::current_env(), s = grouping_set(units))
  q <- rlang::new_quosure(quote(s), inner)
  for (i in 1:3) {
    q <- rlang::new_quosure(rlang::call2("(", q), outer)
  }
  s <- eval(rlang::call2("grouping_sets", q), outer)

  expect_identical(inspect_grouping(data, .grouping = s)$included, "(region)")
})

test_that("injected nested names retain ambiguity and cycle refusals", {
  data <- data.frame(region = "E", s = "column", units = 2)
  forward <- function(x) grouping_sets(({{ x }}))
  s <- grouping_set(region)
  ambiguous <- forward(s)
  cyclic <- local({
    s <- forward(s)
    s
  })

  for (consume in injected_grouping_consumers(data)) {
    expect_error(
      consume(ambiguous), "both a column", class = "marginplyr_error"
    )
    expect_error(consume(cyclic), "both a column", class = "marginplyr_error")
  }
  data$s <- NULL
  for (consume in injected_grouping_consumers(data)) {
    expect_error(
      consume(cyclic), "cyclic reference", class = "marginplyr_error"
    )
  }
})

test_that("injected specifications retain parent-kind restrictions", {
  data <- data.frame(region = "E", units = 2)
  in_set <- function(x) grouping_set(({{ x }}))
  in_rollup <- function(x) rollup(({{ x }}))
  in_cube <- function(x) cube(({{ x }}))

  for (consume in injected_grouping_consumers(data)) {
    expect_error(
      consume(in_set(grouping_set(region))),
      "not another grouping family", class = "marginplyr_error"
    )
    for (wrap in list(in_rollup, in_cube)) {
      expect_error(
        consume(wrap(cube(region))),
        "only accepts columns", class = "marginplyr_error"
      )
      expect_error(
        consume(wrap(grouping_set())),
        "empty.*composite dimension", class = "marginplyr_error"
      )
    }
  }
})

# Active bindings expose repeated reads that promises cache. These counters
# observe ADR 0008's single-evaluation contract through the public consumers.
test_that("injected nested constructors and names are evaluated once", {
  data <- data.frame(region = "E", units = 2)
  forward <- function(x) grouping_sets(({{ x }}))
  counts <- rlang::env(reads = 0L)
  env <- rlang::env(rlang::current_env())
  makeActiveBinding("s", function() {
    counts$reads <- counts$reads + 1L
    grouping_set(region)
  }, env)
  env$grouping_set <- function(...) {
    counts$reads <- counts$reads + 1L
    marginplyr::grouping_set(...)
  }
  invisible(env$s)
  expect_identical(counts$reads, 1L)
  env$grouping_set(region)
  expect_identical(counts$reads, 2L)

  for (payload in list(quote(s), quote(grouping_set(region)),
                       quote(grouping_set(where(is.character))))) {
    s <- eval(rlang::call2("forward", payload), env)
    for (consume in injected_grouping_consumers(data)) {
      counts$reads <- 0L
      result <- consume(s)
      expect_identical(counts$reads, 1L)
      expect_identical(nrow(result), 1L)
    }
  }
})
