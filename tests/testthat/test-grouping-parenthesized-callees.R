test_that("Grouping plans retain selections from parenthesized callees", {
  input <- tibble::tibble(a = c("x", "y"), b = c(1L, 1L))
  inspect <- function(spec) {
    inspect_grouping(input, .grouping = spec, .format = "list")$included
  }
  expand <- function(spec) {
    expand_with_margins(input, .grouping = spec)
  }
  nest_cells <- function(verb) {
    function(spec) {
      result <- verb(input, .grouping = spec)
      list(keys = result$a, cells = result$data)
    }
  }
  consumers <- list(
    inspect = inspect,
    expand = expand,
    nest = nest_cells(nest_with_margins),
    nest_by = nest_cells(nest_by_with_margins)
  )
  expected_nest <- list(
    keys = c("x", "y"),
    cells = list(input[1L, "b"], input[2L, "b"])
  )
  expected <- list(
    inspect = list("a"),
    expand = input,
    nest = expected_nest,
    nest_by = expected_nest
  )

  run <- function(consume, argument, pairs, second_condition) {
    observed <- rlang::env(
      head_reads = 0L, selector_reads = 0L, warnings = list()
    )
    first_warning <- simpleWarning("first selector evaluation")
    caller <- rlang::env()
    caller[["("]] <- function(x) {
      force(x)
      observed$head_reads <- observed$head_reads + 1L
      if (observed$selector_reads > 0L) {
        if (second_condition == "error") {
          stop("second callee evaluation")
        }
        warning("second callee evaluation")
        return(function(...) "b")
      }
      function(...) {
        observed$selector_reads <- observed$selector_reads + 1L
        warning(first_warning)
        "a"
      }
    }

    spec <- eval(rlang::call2("grouping_sets", argument), caller)
    result <- tryCatch(
      withCallingHandlers(consume(spec), warning = function(cnd) {
        observed$warnings <- c(observed$warnings, list(cnd))
        invokeRestart("muffleWarning")
      }),
      error = identity
    )
    expect_identical(observed$head_reads, pairs)
    expect_identical(observed$selector_reads, 1L)
    expect_identical(observed$warnings, list(first_warning))
    result
  }

  for (name in names(consumers)) {
    consume <- consumers[[name]]
    for (second_condition in c("warning", "error")) {
      expect_identical(
        run(consume, quote((rollup)()), 1L, second_condition),
        expected[[name]]
      )
      expect_identical(
        run(consume, quote(((marginplyr::rollup))()), 2L, second_condition),
        expected[[name]]
      )
    }
  }
})

test_that("direct selections and parenthesized nested calls keep their paths", {
  input <- tibble::tibble(a = c("x", "y"), b = c(1L, 1L))
  observed <- rlang::env(reads = 0L)
  select_a <- function() {
    observed$reads <- observed$reads + 1L
    "a"
  }
  direct <- inspect_grouping(
    input, .grouping = grouping_sets(select_a()), .format = "list"
  )
  nested <- inspect_grouping(
    input, .grouping = grouping_sets((marginplyr::rollup)(a)), .format = "list"
  )

  expect_identical(observed$reads, 1L)
  expect_identical(direct$included, list("a"))
  expect_identical(nested$included, list("a", character()))
})

test_that("an explicit qualified parenthesis operator retains its selection", {
  input <- tibble::tibble(a = "x", b = 1L)
  observed <- rlang::env(reads = 0L)
  rollup <- function() {
    observed$reads <- observed$reads + 1L
    if (observed$reads == 1L) "a" else "b"
  }

  plan <- inspect_grouping(
    input,
    .grouping = grouping_sets(base::`(`(rollup)()),
    .format = "list"
  )

  expect_identical(observed$reads, 1L)
  expect_identical(plan$included, list("a"))
})
