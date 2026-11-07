# The receiver keeps ordinary names outside selection and distinguishes typed
# metadata from the unclassed integer-list proxy inside selection.
context_name_dispatch_data <- function() {
  state <- rlang::env(busy = FALSE, typed = 0L)
  registerS3method("names", "margin_context_names", function(x) {
    ordinary <- attr(x, "names")
    if (state$busy) {
      return(ordinary)
    }
    state$busy <- TRUE
    on.exit({
      state$busy <- FALSE
    })
    data <- tryCatch(tidyselect::peek_data(), error = function(cnd) NULL)
    if (!is.null(data) && is.character(data[[1L]])) {
      state$typed <- state$typed + 1L
      return(rev(ordinary))
    }
    ordinary
  }, envir = asNamespace("base"))
  data <- structure(data.frame(a = c("A", "B"), b = c(1L, 1L)),
                    class = c("margin_context_names", "data.frame"))
  list(data = data, state = state)
}

test_that("context-name fixed keys retain dispatched summary and Nest grain", {
  case <- context_name_dispatch_data()
  data <- case$data
  original <- serialize(data, NULL)
  selection <- rlang::quo(names(tidyselect::peek_data())[1L])
  expect_identical(names(data), c("a", "b"))
  expect_identical(dplyr::group_vars(dplyr::group_by(data, a)), "a")
  expect_identical(tidyselect::eval_select(selection, data), c(b = 2L))
  case$state$typed <- 0L

  expect_identical(
    inspect_grouping(data, .by = !!selection, .format = "list")$fixed,
    list("b")
  )
  expect_gt(case$state$typed, 0L)
  summary <- summarize_with_margins(data, count = dplyr::n(), .by = !!selection)
  expect_identical(summary$b, 1L)
  expect_identical(summary$count, 2L)
  for (nest in list(nest_with_margins, nest_by_with_margins)) {
    result <- nest(data, .by = !!selection)
    expect_identical(nrow(result), 1L)
    expect_identical(result$b, 1L)
    expect_identical(nrow(result$data[[1L]]), 2L)
  }
  expect_identical(serialize(data, NULL), original)
})

test_that("context names preserve receiver dispatch in every position", {
  case <- context_name_dispatch_data()
  data <- case$data
  original <- serialize(data, NULL)
  name_alias <- base::names
  selectors <- rlang::quos(
    names(tidyselect::peek_data())[1L],
    base::names(tidyselect::peek_data())[1L],
    (names)(tidyselect::peek_data())[1L],
    name_alias(tidyselect::peek_data())[1L],
    names(tidyselect::peek_data())[[1L]],
    all_of(names(tidyselect::peek_data())[1L]),
    c(names(tidyselect::peek_data())[1L]),
    names(tidyselect::peek_data())[1L:1L]
  )
  constructors <- list(grouping_set, grouping_sets, rollup, cube, grouping_spec)
  consumers <- list(inspect_grouping, expand_with_margins)
  for (selection in selectors) {
    expect_identical(tidyselect::eval_select(selection, data), c(b = 2L))
    expect_identical(
      tidyselect::eval_select(selection, list(a = 1L, b = 2L)), c(a = 1L)
    )
    wrapped <- rlang::quo(identity(!!selection))
    for (consumer in consumers) {
      expected <- consumer(data, .by = b, .grouping = grouping_set(a))
      case$state$typed <- 0L
      expect_identical(
        consumer(data, .by = !!selection, .grouping = grouping_set(a)),
        expected
      )
      expect_gt(case$state$typed, 0L)
      expect_identical(
        consumer(data, .by = !!wrapped, .grouping = grouping_set(a)),
        expected
      )
      for (constructor in constructors) {
        expected <- consumer(data, .by = a, .grouping = constructor(b))
        case$state$typed <- 0L
        expect_identical(
          consumer(data, .by = a, .grouping = constructor(!!selection)),
          expected
        )
        expect_gt(case$state$typed, 0L)
        expect_identical(
          consumer(data, .by = a, .grouping = constructor(!!wrapped)),
          expected
        )
      }
      expect_identical(serialize(data, NULL), original)
    }
  }
})

test_that("a mixed Grouping prefix keeps its context-name receiver", {
  case <- context_name_dispatch_data()
  data <- case$data
  original <- serialize(data, NULL)
  expect_identical(
    inspect_grouping(
      data, .by = a,
      .grouping = grouping_sets(
        names(tidyselect::peek_data())[1L], identity("b")
      ),
      .duplicates = "keep", .format = "list"
    )$included,
    list("b", "b")
  )
  expect_identical(serialize(data, NULL), original)
})

test_that("captured context-name operands retain their dispatched receiver", {
  case <- context_name_dispatch_data()
  data <- case$data
  original <- serialize(data, NULL)
  caller <- new.env(parent = baseenv())
  expression <- quote(base::`(`(marginplyr::grouping_set(
    !!names(tidyselect::peek_data())[1L]
  )))
  selection <- rlang::new_quosure(expression, caller)
  specification <- eval(rlang::call2("grouping_sets", selection))
  expect_identical(
    inspect_grouping(data, .by = a,
                     .grouping = specification, .format = "list"),
    inspect_grouping(data, .by = a,
                     .grouping = grouping_set(b), .format = "list")
  )
  expect_identical(serialize(data, NULL), original)
})
