test_that("nested Margin summaries keep their own Grouping plan", {
  outer <- tibble::tibble(g = "a")
  inner <- tibble::tibble(g = "b", h = "c", value = 2)
  independent <- function() {
    summarize_with_margins(
      inner,
      bit = grouping_bit(g),
      id = marginplyr::grouping_id(g, h),
      .grouping = cube(g, h)
    )
  }
  expected <- independent()
  expect_identical(expected$bit, c(0L, 0L, 1L, 1L))
  expect_identical(expected$id, c(0L, 1L, 2L, 3L))

  actual <- summarize_with_margins(
    outer,
    nested = list(marginplyr::summarize_with_margins(
      inner,
      bit = grouping_bit(g),
      id = marginplyr::grouping_id(g, h),
      .grouping = cube(g, h)
    )),
    control = list(independent()),
    .grouping = rollup(g)
  )
  expect_length(actual$nested, 2L)
  for (i in seq_len(nrow(actual))) {
    expect_identical(actual$nested[[i]], expected)
    expect_identical(actual$nested[[i]], actual$control[[i]])
  }
})

test_that("nested Grouping bits do not become outer branch constants", {
  data <- tibble::tibble(g = "a")
  actual <- summarize_with_margins(
    data,
    nested = list(summarise_with_margins(
      data,
      bit = marginplyr::grouping_bit(g),
      id = grouping_id(),
      .grouping = rollup(g)
    )),
    .grouping = rollup(g)
  )
  for (nested in actual$nested) {
    expect_identical(nested$bit, c(0L, 1L))
    expect_identical(nested$id, c(0L, 1L))
  }
})

test_that("nested Margin summaries own Parent and Total share validation", {
  outer <- tibble::tibble(g = "a", value = 5)
  inner <- tibble::tibble(h = c("a", "b"), value = c(2, 6))
  independent <- function() {
    summarize_with_margins(
      inner, total = sum(value),
      parent = share_of_parent(total),
      overall = marginplyr::share_of_total(total),
      .grouping = rollup(h)
    )
  }
  expected <- independent()
  expect_identical(expected$parent, c(0.25, 0.75, 1))
  expect_identical(expected$overall, c(0.25, 0.75, 1))

  actual <- summarize_with_margins(
    outer,
    nested = list(summarize_with_margins(
      inner, total = sum(value),
      parent = share_of_parent(total),
      overall = marginplyr::share_of_total(total),
      .grouping = rollup(h)
    )),
    control = list(independent()),
    .grouping = grouping_set(g)
  )
  expect_identical(actual$nested[[1L]], expected)
  expect_identical(actual$nested, actual$control)

  with_outer_share <- summarize_with_margins(
    outer,
    total = sum(value), parent = share_of_total(total),
    nested = list(summarize_with_margins(
      inner, parent = sum(value), overall = share_of_total(parent),
      .grouping = rollup(h)
    )),
    selected = list(dplyr::mutate(
      tibble::tibble(parent = 2), dplyr::across(parent, ~ .x * 2)
    )),
    .grouping = rollup(g)
  )
  for (nested in with_outer_share$nested) {
    expect_identical(nested$parent, c(2, 6, 8))
    expect_identical(nested$overall, c(0.25, 0.75, 1))
  }
  for (selected in with_outer_share$selected) {
    expect_identical(selected, tibble::tibble(parent = 4))
  }
  expect_error(
    summarize_with_margins(
      outer, total = sum(value), parent = share_of_total(total),
      nested = list(dplyr::mutate(tibble::tibble(x = parent), x = x * 2)),
      .grouping = rollup(g)
    ),
    "Ordinary summaries cannot use an earlier Total share",
    class = "marginplyr_error"
  )
})

test_that("operation inputs still evaluate in the enclosing Margin summary", {
  data <- tibble::tibble(g = "a")
  actual <- summarize_with_margins(
    data,
    nested = list((dplyr::mutate)(
      .data = tibble::tibble(g = grouping_bit(g)),
      dplyr::across(dplyr::everything(), ~ .x * 2)
    )),
    partial = list(dplyr::mutate(
      .d = tibble::tibble(g = grouping_bit(g)),
      dplyr::across(dplyr::everything(), ~ .x * 2)
    )),
    computed = list((function(bit = grouping_bit(g)) {
      list(bit = bit, data = dplyr::mutate(
        tibble::tibble(g = 1), dplyr::across(dplyr::everything(), ~ .x * 2)
      ))
    })()),
    captured = list(quote(dplyr::mutate(data, bit = grouping_bit(g)))),
    evaluated = eval(quote(grouping_bit(g))),
    .grouping = rollup(g)
  )
  expect_identical(actual$nested, list(
    tibble::tibble(g = 0), tibble::tibble(g = 2)
  ))
  expect_identical(actual$partial, actual$nested)
  expect_identical(lapply(actual$computed, `[[`, "bit"), list(0L, 1L))
  for (computed in actual$computed) {
    expect_identical(computed$data, tibble::tibble(g = 2))
  }
  expect_identical(actual$captured, rep(list(
    quote(dplyr::mutate(data, bit = grouping_bit(g)))
  ), 2))
  expect_identical(actual$evaluated, c(0L, 1L))
  expect_error(
    summarize_with_margins(
      data, nested = list(dplyr::mutate(dplyr::cur_group())),
      .grouping = rollup(g)
    ),
    "does not support",
    class = "marginplyr_error"
  )
})

test_that("nested lazy summaries do not execute their input", {
  outer <- tibble::tibble(g = "a")
  inner <- dbplyr::lazy_frame(h = "b", value = 2)
  expected <- summarize_with_margins(
    inner, total = sum(value), bit = grouping_bit(h),
    .grouping = rollup(h)
  )
  actual <- summarize_with_margins(
    outer,
    nested = list(summarize_with_margins(
      inner, total = sum(value), bit = grouping_bit(h),
      .grouping = rollup(h)
    )),
    .grouping = rollup(g)
  )
  for (nested in actual$nested) {
    expect_s3_class(nested, "tbl_lazy")
    expect_identical(dbplyr::sql_render(nested), dbplyr::sql_render(expected))
  }
})

test_that("nested pick selections are expanded in their own data mask", {
  inner <- tibble::tibble(g = 1, x = 2)
  independent <- function() {
    dplyr::mutate(inner, y = rowSums(dplyr::pick(dplyr::everything())))
  }
  expected <- tibble::tibble(g = 1, x = 2, y = 3)
  expect_identical(independent(), expected)
  for (key in list(10, "a")) {
    outer <- tibble::tibble(g = key, x = 20)
    actual <- summarize_with_margins(
      outer,
      nested = list(dplyr::mutate(
        inner, y = rowSums(dplyr::pick(dplyr::everything()))
      )),
      picked = list(dplyr::summarise(
        inner, columns = list(dplyr::pick(g, x))
      )),
      control = list(independent()),
      .grouping = rollup(g)
    )
    for (i in seq_len(nrow(actual))) {
      expect_identical(actual$nested[[i]], expected)
      expect_identical(actual$nested[[i]], actual$control[[i]])
      expect_identical(actual$picked[[i]]$columns[[1L]], inner)
    }
  }
})

test_that("nested data-mask wrappers ignore the caller's function binding", {
  input <- data.frame(x = 1:3)
  reads <- 0L
  replacement <- function() data.frame(z = 99L)
  factory <- function(...) replacement
  masked_function <- function(...) {
    reads <<- reads + 1L
    factory
  }
  rlang::env_bind(environment(), `function` = masked_function)

  ordinary <- dplyr::summarize(
    input, answer = sum(dplyr::mutate(input, z = x)$z)
  )
  actual <- summarize_with_margins(
    input, answer = sum(dplyr::mutate(input, z = x)$z)
  )
  expect_identical(ordinary$answer, 6L)
  expect_identical(actual$answer, 6L)
  expect_identical(reads, 0L)
})

test_that("function expressions in a nested verb keep caller lookup", {
  input <- data.frame(x = 1:3)
  increment <- function(value) value + 10L
  rlang::env_bind(environment(), `function` = function(...) increment)

  ordinary <- dplyr::summarize(
    input, answer = sum(dplyr::mutate(input, z = (function(value) value)(x))$z)
  )
  actual <- summarize_with_margins(
    input, answer = sum(dplyr::mutate(input, z = (function(value) value)(x))$z)
  )
  expect_identical(ordinary$answer, 36L)
  expect_identical(actual$answer, ordinary$answer)
})

test_that("nested dplyr selections use their own data mask", {
  outer <- tibble::tibble(g = "a", value = 1)
  inner <- tibble::tibble(g = 1, x = 2)
  independent <- function() {
    dplyr::mutate(inner, dplyr::across(dplyr::everything(), ~ .x * 2))
  }
  expected <- tibble::tibble(g = 2, x = 4)
  expect_identical(independent(), expected)

  actual <- summarize_with_margins(
    outer,
    nested = list(dplyr::mutate(
      inner, dplyr::across(dplyr::everything(), ~ .x * 2)
    )),
    from_outer = list(dplyr::mutate(dplyr::pick(value), value = value * 2)),
    piped = list(dplyr::`%>%`(inner, dplyr::mutate(
      dplyr::across(dplyr::everything(), ~ .x * 2)
    ))),
    selected = list(dplyr::select(inner, dplyr::where(is.numeric))),
    counted = list(dplyr::count(
      inner, wt = rowSums(dplyr::across(dplyr::everything()))
    )),
    grouped = list(dplyr::summarise(
      dplyr::group_by(inner, g), group = dplyr::cur_group_id()
    )),
    control = list(independent()),
    .grouping = rollup(g)
  )
  for (i in seq_len(nrow(actual))) {
    expect_identical(actual$nested[[i]], expected)
    expect_identical(actual$from_outer[[i]], tibble::tibble(value = 2))
    expect_identical(actual$piped[[i]], expected)
    expect_identical(actual$nested[[i]], actual$control[[i]])
    expect_identical(actual$selected[[i]], inner)
    expect_identical(actual$counted[[i]], tibble::tibble(n = 3))
    expect_identical(actual$grouped[[i]]$group, 1L)
  }
  expect_error(
    summarize_with_margins(
      outer, nested = list(dplyr::mutate()), .grouping = rollup(g)
    ),
    "no applicable method"
  )
})

# The direct dplyr calls establish the reference value at each grain.
expect_nested_input_evaluation <- function(input, expr, total, detail) {
  ordinary <- dplyr::summarize(input, answer = !!expr)
  grouped <- dplyr::summarize(input, answer = !!expr, .by = dplyr::all_of("g"))
  expect_identical(ordinary$answer, total)
  expect_identical(grouped$answer, detail)
  expect_identical(
    summarize_with_margins(input, answer = !!expr)$answer,
    ordinary$answer
  )
  expect_identical(
    summarize_with_margins(input, answer = !!expr, .by = dplyr::all_of("g"),
                           .sort = "last")$answer,
    grouped$answer
  )
  expect_identical(
    summarize_with_margins(
      input, answer = !!expr, .grouping = rollup(dplyr::all_of("g")),
      .sort = "last"
    )$answer,
    c(grouped$answer, ordinary$answer)
  )
}

# The local input classes exercise the same expressions at all three grains.
nested_scope_inputs <- function() {
  data <- data.frame(g = c("a", "b"), x = c(1L, 2L))
  inputs <- list(data, tibble::as_tibble(data))
  if (suggest_available("data.table")) {
    inputs <- c(inputs, list(data.table::as.data.table(data)))
  }
  inputs
}

test_that("return from a nested verb input exits the caller function", {
  expr <- rlang::expr((function() {
    dplyr::mutate(return(sum(x)), z = 1L)
    99L
  })())
  for (input in nested_scope_inputs()) {
    expect_nested_input_evaluation(input, expr, 3L, 1:2)
  }
})

test_that("assignment in a nested verb input updates the caller variable", {
  expr <- rlang::expr((function() {
    value <- 99L
    dplyr::mutate(
      {
        value <- sum(x)
        data.frame(z = value)
      },
      z = z + 1L
    )
    value
  })())
  for (input in nested_scope_inputs()) {
    expect_nested_input_evaluation(input, expr, 3L, 1:2)
  }
})

test_that("nested pick uses the inner mask after a caller input assignment", {
  expr <- rlang::expr((function() {
    value <- 99L
    nested <- dplyr::mutate(
      {
        value <- sum(x)
        data.frame(z = value)
      },
      extra = rowSums(dplyr::pick(z))
    )
    value + nested$extra
  })())
  for (input in nested_scope_inputs()) {
    expect_nested_input_evaluation(input, expr, 6, c(2, 4))
  }
})

test_that("explicit input evaluation retains ordinary scope and control flow", {
  direct_return <- rlang::expr((function() {
    return(sum(x))
    99L
  })())
  explicit_input <- rlang::expr((function() {
    value <- 99L
    input <- {
      value <- sum(x)
      data.frame(z = value)
    }
    nested <- dplyr::mutate(input, extra = rowSums(dplyr::pick(z)))
    value + nested$extra
  })())
  for (input in nested_scope_inputs()) {
    expect_nested_input_evaluation(input, direct_return, 3L, 1:2)
    expect_nested_input_evaluation(input, explicit_input, 6, c(2, 4))
  }
})

test_that("Arrow Table refuses an unsupported nested summary before reading", {
  skip_if_suggest_absent("arrow")
  input <- arrow::Table$create(data.frame(x = c(1L, 2L)))
  expr <- rlang::expr(dplyr::group_by(tibble::tibble(z = sum(x)), z))

  ordinary <- observe_arrow_collection({
    expect_warning(
      result <- dplyr::summarize(input, !!expr),
      "Pulling data into R"
    )
    result
  })
  expect_true(ordinary$entries > 0L)
  expect_identical(ordinary$result$z, 3L)

  actual <- observe_arrow_collection(summarize_with_margins(input, !!expr))
  expect_identical(actual$entries, 0L)
  expect_s3_class(actual$result, "marginplyr_error")
  message <- conditionMessage(actual$result)
  expect_match(message, rlang::expr_text(expr), fixed = TRUE)
  expect_match(message, "Collect the Arrow input first", fixed = TRUE)
  expect_match(message, "Select the columns you need before collecting",
               fixed = TRUE)
})

test_that("Arrow Dataset keeps its refusal of an unsupported nested summary", {
  skip_if_suggest_absent("arrow")
  input <- arrow::InMemoryDataset$create(arrow::Table$create(x = c(1L, 2L)))
  expr <- rlang::expr(dplyr::group_by(tibble::tibble(z = sum(x)), z))

  ordinary <- observe_arrow_collection(dplyr::summarize(input, !!expr))
  expect_identical(ordinary$entries, 0L)
  expect_s3_class(ordinary$result, "arrow_not_supported")

  actual <- observe_arrow_collection(summarize_with_margins(input, !!expr))
  expect_identical(actual$entries, 0L)
  expect_identical(class(actual$result), class(ordinary$result))
  expect_identical(conditionMessage(actual$result),
                   conditionMessage(ordinary$result))
})

test_that("supported Arrow summaries stay lazy until collection", {
  skip_if_suggest_absent("arrow")
  table <- arrow::Table$create(x = c(1L, 2L))
  for (input in list(table, arrow::InMemoryDataset$create(table))) {
    actual <- observe_arrow_collection(
      summarize_with_margins(input, answer = sum(x))
    )
    expect_identical(actual$entries, 0L)
    expect_s3_class(actual$result, "arrow_dplyr_query")
    expect_identical(dplyr::collect(actual$result)$answer, 3L)
  }
})
