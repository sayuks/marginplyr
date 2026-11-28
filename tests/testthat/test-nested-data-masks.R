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

# A Margin summary of `expr`, a quosure, at `rollup(g)` and a plain summary at
# each grain both return the value the nested call gives evaluated alone.
expect_nested_verb_value <- function(input, expr, alone) {
  outer <- dplyr::collect(summarize_with_margins(
    input, n = !!expr, .grouping = rollup(dplyr::all_of("g")), .sort = "last"
  ))
  native <- dplyr::summarise(
    dplyr::collect(input), n = !!expr, .by = dplyr::all_of("g")
  )
  expect_identical(native$n, rep(alone, nrow(native)))
  expect_identical(outer$n, rep(alone, nrow(outer)))
}

nested_verb_frame <- function() {
  tibble::tibble(g = c("x", "y", "z"), aa = 1:3)
}

test_that("filter_out() and nest_by() own their selections (#1819)", {
  input <- tibble::tibble(g = c("a", "a", "b"), v = c(1, 2, 4))
  nested_g <- nested_verb_frame()
  expect_nested_verb_value(input, rlang::quo(nrow(dplyr::filter_out(
    nested_g, dplyr::if_any(dplyr::everything(), ~ .x == "x")
  ))), 2L)
  expect_nested_verb_value(input, rlang::quo(ncol(dplyr::nest_by(
    nested_g, dplyr::across(dplyr::everything())
  ))), 3L)
})

# dplyr expands `pick()` syntactically in any call argument, so a plain summary
# reads this nested-only column from the outer data and fails; the registered
# `filter()` already returns its own mask's value, the control here.
test_that("pick() inside filter_out() and nest_by() reads its own frame", {
  input <- tibble::tibble(g = c("a", "a", "b"), v = c(1, 2, 4))
  nested_g <- nested_verb_frame()
  picked <- function(expr) {
    summarize_with_margins(input, n = !!expr, .grouping = rollup(g))$n
  }
  expect_identical(
    picked(quote(nrow(dplyr::filter(nested_g, dplyr::pick(aa)[[1L]] > 1L)))),
    rep(2L, 3L)
  )
  expect_identical(
    picked(quote(nrow(dplyr::filter_out(
      nested_g, dplyr::pick(aa)[[1L]] > 1L
    )))),
    rep(1L, 3L)
  )
  expect_identical(
    picked(quote(nrow(dplyr::nest_by(nested_g, dplyr::pick(aa)[[1L]] > 1L)))),
    rep(2L, 3L)
  )
})

test_that("filter_out() and nest_by() own them on a dtplyr input (#1819)", {
  skip_if_suggest_absent("dtplyr")
  input <- dtplyr::lazy_dt(data.frame(g = c("a", "a", "b"), v = c(1, 2, 4)))
  nested_g <- nested_verb_frame()
  expect_nested_verb_value(input, rlang::quo(nrow(dplyr::filter_out(
    nested_g, dplyr::if_any(dplyr::everything(), ~ .x == "x")
  ))), 2L)
  expect_nested_verb_value(input, rlang::quo(ncol(dplyr::nest_by(
    nested_g, dplyr::across(dplyr::everything())
  ))), 3L)
})

test_that("Grouping helpers inside filter_out() are refused as in filter()", {
  input <- tibble::tibble(g = c("a", "a", "b"), v = c(1, 2, 4))
  nested_g <- nested_verb_frame()
  for (helper in list(quote(grouping_bit(g)), quote(grouping_id(g)))) {
    for (verb in c("filter", "filter_out")) {
      expr <- rlang::call2(
        "nrow",
        rlang::call2(rlang::call2("::", quote(dplyr), rlang::sym(verb)),
                     quote(nested_g), rlang::call2("==", helper, 0L))
      )
      expect_error(
        summarize_with_margins(input, n = !!expr, .grouping = rollup(g)),
        "can only be used inside `summarize_with_margins()`",
        fixed = TRUE
      )
    }
  }
})

test_that("superseded and tidyr verbs own their selections (#1819)", {
  input <- tibble::tibble(g = c("a", "a", "b"), v = c(1, 2, 4))
  nested_g <- nested_verb_frame()
  expect_nested_verb_value(input, rlang::quo(nrow(dplyr::top_n(
    nested_g, 1, dplyr::if_any(dplyr::everything(), ~ .x == "x")
  ))), 1L)
  expect_nested_verb_value(input, rlang::quo(nrow(dplyr::top_frac(
    nested_g, 0.34, dplyr::if_any(dplyr::everything(), ~ .x == "x")
  ))), 1L)
  expect_nested_verb_value(input, rlang::quo(ncol(dplyr::group_nest(
    nested_g, dplyr::across(dplyr::everything())
  ))), 3L)
  expect_nested_verb_value(input, rlang::quo(nrow(tidyr::uncount(
    nested_g,
    as.integer(dplyr::if_any(dplyr::everything(), ~ .x == "x"))
  ))), 1L)
  expect_nested_verb_value(input, rlang::quo(dplyr::sample_n(
    nested_g, 1,
    weight = as.numeric(dplyr::if_any(dplyr::everything(), ~ .x == "x"))
  )$aa), 1L)
  expect_nested_verb_value(input, rlang::quo(dplyr::sample_frac(
    nested_g, 0.34,
    weight = as.numeric(dplyr::if_any(dplyr::everything(), ~ .x == "x"))
  )$aa), 1L)
})

# Each verb opens a mask of its own, so its `cur_group_id()` describes that
# mask's one group rather than the Margin summary's branches.
test_that("cur_group_id() inside a verb's own mask is dplyr's (#1819)", {
  input <- tibble::tibble(g = c("a", "a", "b"), v = c(1, 2, 4))
  nested_g <- nested_verb_frame()
  expect_nested_verb_value(input, rlang::quo(nrow(dplyr::top_n(
    nested_g, 1, -dplyr::cur_group_id()
  ))), 3L)
  expect_nested_verb_value(input, rlang::quo(nrow(tidyr::uncount(
    nested_g, dplyr::cur_group_id() * 2L
  ))), 6L)
  expect_nested_verb_value(input, rlang::quo(ncol(dplyr::group_nest(
    nested_g, k = dplyr::cur_group_id()
  ))), 2L)
  expect_nested_verb_value(input, rlang::quo(dplyr::mutate_at(
    nested_g, "aa", ~ . + dplyr::cur_group_id()
  )$aa[[1L]]), 2L)
  expect_nested_verb_value(input, rlang::quo(dplyr::summarise_all(
    nested_g, ~ dplyr::cur_group_id()
  )$aa), 1L)
  expect_nested_verb_value(input, rlang::quo(nrow(dplyr::filter_at(
    nested_g, "aa", dplyr::all_vars(. > dplyr::cur_group_id())
  ))), 2L)
})

test_that("a share source may read a nested frame's own column (#1819)", {
  input <- tibble::tibble(g = c("a", "a", "b"), v = c(1, 2, 4))
  nested_t <- tibble::tibble(total = c(1, 5, 3))
  for (source in list(
    rlang::quo(sum(dplyr::filter(nested_t, total > 2)$total)),
    rlang::quo(sum(dplyr::top_n(nested_t, 2, total)$total))
  )) {
    actual <- summarize_with_margins(
      input, total = sum(v), x = !!source, p = share_of_total(x),
      .grouping = rollup(g)
    )
    expect_identical(actual$x, c(8, 8, 8))
    expect_identical(actual$p, c(1, 1, 1))
  }
})

# `with_groups()` takes the verb as a value, so which mask owns its arguments
# depends on that value (#1652); its lambda form is the supported spelling.
test_that("with_groups() keeps ordinary traversal (#1819)", {
  input <- tibble::tibble(g = c("a", "a", "b"), v = c(1, 2, 4))
  nested_g <- nested_verb_frame()
  expect_null(independent_mask_input(quote(dplyr::with_groups(
    nested_g, g, dplyr::mutate, k = 1
  ))))
  expect_nested_verb_value(input, rlang::quo(dplyr::with_groups(
    nested_g, g, function(data) dplyr::mutate(data, k = dplyr::cur_group_id())
  )$k[[3L]]), 3L)
})

# The written set of verbs that evaluate caller arguments against their own
# input at the required dplyr version; see `dplyr_mask` in
# `static_spelling_rules()` for what is left out and why.
test_that("the nested-verb registry names every data-mask verb (#1819)", {
  scoped <- c(
    "mutate", "transmute", "summarise", "summarize", "filter", "arrange",
    "group_by", "distinct", "select", "rename"
  )
  dplyr_verbs <- c(
    "add_count", "add_tally", "arrange", "count", "distinct", "do", "filter",
    "filter_out", "group_by", "group_nest", "group_split", "mutate",
    "nest_by", "pull", "reframe", "relocate", "rename", "rename_with",
    "rowwise", "sample_frac", "sample_n", "select", "slice", "slice_head",
    "slice_max", "slice_min", "slice_sample", "slice_tail", "summarise",
    "summarize", "tally", "top_frac", "top_n", "transmute", "ungroup",
    paste0(rep(scoped, each = 3L), c("_all", "_at", "_if"))
  )
  expect_setequal(static_spelling_names("dplyr_mask"), dplyr_verbs)
  expect_setequal(static_spelling_names("tidyr_mask"), "uncount")
  expect_true(all(dplyr_verbs %in% getNamespaceExports("dplyr")))
  expect_true("uncount" %in% getNamespaceExports("tidyr"))
  inputs <- c(
    static_spelling_rule("dplyr_mask")$inputs,
    static_spelling_rule("tidyr_mask")$inputs
  )
  owners <- c(
    rlang::set_names(rep("dplyr", length(dplyr_verbs)), dplyr_verbs),
    uncount = "tidyr"
  )
  for (verb in names(inputs)) {
    formals <- names(formals(getExportedValue(owners[[verb]], verb)))
    expect_identical(formals[[1L]], inputs[[verb]], label = verb)
  }
})

# The written set above is this package's reading. dplyr's help pages mark an
# argument evaluated against the verb's own input as `<data-masking>` or
# `<tidy-select>`, so a verb the installed dplyr adds shows there first.
test_that("each export dplyr documents as masking is registered (#1819)", {
  section <- function(rd, tag) {
    parts <- Filter(function(part) identical(attr(part, "Rd_tag"), tag), rd)
    vapply(parts, function(part) paste(unlist(part), collapse = ""),
           character(1))
  }
  exports <- getNamespaceExports("dplyr")
  documented <- unlist(lapply(tools::Rd_db("dplyr"), function(rd) {
    arguments <- paste(section(rd, "\\arguments"), collapse = "")
    if (!grepl("<data-masking>|<tidy-select>", arguments)) return(NULL)
    usage <- paste(section(rd, "\\usage"), collapse = "")
    Filter(function(alias) {
      grepl(paste0("(^|[^A-Za-z0-9_.])", alias, "[(]"), usage)
    }, intersect(section(rd, "\\alias"), exports))
  }), use.names = FALSE)
  # The Contextual selections and the scoped verbs' helpers capture an
  # expression for a verb and own no input; `with_groups()` takes its verb as
  # a value (#1652).
  helpers <- c(
    static_spelling_names("selection"),
    "all_vars", "any_vars", "funs", "vars", "with_groups"
  )
  expect_true(all(helpers %in% documented))
  expect_setequal(
    setdiff(documented, helpers),
    intersect(static_spelling_names("dplyr_mask"), documented)
  )
})

# The eager pipe's infix spelling is what the registry is asked to recognise.
# nolint start: pipe_consistency_linter.
test_that("a verb on the right of %!>% owns its arguments (#1827)", {
  `%!>%` <- magrittr::`%!>%`
  input <- tibble::tibble(g = c("a", "a", "b"), v = c(1, 2, 4))
  nested_g <- nested_verb_frame()
  expect_nested_verb_value(input, rlang::quo(nrow(nested_g %!>% dplyr::filter(
    !dplyr::if_any(dplyr::everything(), ~ .x == "x")
  ))), 2L)
  expect_nested_verb_value(input, rlang::quo(ncol(magrittr::`%!>%`(
    nested_g,
    dplyr::mutate(dplyr::across(
      dplyr::everything(), ~ .x, .names = "{.col}_c"
    ))
  ))), 4L)
  expect_nested_verb_value(input, rlang::quo(nrow(
    nested_g %!>% summarize_with_margins(k = dplyr::n(), .grouping = NULL)
  )), 1L)
  for (pipe in c("%>%", "%!>%")) {
    expr <- rlang::call2(
      "nrow",
      rlang::call2(
        pipe, quote(nested_g), quote(dplyr::filter(grouping_bit(g) == 0L))
      )
    )
    expect_error(
      summarize_with_margins(input, n = !!expr, .grouping = rollup(g)),
      "can only be used inside `summarize_with_margins()`",
      fixed = TRUE
    )
  }
})
# nolint end

# `%$%` exposes its left side as an environment rather than inserting it as
# the first argument, so it supplies no input and is not a data pipe. Its
# infix spelling is what the registry is asked to leave unrecognised.
# nolint start: pipe_consistency_linter.
test_that("%$% keeps ordinary traversal (#1827)", {
  `%$%` <- magrittr::`%$%`
  input <- tibble::tibble(g = c("a", "a", "b"), v = c(1, 2, 4))
  nested_g <- nested_verb_frame()
  expect_nested_verb_value(input, rlang::quo(nested_g %$% sum(aa)), 6L)
  expect_identical(
    independent_mask_call_parts(quote(nested_g %$% dplyr::filter(aa > 1L))),
    c(FALSE, FALSE)
  )
})
# nolint end

# magrittr fixes the tee pipe's name, and its infix spelling is what the
# registry is asked to recognise.
# nolint start: object_name_linter, pipe_consistency_linter.
test_that("a verb on the right of %T>% owns its arguments (#1826)", {
  `%T>%` <- magrittr::`%T>%`
  input <- tibble::tibble(g = c("a", "a", "b"), v = c(1, 2, 4))
  nested_g <- nested_verb_frame()
  expect_nested_verb_value(input, rlang::quo(nrow(
    nested_g %T>% summarize_with_margins(k = dplyr::n(), .grouping = NULL)
  )), 3L)
  expect_error(
    summarize_with_margins(
      input,
      n = nrow(nested_g %T>% dplyr::filter(grouping_bit(g) == 0L)),
      .grouping = rollup(g)
    ),
    "can only be used inside `summarize_with_margins()`",
    fixed = TRUE
  )
})
# nolint end
