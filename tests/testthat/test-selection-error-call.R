# A column selection tidyselect refuses keeps tidyselect's class, message and
# cause, and is blamed on a call the caller wrote (#1573). Native dplyr is the
# independent reading of what that call and the rendered context are.

selection_call_names <- function(cnd) {
  vapply(
    condition_chain(cnd),
    function(condition) {
      call <- conditionCall(condition)
      if (!is.call(call)) {
        return(NA_character_)
      }
      rlang::call_name(call) %||% NA_character_
    },
    character(1)
  )
}

expect_caller_selection_error <- function(cnd, verb, native) {
  expect_identical(class(cnd), class(native))
  expect_identical(conditionMessage(cnd), conditionMessage(native))
  expect_identical(rlang::call_name(conditionCall(cnd)), verb)
}

test_that("a refused .by or .grouping selection blames the verb written", {
  data <- tibble::tibble(g = c("a", "b"), value = 1:2)
  native <- expect_error(dplyr::summarise(data, n = dplyr::n(), .by = zz))
  for (verb in names(forwarded_verbs)) {
    by <- expect_error(forwarded_verbs[[verb]](data, zz, NULL))
    expect_caller_selection_error(by, verb, native)
    grouping <- expect_error(forwarded_verbs[[verb]](data, NULL, rollup(zz)))
    expect_caller_selection_error(grouping, verb, native)
  }
})

# The nesting verbs refuse a lazy table before reading `.by` or `.grouping`.
lazy_input_verbs <- setdiff(
  names(forwarded_verbs), c("nest_with_margins", "nest_by_with_margins")
)

test_that("a refused lazy .by or .grouping selection blames the verb", {
  data <- dbplyr::lazy_frame(g = c("a", "b"), value = 1:2)
  native <- expect_error(dplyr::summarise(data, n = dplyr::n(), .by = zz))
  for (verb in lazy_input_verbs) {
    by <- expect_error(forwarded_verbs[[verb]](data, zz, NULL))
    expect_caller_selection_error(by, verb, native)
    grouping <- expect_error(forwarded_verbs[[verb]](data, NULL, rollup(zz)))
    expect_caller_selection_error(grouping, verb, native)
  }
})

test_that("a refused first local summary selection blames the verb", {
  data <- tibble::tibble(g = c("a", "b"), x = 1:2)
  native <- expect_error(dplyr::summarise(data, n = dplyr::n(), .by = zz))
  first <- expect_error(summarize_with_margins(
    data, dplyr::across(zz, sum), .grouping = rollup(g)
  ))
  expect_caller_selection_error(first, "summarize_with_margins", native)
})

test_that("a refused later local summary selection reads as native dplyr's", {
  data <- tibble::tibble(g = c("a", "b"), x = 1:2)
  summaries <- rlang::exprs(
    dplyr::across(zz, sum),
    m = nrow(dplyr::pick(zz)),
    dplyr::if_any(zz, ~ .x > 0),
    dplyr::if_all(zz, ~ .x > 0)
  )
  for (index in seq_along(summaries)) {
    summary <- summaries[index]
    native <- expect_error(dplyr::summarise(
      data, n = dplyr::n(), !!!summary, .by = g
    ))
    attempts <- list(
      expect_error(summarize_with_margins(
        data, n = dplyr::n(), !!!summary, .grouping = rollup(g)
      )),
      expect_error(summarize_with_margins(
        data, n = dplyr::n(), !!!summary, .by = g
      ))
    )
    for (actual in attempts) {
      expect_identical(conditionMessage(actual), conditionMessage(native))
      expect_identical(
        lapply(condition_chain(actual), class),
        lapply(condition_chain(native), class)
      )
      expect_identical(
        selection_call_names(actual),
        c("summarize_with_margins", selection_call_names(native)[-1L])
      )
    }
  }
})

# Two unnamed `across()` calls share a label, so each has its selection bound
# apart from the call (#1572), at either position.
test_that("a selection failing beside another unnamed across reads natively", {
  data <- tibble::tibble(g = c("a", "a", "b"), v = 1:3)
  failing <- rlang::exprs(
    dplyr::across(dplyr::where(function(x) stop("boom")), sum),
    dplyr::across(dplyr::all_of(nope), sum)
  )
  other <- rlang::exprs(dplyr::across(v, sum))
  for (index in seq_along(failing)) {
    for (dots in list(c(other, failing[index]), c(failing[index], other))) {
      native <- expect_error(dplyr::summarise(data, !!!dots, .by = g))
      actual <- expect_error(summarize_with_margins(
        data, !!!dots, .grouping = rollup(g)
      ))
      expect_identical(conditionMessage(actual), conditionMessage(native))
      expect_identical(
        lapply(condition_chain(actual), class),
        lapply(condition_chain(native), class)
      )
      expect_identical(
        selection_call_names(actual),
        c("summarize_with_margins", selection_call_names(native)[-1L])
      )
    }
  }
})

test_that("a refused lazy summary selection names no internal call", {
  data <- dbplyr::lazy_frame(g = c("a", "b"), x = 1:2)
  native <- expect_error(dplyr::summarise(data, n = dplyr::n(), .by = zz))
  summaries <- rlang::exprs(
    dplyr::across(zz, sum),
    n = dplyr::n(),
    dplyr::across(zz, sum),
    m = nrow(dplyr::pick(zz))
  )
  for (dots in list(summaries[1L], summaries[2:3], summaries[4L])) {
    actual <- expect_error(summarize_with_margins(
      data, !!!dots, .grouping = rollup(g)
    ))
    expect_caller_selection_error(actual, "summarize_with_margins", native)
    expect_null(actual$parent)
  }
})

test_that("predicate and nested refusals stay Package conditions", {
  data <- dbplyr::lazy_frame(g = c("a", "b"), value = 1:2)
  for (verb in lazy_input_verbs) {
    predicate <- expect_error(
      forwarded_verbs[[verb]](data, dplyr::where(is.character), NULL)
    )
    expect_s3_class(predicate, "marginplyr_error")
    expect_identical(rlang::call_name(conditionCall(predicate)), verb)
    expect_s3_class(
      predicate$parent, "tidyselect_error_predicates_unsupported"
    )
  }
  spec_from_caller <- function(...) rollup(...)
  nested <- expect_error(summarize_with_margins(
    tibble::tibble(g = c("a", "b"), x = 1:2),
    n = dplyr::n(),
    .grouping = grouping_sets(spec_from_caller(g))
  ))
  expect_s3_class(nested, "marginplyr_error")
  expect_identical(
    rlang::call_name(conditionCall(nested)), "summarize_with_margins"
  )
})
