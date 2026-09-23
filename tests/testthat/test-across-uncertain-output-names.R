test_that("a bound across list does not invent an identifier collision", {
  data <- data.frame(group = c("a", "b"), x = c(1, 2))
  fns <- list(total = sum, avg = mean)

  bound <- summarize_with_margins(
    data,
    dplyr::across(x, fns, .names = "{.col}_{.fn}"),
    .grouping = grouping_set(group),
    .id = "x_1"
  )
  literal <- summarize_with_margins(
    data,
    dplyr::across(x, list(total = sum, avg = mean),
                  .names = "{.col}_{.fn}"),
    .grouping = grouping_set(group),
    .id = "x_1"
  )

  expect_identical(names(bound), c("group", "x_1", "x_total", "x_avg"))
  expect_equal(bound, literal)
  expect_equal(bound$x_total, c(1, 2))
  expect_equal(bound$x_avg, c(1, 2))
  expect_equal(bound$x_1, c(1L, 1L))
})

test_that("default across names stay unknown for a bound function list", {
  data <- data.frame(group = c("a", "b"), x = c(1, 2))
  fns <- list(total = sum, avg = mean)

  bound <- summarize_with_margins(
    data, dplyr::across(x, fns),
    .grouping = grouping_set(group), .id = "x_1"
  )
  literal <- summarize_with_margins(
    data, dplyr::across(x, list(total = sum, avg = mean)),
    .grouping = grouping_set(group), .id = "x_1"
  )

  expect_equal(bound, literal)
  expect_identical(names(bound), c("group", "x_1", "x_total", "x_avg"))
})

test_that("a function list can shadow a familiar function name", {
  data <- data.frame(group = c("a", "b"), x = c(1, 2))
  result <- local({
    sum <- list(total = base::sum, avg = base::mean)
    summarize_with_margins(
      data, dplyr::across(x, sum, .names = "{.col}_{.fn}"),
      .grouping = grouping_set(group), .id = "x_1"
    )
  })

  expect_identical(names(result), c("group", "x_1", "x_total", "x_avg"))
})

test_that("actual bound-list collisions are refused after execution", {
  data <- data.frame(group = c("a", "b"), x = c(1, 2))
  fns <- list(total = sum, avg = mean)
  group_data <- data.frame(x_total = c("a", "b"), x = c(1, 2))

  id_error <- expect_error(summarize_with_margins(
    data, dplyr::across(x, fns, .names = "{.col}_{.fn}"),
    .grouping = grouping_set(group), .id = "x_total"
  ), "`.id`.*`x_total`.*conflicts with a summary output")
  expect_s3_class(id_error, "marginplyr_error")

  group_error <- expect_error(summarize_with_margins(
    group_data, dplyr::across(x, fns, .names = "{.col}_{.fn}"),
    .grouping = grouping_set(x_total)
  ), "cannot overwrite grouping column.*`x_total`")
  expect_s3_class(group_error, "marginplyr_error")

  internal_fns <- list(total = sum)
  internal_fns[["..marginplyr_sort_1"]] <- mean
  internal_error <- expect_error(summarize_with_margins(
    data, dplyr::across(x, internal_fns, .names = "{.fn}"),
    .grouping = rollup(group), .sort = "last"
  ), "summary output names conflict with internal grouping columns")
  expect_match(conditionMessage(internal_error), "..marginplyr_sort_1",
               fixed = TRUE)
  expect_false(grepl("`.id`", conditionMessage(internal_error), fixed = TRUE))
})

test_that("known across shapes still name their actual outputs", {
  data <- data.frame(group = c("a", "b"), x = c(1, 2))

  single <- expect_error(summarize_with_margins(
    data, dplyr::across(x, base::sum, .names = "{.col}_{.fn}"),
    .grouping = grouping_set(group), .id = "x_1"
  ), "`.id`.*`x_1`.*conflicts with a summary output")
  literal <- expect_error(summarize_with_margins(
    data, dplyr::across(x, list(total = sum), .names = "{.col}_{.fn}"),
    .grouping = grouping_set(group), .id = "x_total"
  ), "`.id`.*`x_total`.*conflicts with a summary output")

  expect_s3_class(single, "marginplyr_error")
  expect_s3_class(literal, "marginplyr_error")
})

test_that("prediction does not evaluate a function-list expression", {
  data <- data.frame(group = c("a", "b"), x = c(1, 2))
  calls <- 0L
  make_fns <- function() {
    calls <<- calls + 1L
    list(total = sum, avg = mean)
  }

  result <- summarize_with_margins(
    data,
    dplyr::across(x, make_fns(), .names = "{.col}_{.fn}"),
    .grouping = grouping_set(group), .id = "x_1"
  )
  expect_identical(calls, 1L)
  expect_identical(names(result), c("group", "x_1", "x_total", "x_avg"))
})

test_that("prediction does not force a delayed function-list binding", {
  data <- data.frame(group = c("a", "b"), x = c(1, 2))
  counter <- new.env(parent = emptyenv())
  counter$calls <- 0L
  delayedAssign("fns", {
    counter$calls <- counter$calls + 1L
    list(total = sum, avg = mean)
  })

  result <- summarize_with_margins(
    data, dplyr::across(x, fns, .names = "{.col}_{.fn}"),
    .grouping = grouping_set(group), .id = "x_1"
  )

  expect_identical(counter$calls, 1L)
  expect_identical(names(result), c("group", "x_1", "x_total", "x_avg"))
})

test_that("a bound function list keeps a simulated SQL result lazy", {
  data <- data.frame(group = c("a", "b"), x = c(1, 2))
  remote <- dbplyr::tbl_lazy(data, con = dbplyr::simulate_postgres())
  fns <- list(total = sum, avg = mean)

  result <- summarize_with_margins(
    remote, dplyr::across(x, fns, .names = "{.col}_{.fn}"),
    .grouping = grouping_set(group), .id = "x_1"
  )

  expect_s3_class(result, "tbl_lazy")
  expect_identical(colnames(result), c("group", "x_1", "x_total", "x_avg"))
})

test_that("unknown ordinary outputs are not admitted as share sources", {
  data <- data.frame(group = c("a", "b"), x = c(1, 2))
  fns <- list(total = sum, avg = mean)

  error <- expect_error(summarize_with_margins(
    data,
    dplyr::across(x, fns, .names = "{.col}_{.fn}"),
    share = share_of_parent(x_total),
    .grouping = rollup(group)
  ), "unknown preceding ordinary summary `x_total`")
  expect_s3_class(error, "marginplyr_error")
})

test_that("a delayed single function keeps its share source name", {
  data <- data.frame(group = c("a", "b"), x = c(1, 2))
  summarize <- function(delayed) {
    if (delayed) {
      delayedAssign("fn", function(value) sum(value))
    } else {
      fn <- function(value) sum(value)
    }
    summarize_with_margins(
      data,
      dplyr::across(x, fn, .names = "{.col}_{.fn}"),
      share = share_of_parent(x_1),
      .grouping = rollup(group),
      .margin_label = NULL
    )
  }

  delayed <- summarize(TRUE)
  eager <- summarize(FALSE)
  expect_equal(delayed, eager)
  expect_identical(names(delayed), c("group", "x_1", "share"))
  expect_equal(delayed$share, c(1 / 3, 2 / 3, 1))
})

test_that("a globally bound single function keeps its share source name", {
  run_global_call <- function() {
    assign("marginplyr_test_fn_607", sum, envir = globalenv())
    on.exit(rm("marginplyr_test_fn_607", envir = globalenv()))
    eval(quote(summarize_with_margins(
      data.frame(group = c("a", "b"), x = c(1, 2)),
      dplyr::across(x, marginplyr_test_fn_607, .names = "{.col}_{.fn}"),
      share = share_of_parent(x_1),
      .grouping = rollup(group),
      .margin_label = NULL
    )), envir = globalenv())
  }

  result <- run_global_call()
  expect_identical(names(result), c("group", "x_1", "share"))
  expect_equal(result$share, c(1 / 3, 2 / 3, 1))
})
