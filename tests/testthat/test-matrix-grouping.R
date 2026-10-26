matrix_grouping_data <- function() {
  tibble::tibble(
    g = matrix(1:4, nrow = 2L),
    value = 1:2
  )
}

dimensional_margin_operations <- function(data, label,
                                          check = TRUE, sort = "last") {
  list(
    summary = function() {
      summarize_with_margins(
        data, total = sum(.data$value),
        .grouping = rollup(dplyr::all_of("g")),
        .margin_label = label, .check_margin_label = check,
        .id = "set", .sort = sort
      )
    },
    expansion = function() {
      expand_with_margins(
        data, .grouping = rollup(dplyr::all_of("g")),
        .margin_label = label, .check_margin_label = check,
        .id = "set", .sort = sort
      )
    },
    nest = function() {
      nest_with_margins(
        data, .grouping = rollup(dplyr::all_of("g")),
        .margin_label = label, .check_margin_label = check,
        .id = "set", .sort = sort
      )
    },
    nest_by = function() {
      nest_by_with_margins(
        data, .grouping = rollup(dplyr::all_of("g")),
        .margin_label = label, .check_margin_label = check,
        .id = "set", .sort = sort
      )
    }
  )
}

test_that("a non-missing matrix label is refused by every Margin verb", {
  data <- matrix_grouping_data()
  expect_error(
    summarize_with_margins(data, total = sum(value),
                           .grouping = rollup(g)),
    "matrix or array grouping dimensions"
  )
  for (check in c(TRUE, FALSE)) {
    for (operation in dimensional_margin_operations(data, "Total", check)) {
      error <- expect_error(operation())
      expect_s3_class(error, "marginplyr_error")
      expect_match(conditionMessage(error),
                   "matrix or array grouping dimension")
      expect_match(conditionMessage(error), "`g`", fixed = TRUE)
      expect_match(conditionMessage(error), "`.margin_label`", fixed = TRUE)
      expect_match(conditionMessage(error), "non-missing scalar label")
      expect_match(conditionMessage(error), "NULL", fixed = TRUE)
      expect_match(conditionMessage(error), "NA_character_", fixed = TRUE)
      expect_match(conditionMessage(error), "separate columns")
    }
  }
})

test_that("typed-missing matrix labels retain shape, type, and Margin order", {
  data <- matrix_grouping_data()
  missing_g <- vctrs::vec_init(data$g, 1L)
  for (label in list(NULL, NA_character_)) {
    for (sort in c("last", "first")) {
      results <- lapply(dimensional_margin_operations(data, label, sort = sort),
                        function(operation) operation())
      margin_first <- identical(sort, "first")
      for (verb in names(results)) {
        result <- results[[verb]]
        expect_identical(typeof(result$g), "integer", info = verb)
        expect_identical(ncol(result$g), 2L, info = verb)
        expect_identical(result$set[1L],
                         if (margin_first) 2L else 1L, info = verb)
        expect_identical(result$g[result$set == 1L, , drop = FALSE],
                         data$g, info = verb)
        margin_rows <- result$g[result$set == 2L, , drop = FALSE]
        expect_identical(margin_rows,
                         vctrs::vec_rep(missing_g, nrow(margin_rows)),
                         info = verb)
      }
      expect_identical(nrow(results$summary), 3L)
      expect_identical(results$summary$total[results$summary$set == 2L], 3L)
      expect_identical(nrow(results$expansion), 4L)
      expect_identical(results$expansion$value[results$expansion$set == 2L],
                       data$value)
      for (verb in c("nest", "nest_by")) {
        result <- results[[verb]]
        expect_identical(nrow(result), 3L)
        expect_identical(result$data[[which(result$set == 2L)]]$value,
                         data$value)
      }
    }
  }
})

test_that("matrix dimensions can be missing-labelled beside display labels", {
  data <- matrix_grouping_data()
  data$region <- c("East", "West")
  for (missing_label in list(NULL, NA_character_)) {
    label <- list(region = "All regions", g = missing_label)
    results <- list(
      summarize_with_margins(data, total = sum(value),
                             .grouping = rollup(region, g),
                             .margin_label = label, .id = "set"),
      expand_with_margins(data, .grouping = rollup(region, g),
                          .margin_label = label, .id = "set"),
      nest_with_margins(data, .grouping = rollup(region, g),
                        .margin_label = label, .id = "set"),
      nest_by_with_margins(data, .grouping = rollup(region, g),
                           .margin_label = label, .id = "set")
    )
    for (result in results) {
      expect_true(any(result$region == "All regions"))
      expect_identical(result$g[result$set == 3L, , drop = FALSE],
                       vctrs::vec_init(data$g, sum(result$set == 3L)))
    }
  }
})

test_that("a matrix used only as a fixed key needs no Margin label", {
  data <- matrix_grouping_data()
  data$region <- c("East", "West")
  results <- list(
    summarize_with_margins(data, total = sum(value),
                           .by = g, .grouping = rollup(region)),
    expand_with_margins(data, .by = g, .grouping = rollup(region)),
    nest_with_margins(data, .by = g, .grouping = rollup(region)),
    nest_by_with_margins(data, .by = g, .grouping = rollup(region))
  )
  for (result in results) {
    expect_identical(result$g, vctrs::vec_rep(data$g, 2L))
    expect_true(any(result$region == "Total"))
  }
})

test_that("a scalar array label is refused from populated and empty schemas", {
  data <- tibble::tibble(g = array(1:8, dim = c(2L, 2L, 2L)), value = 1:2)
  for (rows in list(1:2, integer())) {
    input <- vctrs::vec_slice(data, rows)
    error <- expect_error(expand_with_margins(input, .grouping = rollup(g)))
    expect_s3_class(error, "marginplyr_error")
    for (check in c(TRUE, FALSE)) {
      for (operation in dimensional_margin_operations(input, "All", check)) {
        error <- expect_error(operation())
        expect_s3_class(error, "marginplyr_error")
        expect_match(conditionMessage(error), "array grouping dimensions")
        expect_match(conditionMessage(error), "`g`", fixed = TRUE)
        expect_match(conditionMessage(error), "NULL", fixed = TRUE)
        expect_match(conditionMessage(error), "NA_character_", fixed = TRUE)
        expect_match(conditionMessage(error), "separate columns", fixed = TRUE)
      }
    }
  }
})

test_that("typed-missing array labels retain shape and source rows", {
  data <- tibble::tibble(g = array(1:8, dim = c(2L, 2L, 2L)), value = 1:2)
  for (label in list(NULL, NA_character_)) {
    results <- lapply(dimensional_margin_operations(data, label),
                      function(operation) operation())
    for (verb in names(results)) {
      result <- results[[verb]]
      rows <- if (verb == "expansion") 4L else 3L
      expect_identical(nrow(result), rows, info = verb)
      expect_identical(dim(result$g), c(rows, 2L, 2L), info = verb)
      expect_identical(result$g[result$set == 1L, , , drop = FALSE],
                       data$g, info = verb)
      margin_rows <- if (verb == "expansion") 2L else 1L
      expect_identical(result$g[result$set == 2L, , , drop = FALSE],
                       array(NA_integer_, dim = c(margin_rows, 2L, 2L)),
                       info = verb)
    }
    expect_identical(results$expansion$value, c(1L, 2L, 1L, 2L))
    expect_identical(results$summary$total, c(1L, 2L, 3L))
    expect_identical(results$nest$data[[3L]]$value, 1:2)
    expect_identical(results$nest_by$data[[3L]]$value, 1:2)
  }
})

test_that("an array used only as a fixed key needs no Margin label", {
  data <- tibble::tibble(
    g = array(1:8, dim = c(2L, 2L, 2L)), region = c("East", "West"),
    value = 1:2
  )
  results <- list(
    summarize_with_margins(data, total = sum(value),
                           .by = g, .grouping = rollup(region)),
    expand_with_margins(data, .by = g, .grouping = rollup(region)),
    nest_with_margins(data, .by = g, .grouping = rollup(region)),
    nest_by_with_margins(data, .by = g, .grouping = rollup(region))
  )
  for (result in results) {
    expect_identical(result$g, vctrs::vec_rep(data$g, 2L))
    expect_true(any(result$region == "Total"))
  }
})

test_that("caller errors with array keys remain External conditions", {
  data <- tibble::tibble(g = array(1:8, dim = c(2L, 2L, 2L)), value = 1:2)
  cause <- simpleError("g must be size 2 or 1, not 8")
  control <- expect_error(dplyr::summarize(
    dplyr::group_by(data, g), total = stop(cause)
  ))
  for (label in list(NULL, NA_character_)) {
    error <- expect_error(summarize_with_margins(
      data, total = stop(cause), .grouping = rollup(g), .margin_label = label
    ))
    expect_false(inherits(error, "marginplyr_error"))
    expect_identical(class(error), class(control))
    expect_identical(error$parent, cause)
    expect_identical(error$parent, control$parent)
  }
})

test_that("one-dimensional arrays still accept scalar display labels", {
  data <- tibble::tibble(g = array(1:2, dim = 2L), value = 1:2)
  result <- expand_with_margins(
    data, .grouping = rollup(g), .margin_label = "All", .sort = "none"
  )
  expect_identical(result$g, c("1", "2", "All", "All"))
})
