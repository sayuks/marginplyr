matrix_grouping_data <- function() {
  tibble::tibble(
    g = matrix(1:4, nrow = 2L),
    value = 1:2
  )
}

matrix_margin_operations <- function(data, label, check = TRUE, sort = "last") {
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
    "matrix grouping dimensions"
  )
  for (check in c(TRUE, FALSE)) {
    for (operation in matrix_margin_operations(data, "Total", check)) {
      error <- expect_error(operation())
      expect_s3_class(error, "marginplyr_error")
      expect_match(conditionMessage(error), "matrix grouping dimension")
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
      results <- lapply(matrix_margin_operations(data, label, sort = sort),
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
