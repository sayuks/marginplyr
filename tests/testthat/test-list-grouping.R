list_grouping_dimensions <- function() {
  list(
    bare = list(2:3, "2:3", NULL, "NULL"),
    list_of = vctrs::list_of(1L, 2L, NULL, 3L)
  )
}

list_margin_operations <- function(data, label, check = TRUE) {
  grouping <- rollup(dplyr::all_of(c("other", "l")))
  list(
    summary = function() {
      summarize_with_margins(
        data, total = sum(.data$value), .grouping = grouping,
        .margin_label = label, .check_margin_label = check, .id = "set"
      )
    },
    expansion = function() {
      expand_with_margins(
        data, .grouping = grouping,
        .margin_label = label, .check_margin_label = check, .id = "set"
      )
    },
    nest = function() {
      nest_with_margins(
        data, .grouping = grouping,
        .margin_label = label, .check_margin_label = check, .id = "set"
      )
    },
    nest_by = function() {
      nest_by_with_margins(
        data, .grouping = grouping,
        .margin_label = label, .check_margin_label = check, .id = "set"
      )
    }
  )
}

expect_list_label_refusal <- function(operation, info) {
  error <- expect_error(operation(), class = "marginplyr_error", info = info)
  message <- conditionMessage(error)
  expect_match(message, "list-column grouping dimensions", info = info)
  expect_match(message, "`l`", fixed = TRUE, info = info)
  expect_no_match(message, "`other`", fixed = TRUE, info = info)
  expect_match(message, "`.margin_label`", fixed = TRUE, info = info)
  expect_match(message, "NULL", fixed = TRUE, info = info)
  expect_match(message, "NA_character_", fixed = TRUE, info = info)
  expect_match(message, "separate columns", info = info)
}

test_that("a non-missing list-column label is refused by every Margin verb", {
  for (kind in names(list_grouping_dimensions())) {
    values <- list_grouping_dimensions()[[kind]]
    data <- tibble::tibble(
      other = c("a", "a", "b", "b"), l = values, value = 1:4
    )
    labels <- list(
      "Total", c(other = "All", l = "All"), list(other = NULL, l = "All")
    )
    for (label in labels) {
      for (check in c(TRUE, FALSE)) {
        operations <- list_margin_operations(data, label, check)
        for (verb in names(operations)) {
          expect_list_label_refusal(
            operations[[verb]], paste(kind, verb, check, deparse1(label))
          )
        }
      }
    }
  }
})

test_that("a list-column dimension accepts typed-missing labels", {
  for (kind in names(list_grouping_dimensions())) {
    values <- list_grouping_dimensions()[[kind]]
    data <- tibble::tibble(
      other = c("a", "a", "b", "b"), l = values, value = 1:4
    )
    missing <- list_margin_operations(data, NULL)
    opt_out <- list_margin_operations(data, list(other = "All", l = NULL))
    typed_na <- list_margin_operations(data, c(other = "All",
                                               l = NA_character_))
    for (verb in names(missing)) {
      info <- paste(kind, verb)
      reference <- missing[[verb]]()
      expect_identical(reference$l, opt_out[[verb]]()$l, info = info)
      expect_identical(reference$l, typed_na[[verb]]()$l, info = info)
      expect_identical(class(reference$l), class(values), info = info)
      expect_identical(
        vctrs::vec_slice(reference$l, reference$set == 1L), values,
        info = info
      )
      expect_true(all(vctrs::vec_detect_missing(
        vctrs::vec_slice(reference$l, reference$set > 1L)
      )), info = info)
    }
  }
})

test_that("dtplyr refuses a non-missing list-column label lazily", {
  skip_if_suggest_absent("dtplyr")
  for (kind in names(list_grouping_dimensions())) {
    data <- tibble::tibble(
      other = c("a", "a", "b", "b"), l = list_grouping_dimensions()[[kind]],
      value = 1:4
    )
    operations <- list_margin_operations(dtplyr::lazy_dt(data), "Total")
    for (verb in names(operations)) {
      expect_list_label_refusal(operations[[verb]], paste(kind, verb))
    }
  }
})

test_that("Arrow refuses a non-missing label on list-typed dimensions", {
  skip_if_suggest_absent("arrow")
  types <- list(
    list = arrow::list_of(arrow::int32()),
    large_list = arrow::large_list_of(arrow::int32()),
    fixed_size_list = arrow::fixed_size_list_of(arrow::int32(), 2L)
  )
  for (type in names(types)) {
    source <- arrow::Table$create(
      other = c("a", "b"),
      l = arrow::Array$create(list(1:2, 3:4), type = types[[type]]),
      value = 1:2
    )
    operations <- list_margin_operations(source, "Total")
    for (verb in c("summary", "expansion")) {
      expect_list_label_refusal(operations[[verb]], paste(type, verb))
    }
  }
})

test_that("an Arrow binary dimension still takes a string label", {
  skip_if_suggest_absent("arrow")
  source <- arrow::Table$create(
    l = arrow::Array$create(list(as.raw(1L), as.raw(2L)),
                            type = arrow::binary()),
    value = 1:2
  )
  result <- dplyr::collect(summarize_with_margins(
    source, total = sum(value), .grouping = rollup(l), .sort = "last"
  ))
  expect_identical(result$l[[3L]], "Total")
  expect_identical(result$total, c(1L, 2L, 3L))
})
