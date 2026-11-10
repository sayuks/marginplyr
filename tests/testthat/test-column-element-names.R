test_that("included dimensions follow native expansion element names", {
  input <- tibble::tibble(k = c("b", "a", "b"), g = c("y", "x", "y"),
                          z = c(2, 1, 3))
  names(input$k) <- c("fixed", "", NA_character_)
  names(input$g) <- c("same", "same", "")
  names(input$z) <- c("z3", "z1", "z2")
  expect_identical(names(input$g), c("same", "same", ""))
  control <- dplyr::union_all(input, dplyr::mutate(input, g = "Total"))
  result <- expand_with_margins(input, .by = k, .grouping = rollup(g))
  for (column in c("k", "g", "z")) {
    expect_identical(result[[column]], control[[column]], info = column)
  }
})

test_that("factor display reconstruction follows native branch element names", {
  input <- tibble::tibble(g = ordered(c("b", "a", "b")), z = 1:3)
  names(input$g) <- c("same", "", NA_character_)
  expect_identical(names(input$g), c("same", "", NA_character_))
  control <- dplyr::union_all(input, dplyr::mutate(input, g = "Total"))
  result <- expand_with_margins(input, .grouping = rollup(g))
  expect_identical(names(result$g), names(control$g))
  expect_identical(unname(as.character(result$g)), unname(control$g))
  expect_true(is.ordered(result$g))
  expect_identical(levels(result$g), c("a", "b", "Total"))
})

test_that("nesting follows native outer and inner column element names", {
  input <- tibble::tibble(k = c("a", "a", "b"), g = c("x", "x", "y"),
                          z = c(2, 1, 3))
  for (column in names(input)) {
    names(input[[column]]) <- c("same", "", NA_character_)
  }
  expect_identical(unname(lapply(input, names)),
                   rep(list(c("same", "", NA_character_)), 3L))
  for (rowwise in c(FALSE, TRUE)) {
    for (keep in c(FALSE, TRUE)) {
      branches <- lapply(c(TRUE, FALSE), function(included) {
        keys <- c("k", "g"[included])
        branch <- if (rowwise) {
          dplyr::ungroup(dplyr::nest_by(
            dplyr::select(input, dplyr::all_of(
              if (keep) names(input) else c(keys, "z")
            )),
            !!!rlang::syms(keys), .keep = keep
          ))
        } else {
          payload <- if (keep) names(input) else "z"
          tidyr::nest(input, data = dplyr::all_of(payload),
                      .by = dplyr::all_of(keys))
        }
        if (!included) branch <- dplyr::mutate(branch, g = "Total")
        branch
      })
      control <- dplyr::union_all(branches[[1L]], branches[[2L]])
      verb <- if (rowwise) nest_by_with_margins else nest_with_margins
      result <- verb(input, .by = k, .grouping = rollup(g), .keep = keep)
      for (column in c("k", "g")) {
        expect_identical(names(result[[column]]), names(control[[column]]),
                         info = paste(rowwise, keep, column))
      }
      expect_identical(lapply(result$data, function(cell) lapply(cell, names)),
                       lapply(control$data, function(cell) lapply(cell, names)),
                       info = paste(rowwise, keep))
    }
  }
})

test_that("local element-name representations follow native compositions", {
  for (convert in list(identity, tibble::as_tibble)) {
    for (element_names in list(c("same", "same", "last"), c("", "", ""),
                               c(NA_character_, "", NA_character_), NULL)) {
      input <- element_name_input(convert, element_names = element_names)
      for (verb in c("summary", "alias", "expand", "nest", "rowwise")) {
        for (included in list(TRUE, c(TRUE, FALSE))) {
          expect_element_name_input(input, element_names)
          control <- element_name_reference(
            input, included,
            if (verb == "alias") "summary" else verb
          )
          result <- element_name_result(input, included, verb)
          expect_element_names(result, control, paste(verb, included))
          expect_element_names(dplyr::collect(result), dplyr::collect(control),
                               paste(verb, included, "collect"))
        }
      }
    }
  }
})

test_that("empty names and omitted replacements follow native mutation", {
  for (convert in list(identity, tibble::as_tibble)) {
    cases <- list(character(), NULL, c("same", "", NA_character_))
    for (element_names in cases) {
      size <- length(element_names)
      input <- element_name_input(convert, size, element_names)
      for (label in list(NULL, NA_character_)) {
        for (verb in c("summary", "alias", "expand", "nest", "rowwise")) {
          for (included in list(TRUE, c(TRUE, FALSE))) {
            expect_element_name_input(input, element_names)
            control <- element_name_reference(
              input, included, if (verb == "alias") "summary" else verb,
              label = NA_character_, keep = TRUE
            )
            result <- element_name_result(input, included, verb, label,
                                          keep = TRUE)
            expect_element_names(result, control, paste(size, verb, included))
          }
        }
      }
    }
  }
})

test_that("ordered and duplicate plans keep native value-name pairs", {
  input <- element_name_input(tibble::as_tibble)
  for (verb in c("summary", "alias", "expand", "nest", "rowwise")) {
    included <- if (verb %in% c("summary", "alias", "expand")) {
      c(TRUE, FALSE, TRUE)
    } else {
      c(TRUE, FALSE)
    }
    for (sort in c("last", "first")) {
      expect_element_name_input(input, c("same", "", NA_character_))
      control <- element_name_reference(
        input, included, if (verb == "alias") "summary" else verb, keep = TRUE
      )
      control <- if (sort == "last") {
        dplyr::arrange(control, .data$k, .data$set == 2L, .data$g, .data$set)
      } else {
        dplyr::arrange(control, .data$k, .data$set != 2L, .data$g, .data$set)
      }
      result <- element_name_result(input, included, verb, keep = TRUE,
                                    sort = sort)
      expect_element_names(result, control, paste(verb, sort))
    }
  }
})

test_that("direct data.table names follow its native operations", {
  skip_if_suggest_absent("data.table")
  for (size in c(0L, 3L)) {
    element_names <- c("same", "", NA_character_)[seq_len(size)]
    for (verb in c("summary", "alias", "expand", "nest", "rowwise")) {
      input <- data.table::as.data.table(element_name_input(size = size))
      for (column in names(input)) {
        data.table::setattr(input[[column]], "names",
                            element_names)
      }
      expect_element_name_input(input, element_names)
      control <- element_name_reference(
        data.table::copy(input), c(TRUE, FALSE),
        if (verb == "alias") "summary" else verb, keep = TRUE
      )
      result <- element_name_result(input, c(TRUE, FALSE), verb, keep = TRUE)
      expect_element_names(result, control, verb)
    }
  }
})

test_that("dtplyr names follow native evaluation and local rowwise nesting", {
  skip_if_suggest_absent("dtplyr")
  for (size in c(0L, 3L)) {
    element_names <- c("same", "", NA_character_)[seq_len(size)]
    for (verb in c("summary", "alias", "expand", "nest", "rowwise")) {
      input <- dtplyr::lazy_dt(element_name_input(size = size))
      for (column in names(input$parent)) {
        data.table::setattr(input$parent[[column]], "names",
                            element_names)
      }
      expect_element_name_input(input$parent, element_names)
      reference_input <- if (verb == "rowwise") dplyr::collect(input) else input
      control <- element_name_reference(
        reference_input, c(TRUE, FALSE),
        if (verb == "alias") "summary" else verb,
        keep = TRUE
      )
      result <- element_name_result(input, c(TRUE, FALSE), verb, keep = TRUE)
      expect_element_names(
        dplyr::arrange(dplyr::collect(result), .data$set, .data$k, .data$g),
        dplyr::arrange(dplyr::collect(control), .data$set, .data$k, .data$g),
        verb
      )
      expect_element_name_input(input$parent, element_names)
    }
  }
})

test_that("Arrow names follow native R metadata without row tracking", {
  skip_if_suggest_absent("arrow")
  for (size in c(0L, 3L)) {
    element_names <- c("same", "", NA_character_)[seq_len(size)]
    table <- arrow::Table$create(element_name_input(tibble::as_tibble, size,
                                                    element_names))
    for (input_mode in c("plain", "arranged", "filtered")) {
      input <- switch(input_mode, plain = table,
                      arranged = dplyr::arrange(table, dplyr::desc(z)),
                      filtered = dplyr::filter(table, z > 1))
      sort <- if (input_mode == "arranged") "last" else "none"
      for (summary in c(FALSE, TRUE)) {
        for (included in list(TRUE, c(TRUE, FALSE), c(TRUE, FALSE, TRUE))) {
          metadata_source <- if (inherits(input, "arrow_dplyr_query")) {
            input$.data
          } else {
            input
          }
          # Arrow stores native R element names on its input schema.
          for (column in c("k", "g", "z")) {
            expect_identical(
              metadata_source$schema$metadata$r$columns[[column]]$attributes[[
                "names"
              ]],
              element_names
            )
          }
          branches <- lapply(seq_along(included), function(i) {
            keys <- c("k", "g"[included[[i]]])
            branch <- if (summary) {
              dplyr::summarise(input, z = mean(.data$z),
                               .by = dplyr::all_of(keys))
            } else {
              input
            }
            if (!included[[i]]) branch <- dplyr::mutate(branch, g = "Total")
            branch <- dplyr::select(branch, dplyr::all_of(c("k", "g", "z")))
            dplyr::mutate(branch, set = i)
          })
          control <- Reduce(dplyr::union_all, branches)
          if (sort == "last") {
            control <- dplyr::arrange(control, k, set == 2L, g, set)
          }
          spec <- grouping_sets(!!!lapply(included, function(present) {
            if (present) grouping_set(g) else grouping_set()
          }))
          observed <- observe_arrow_collection(if (summary) {
            summarize_with_margins(input, z = mean(.data$z), .by = k,
                                   .grouping = spec, .id = "set",
                                   .duplicates = "keep", .sort = sort)
          } else {
            expand_with_margins(input, .by = k, .grouping = spec,
                                .id = "set", .duplicates = "keep", .sort = sort)
          })
          expect_identical(observed$entries, 0L)
          actual <- element_name_collect(observed$result)
          expected <- element_name_collect(control)
          expect_identical(actual$warnings, expected$warnings)
          for (column in c("k", "g", "z")) {
            expect_identical(names(actual$result[[column]]),
                             names(expected$result[[column]]),
                             info = paste(size, input_mode, summary, column))
          }
        }
      }
    }
  }
})

test_that("calculated names follow native evaluation once per group", {
  input <- element_name_input(tibble::as_tibble)
  calls <- 0L
  calculate <- function(x) {
    calls <<- calls + 1L
    stats::setNames(sum(x), "calculated")
  }
  control <- dplyr::union_all(
    dplyr::summarise(input, z = calculate(.data$z), .by = c(k, g)),
    dplyr::mutate(dplyr::summarise(input, z = calculate(.data$z), .by = k),
                  g = "Total")
  )
  expected_calls <- calls
  calls <- 0L
  expect_element_name_input(input, c("same", "", NA_character_))
  result <- summarize_with_margins(input, z = calculate(.data$z), .by = k,
                                   .grouping = rollup(g))
  expect_identical(calls, expected_calls)
  expect_identical(calls, 5L)
  expect_element_names(result, control)
})

test_that("typed-missing dimensions follow native element names", {
  for (values in list(c(2, 1, 2), as.Date(c("2026-01-02", "2026-01-01",
                                            "2026-01-02")),
                      ordered(c("b", "a", "b")))) {
    input <- element_name_input(tibble::as_tibble)
    names(values) <- c("same", "", NA_character_)
    input$g <- values
    replacement <- vctrs::vec_init(vctrs::vec_ptype(values), 1L)
    for (verb in c("summary", "expand", "nest", "rowwise")) {
      for (included in list(TRUE, c(TRUE, FALSE))) {
        expect_element_name_input(input, c("same", "", NA_character_))
        control <- element_name_reference(input, included, verb,
                                          replacement, keep = TRUE)
        result <- element_name_result(input, included, verb, NULL, keep = TRUE)
        result <- dplyr::arrange(result, .data$set, .data$k, .data$g)
        control <- dplyr::arrange(control, .data$set, .data$k, .data$g)
        expect_element_names(result, control, paste(class(values)[[1L]], verb))
      }
    }
  }
})

test_that("factor names survive when native composition does not cast them", {
  for (na_level in c(FALSE, TRUE)) {
    input <- element_name_input(tibble::as_tibble)
    input$g <- factor(c("b", "a", "b"),
                      levels = c("a", "b", if (na_level) NA_character_),
                      exclude = NULL)
    names(input$g) <- c("same", "", NA_character_)
    for (verb in c("summary", "expand", "nest", "rowwise")) {
      plans <- list(TRUE, c(TRUE, FALSE))
      if (verb %in% c("summary", "expand")) {
        plans <- c(plans, list(c(TRUE, TRUE)))
      }
      for (included in plans) {
        expect_element_name_input(input, c("same", "", NA_character_))
        control <- element_name_reference(input, included, verb, keep = TRUE)
        result <- element_name_result(input, included, verb, keep = TRUE)
        result <- dplyr::arrange(result, .data$set, .data$k, .data$g)
        control <- dplyr::arrange(control, .data$set, .data$k, .data$g)
        expect_identical(names(result$g), names(control$g), info = verb)
        if ("data" %in% names(control)) {
          expect_identical(lapply(result$data, function(cell) names(cell$g)),
                           lapply(control$data, function(cell) names(cell$g)),
                           info = verb)
        }
      }
    }
  }
})
