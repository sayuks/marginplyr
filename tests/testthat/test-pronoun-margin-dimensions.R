test_that("a .data Margin dimension retains its source value", {
  data <- tibble::tibble(.data = "a", value = 1L)
  result <- summarize_with_margins(
    data,
    total = sum(value),
    .grouping = grouping_set(dplyr::all_of(".data"))
  )

  expect_identical(result[[".data"]], "a")
  expect_identical(result$total, 1L)
})

run_pronoun_margin_verb <- function(verb, data, name, plan, sort, check,
                                    id, label = "Total") {
  grouping <- if (identical(plan, "one")) {
    grouping_set(dplyr::all_of(name))
  } else {
    rollup(dplyr::all_of(name))
  }
  if (identical(verb, "summarize_with_margins")) {
    return(summarize_with_margins(
      data, total = sum(.data[["value"]]), .grouping = grouping,
      .margin_label = label, .check_margin_label = check,
      .sort = sort, .id = id
    ))
  }
  do.call(get(verb, mode = "function"), list(
    .data = data, .grouping = grouping,
    .margin_label = label, .check_margin_label = check,
    .sort = sort, .id = id
  ))
}

test_that("pronoun-named dimensions preserve keys across Margin options", {
  verbs <- c(
    "summarize_with_margins", "expand_with_margins",
    "nest_with_margins", "nest_by_with_margins"
  )
  for (name in c(".data", ".env")) {
    for (numeric_key in c(FALSE, TRUE)) {
      values <- if (numeric_key) c(1L, 2L) else c("a", "b")
      for (empty in c(FALSE, TRUE)) {
        data <- tibble::tibble(value = c(1L, 2L))
        data[[name]] <- values
        if (empty) data <- data[0, ]
        for (plan in c("one", "rollup")) {
          for (sort in c("none", "last", "first")) {
            for (check in c(FALSE, TRUE)) {
              for (with_id in c(FALSE, TRUE)) {
                for (verb in verbs) {
                  info <- paste(name, numeric_key, empty, plan, sort,
                                check, with_id, verb, sep = "/")
                  id <- if (with_id) "set" else NULL
                  result <- run_pronoun_margin_verb(
                    verb, data, name, plan, sort, check, id
                  )
                  expect_true(name %in% names(result), info = info)
                  expect_identical("set" %in% names(result), with_id,
                                   info = info)
                  expect_identical(result[[name]],
                                   as.character(result[[name]]), info = info)
                  if (empty) {
                    expected_rows <- if (verb == "summarize_with_margins" &&
                                           plan == "rollup") 1L else 0L
                    expect_identical(nrow(result), expected_rows, info = info)
                    if (expected_rows == 1L) {
                      expect_identical(result[[name]], "Total", info = info)
                      expect_identical(result$total, 0L, info = info)
                    }
                    next
                  }

                  detail <- result[[name]] != "Total"
                  expect_true(setequal(result[[name]][detail],
                                       as.character(values)), info = info)
                  if (with_id) {
                    expect_true(all(result$set[detail] == 1L), info = info)
                  }
                  if (identical(verb, "summarize_with_margins")) {
                    expect_identical(
                      base::sort(result$total[detail]), c(1L, 2L), info = info
                    )
                  } else if (identical(verb, "expand_with_margins")) {
                    expect_identical(
                      base::sort(result$value[detail]), c(1L, 2L), info = info
                    )
                  } else {
                    cell_sizes <- vapply(result$data[detail], nrow, integer(1))
                    expect_identical(base::sort(cell_sizes), c(1L, 1L),
                                     info = info)
                  }
                  if (identical(plan, "rollup")) {
                    margin <- !detail
                    expected_margin_rows <- if (verb == "expand_with_margins") {
                      2L
                    } else {
                      1L
                    }
                    expect_identical(sum(margin), expected_margin_rows,
                                     info = info)
                    if (with_id) {
                      expect_true(all(result$set[margin] == 2L), info = info)
                    }
                    if (identical(verb, "summarize_with_margins")) {
                      expect_identical(result$total[margin], 3L, info = info)
                    } else if (identical(verb, "expand_with_margins")) {
                      expect_identical(
                        base::sort(result$value[margin]), c(1L, 2L), info = info
                      )
                    } else {
                      expect_identical(nrow(result$data[[which(margin)]]), 2L,
                                       info = info)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
})

test_that("a two-dimension rollup labels only omitted pronoun keys", {
  data <- tibble::tibble(.data = "a", .env = "b", value = 1L)
  result <- summarize_with_margins(
    data, total = sum(value),
    .grouping = rollup(dplyr::all_of(c(".data", ".env"))),
    .id = "set", .sort = "last"
  )

  expect_identical(result[[".data"]], c("a", "a", "Total"))
  expect_identical(result[[".env"]], c("b", "Total", "Total"))
  expect_identical(result$set, 1:3)
  expect_identical(result$total, c(1L, 1L, 1L))
})

test_that("Margin order places pronoun totals as requested", {
  data <- tibble::tibble(.env = c("b", "a"), value = 1:2)
  for (sort in c("last", "first")) {
    result <- summarize_with_margins(
      data, total = sum(value),
      .grouping = rollup(dplyr::all_of(".env")), .sort = sort
    )
    keys <- if (identical(sort, "last")) {
      c("a", "b", "Total")
    } else {
      c("Total", "a", "b")
    }
    expect_identical(result[[".env"]], keys)
  }
})

test_that("pronoun factor dimensions keep NA levels and label position", {
  verbs <- c(
    "summarize_with_margins", "expand_with_margins",
    "nest_with_margins", "nest_by_with_margins"
  )
  for (name in c(".data", ".env")) {
    for (ordered_key in c(FALSE, TRUE)) {
      for (position in c("last", "first")) {
        data <- tibble::tibble(value = 1:3)
        data[[name]] <- structure(
          c(1L, 2L, NA_integer_), levels = c("a", NA_character_),
          class = if (ordered_key) c("ordered", "factor") else "factor"
        )
        grouping <- rollup(dplyr::all_of(name))
        for (verb in verbs) {
          args <- list(
            .data = data, .grouping = grouping, .id = "set",
            .margin_label_position = position
          )
          if (identical(verb, "summarize_with_margins")) {
            result <- summarize_with_margins(
              data, total = sum(value), .grouping = grouping, .id = "set",
              .margin_label_position = position
            )
          } else {
            result <- do.call(get(verb, mode = "function"), args)
          }
          info <- paste(name, ordered_key, position, verb, sep = "/")
          key <- result[[name]]
          expect_identical(is.ordered(key), ordered_key, info = info)
          expected_levels <- if (identical(position, "last")) {
            c("a", NA_character_, "Total")
          } else {
            c("Total", "a", NA_character_)
          }
          expect_identical(levels(key), expected_levels, info = info)
          detail_codes <- as.integer(key[result$set == 1L])
          expected_codes <- if (identical(position, "last")) {
            c(1L, 2L, NA_integer_)
          } else {
            c(2L, 3L, NA_integer_)
          }
          expect_true(setequal(detail_codes, expected_codes), info = info)
          margin_code <- if (identical(position, "last")) 3L else 1L
          expect_true(all(as.integer(key[result$set == 2L]) == margin_code),
                      info = info)
        }
      }
    }
  }
})

test_that("typed missing labels retain pronoun key types", {
  for (name in c(".data", ".env")) {
    for (label in list(NULL, NA_character_)) {
      data <- tibble::tibble(value = 1:2)
      data[[name]] <- c(1L, NA_integer_)
      result <- summarize_with_margins(
        data, total = sum(value),
        .grouping = rollup(dplyr::all_of(name)),
        .margin_label = label, .id = "set"
      )
      expect_identical(result[[name]][result$set == 1L],
                       c(1L, NA_integer_))
      expect_identical(result[[name]][result$set == 2L], NA_integer_)
    }
    data <- tibble::tibble(value = 1:2)
    data[[name]] <- structure(c(1L, 2L),
                              levels = c("a", NA_character_), class = "factor")
    result <- summarize_with_margins(
      data, total = sum(value),
      .grouping = rollup(dplyr::all_of(name)),
      .margin_label = NULL, .id = "set"
    )
    expect_identical(levels(result[[name]]), c("a", NA_character_))
    expect_true(setequal(as.integer(result[[name]][result$set == 1L]),
                         c(1L, 2L)))
    expect_identical(as.integer(result[[name]][result$set == 2L]),
                     NA_integer_)
  }
})

test_that("pronoun dimensions still reject observed label collisions", {
  for (name in c(".data", ".env")) {
    data <- tibble::tibble(value = 1L)
    data[[name]] <- "Total"
    error <- expect_error(summarize_with_margins(
      data, total = sum(value),
      .grouping = rollup(dplyr::all_of(name))
    ), "already present in grouping column", fixed = TRUE)
    expect_s3_class(error, "marginplyr_error")

    result <- summarize_with_margins(
      data, total = sum(value),
      .grouping = rollup(dplyr::all_of(name)),
      .check_margin_label = FALSE, .id = "set"
    )
    expect_identical(result[[name]], c("Total", "Total"))
    expect_identical(result$set, 1:2)
  }
})

test_that("pronoun fixed keys, payloads, and summary names remain available", {
  fixed <- tibble::tibble(.data = "fixed", g = "a", value = 1L)
  summary <- summarize_with_margins(
    fixed, .env = sum(value),
    .by = dplyr::all_of(".data"), .grouping = rollup(g)
  )
  expect_identical(summary[[".data"]], c("fixed", "fixed"))
  expect_identical(summary[[".env"]], c(1L, 1L))

  payload <- tibble::tibble(g = "a", .env = "payload")
  expanded <- expand_with_margins(payload, .grouping = rollup(g))
  expect_identical(expanded[[".env"]], c("payload", "payload"))
  nested <- nest_with_margins(payload, .grouping = rollup(g))
  expect_true(all(vapply(nested$data, function(cell) {
    identical(cell[[".env"]], "payload")
  }, logical(1))))
})

test_that("dtplyr preserves pronoun dimensions through generated conversion", {
  skip_if_suggest_absent("dtplyr")
  verbs <- c(
    "summarize_with_margins", "expand_with_margins",
    "nest_with_margins", "nest_by_with_margins"
  )
  for (name in c(".data", ".env")) {
    data <- tibble::tibble(value = 1L)
    data[[name]] <- "a"
    source <- dtplyr::lazy_dt(data)
    for (verb in verbs) {
      result <- run_pronoun_margin_verb(
        verb, source, name, "rollup", "last", TRUE, "set"
      )
      if (!identical(verb, "nest_by_with_margins")) {
        expect_s3_class(result, "dtplyr_step")
        result <- dplyr::collect(result)
      }
      info <- paste(name, verb, sep = "/")
      expect_identical(result[[name]], c("a", "Total"), info = info)
      expect_identical(result$set, 1:2, info = info)
      if (identical(verb, "summarize_with_margins")) {
        expect_identical(result$total, c(1L, 1L), info = info)
      } else if (identical(verb, "expand_with_margins")) {
        expect_identical(result$value, c(1L, 1L), info = info)
      } else {
        expect_true(all(vapply(result$data, function(cell) {
          identical(cell$value, 1L)
        }, logical(1))), info = info)
      }
    }
    expect_identical(dplyr::collect(source)[[name]], "a")
  }
})
