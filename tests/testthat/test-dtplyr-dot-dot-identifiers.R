test_that("dtplyr nesting preserves a dot-dot Grouping set identifier", {
  skip_if_suggest_absent("dtplyr")
  input <- dtplyr::lazy_dt(tibble::tibble(value = 1L))
  result <- nest_with_margins(input, .id = "..1")
  expect_s3_class(result, "dtplyr_step")
  result <- tibble::as_tibble(result, .name_repair = "minimal")
  expect_identical(names(result), c("..1", "data"))
  expect_identical(result[["..1"]], 1L)
  expect_identical(as.list(result$data[[1L]]), list(value = 1L))
})

# Each plan is exercised through the exported verbs, against the ordinary-name
# control. The duplicated plan retains the same two occurrences as the rollup.
dot_dot_identifier_plans <- function() {
  list(
    none = NULL,
    fixed = NULL,
    rollup = rollup(dplyr::all_of("g")),
    fixed_rollup = rollup(dplyr::all_of("g")),
    duplicates = grouping_sets(
      grouping_set(dplyr::all_of("g")), rollup(dplyr::all_of("g"))
    )
  )
}

# Compare outer values and cell membership without rowwise or data.table
# attributes, keeping column names, types, row order, and empty-cell size.
dot_dot_identifier_values <- function(result, id, nested = FALSE) {
  columns <- as.list(dplyr::ungroup(result))
  names(columns)[names(columns) == id] <- "set"
  if (nested) {
    columns$data <- lapply(columns$data, function(cell) {
      list(rows = nrow(cell), columns = as.list(cell))
    })
  }
  columns
}

test_that("dtplyr nesting retains dot-dot identifiers across Margin options", {
  skip_if_suggest_absent("dtplyr")
  plans <- dot_dot_identifier_plans()
  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    rowwise <- identical(verb, nest_by_with_margins)
    for (plan in names(plans)) {
      fixed <- plan %in% c("fixed", "fixed_rollup")
      keys <- c(if (fixed) "f", if (!is.null(plans[[plan]])) "g")
      for (size in c(0L, 4L)) {
        input <- dtplyr::lazy_dt(tibble::tibble(
          f = c(2L, 2L, 1L, 1L)[seq_len(size)],
          g = c("b", "a", "b", "a")[seq_len(size)], value = seq_len(size)
        ))
        for (keep in c(FALSE, TRUE)) {
          for (sort in c("none", "last", "first")) {
            run <- function(id) {
              verb(
                input, .by = if (fixed) dplyr::all_of("f"),
                .grouping = plans[[plan]], .duplicates = "drop",
                .id = id, .keep = keep, .sort = sort
              )
            }
            control <- dplyr::collect(run("set"))
            for (id in c("...", "..1", "..2", "..0", "..01")) {
              info <- paste(rowwise, plan, size, keep, sort, id)
              result <- run(id)
              if (!rowwise) expect_s3_class(result, "dtplyr_step")
              result <- if (rowwise) {
                result
              } else {
                tibble::as_tibble(result, .name_repair = "minimal")
              }
              expect_identical(names(result), c(keys, id, "data"), info = info)
              expect_type(result[[id]], "integer")
              expect_identical(
                dot_dot_identifier_values(result, id, nested = TRUE),
                dot_dot_identifier_values(control, "set", nested = TRUE),
                info = info
              )
              expect_identical(dplyr::group_vars(result),
                               if (rowwise) c(keys, id) else character(),
                               info = info)
              expect_false(any(vapply(result$data, function(cell) {
                id %in% names(cell)
              }, logical(1))))
              if (size > 0L) {
                expect_setequal(result[[id]],
                                if (length(keys) == 0L || plan == "fixed") 1L
                                else 1:2)
              }
              if (rowwise) {
                expect_s3_class(result, "rowwise_df")
                if (size == 0L && plan == "none") {
                  expect_identical(result[[id]], 1L)
                  expect_identical(nrow(result$data[[1L]]), 0L)
                }
              }
            }
          }
        }
      }
    }
  }
})

test_that("dtplyr summary and expansion finalize dot-dot identifiers", {
  skip_if_suggest_absent("dtplyr")
  plans <- dot_dot_identifier_plans()
  verbs <- list(
    summary = function(...) summarize_with_margins(..., total = sum(value)),
    expand = expand_with_margins
  )
  for (verb in verbs) {
    for (plan in names(plans)) {
      for (size in c(0L, 4L)) {
        input <- dtplyr::lazy_dt(tibble::tibble(
          f = c(2L, 2L, 1L, 1L)[seq_len(size)],
          g = c("b", "a", "b", "a")[seq_len(size)], value = seq_len(size)
        ))
        for (sort in c("none", "last", "first")) {
          run <- function(id) {
            verb(
              input,
              .by = if (plan %in% c("fixed", "fixed_rollup")) {
                dplyr::all_of("f")
              },
              .grouping = plans[[plan]], .duplicates = "drop",
              .id = id, .sort = sort
            )
          }
          control <- dplyr::collect(run("set"))
          for (id in c("...", "..1", "..2", "..0", "..01")) {
            query <- run(id)
            expect_s3_class(query, "dtplyr_step")
            result <- tibble::as_tibble(query, .name_repair = "minimal")
            expect_type(result[[id]], "integer")
            expect_identical(
              dot_dot_identifier_values(result, id),
              dot_dot_identifier_values(control, "set"),
              info = paste(plan, size, sort, id)
            )
            expect_identical(dplyr::group_vars(result), character())
          }
        }
      }
    }
  }
})


test_that("dtplyr dot-dot identifier aliases avoid public names", {
  skip_if_suggest_absent("dtplyr")
  input <- dtplyr::lazy_dt(tibble::tibble(
    g = c("b", "a"), ..marginplyr_dtplyr_column_1 = 1:2
  ))
  key <- "..marginplyr_dtplyr_column_2"
  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    result <- verb(input, .grouping = rollup(g), .id = "..1",
                   .key = key, .keep = TRUE, .sort = "last")
    result <- if (inherits(result, "dtplyr_step")) {
      tibble::as_tibble(result, .name_repair = "minimal")
    } else {
      result
    }
    expect_identical(names(result), c("g", "..1", key))
    expect_identical(result[["..1"]], c(1L, 1L, 2L))
    expect_identical(names(result[[key]][[3L]]),
                     c("g", "..marginplyr_dtplyr_column_1"))
    expect_identical(result[[key]][[3L]][["..marginplyr_dtplyr_column_1"]], 1:2)
  }
})

test_that("dtplyr identifier handling retains name admission and validation", {
  skip_if_suggest_absent("dtplyr")
  input <- dtplyr::lazy_dt(tibble::tibble(value = 1L))
  for (verb in list(summarize_with_margins, expand_with_margins,
                    nest_with_margins, nest_by_with_margins)) {
    expect_error(verb(input, .id = "value"), "conflicts with a source column",
                 class = "marginplyr_error")
    for (id in list(1, NA_character_, "", c("first", "second"))) {
      expect_error(verb(input, .id = id), "one non-missing, non-empty",
                   class = "marginplyr_error")
    }
    for (name in c("...", "..1", "..2")) {
      source <- dtplyr::lazy_dt(rlang::set_names(
        tibble::tibble(value = 1L), name
      ))
      expect_error(verb(source), class = "vctrs_error_names_cannot_be_dot_dot")
    }
  }
  for (id in c("...", "..1", "..2", "..0", "..01")) {
    for (verb in list(nest_with_margins, nest_by_with_margins)) {
      expect_error(verb(input, .id = id, .key = id), "conflicts with nesting",
                   class = "marginplyr_error")
    }
    expect_error(summarize_with_margins(input, !!id := sum(value), .id = id),
                 "conflicts with a summary output", class = "marginplyr_error")
  }
  expect_error(nest_by_with_margins(input, .id = ".rows"),
               "`.rows`", class = "marginplyr_error")
})
