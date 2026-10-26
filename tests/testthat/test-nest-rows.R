test_that("nesting preserves a .rows Grouping set identifier", {
  input <- tibble::tibble(g = c("a", "b"), value = 1:2)
  result <- nest_with_margins(input, .grouping = rollup(g), .id = ".rows")
  result <- dplyr::arrange(result, .rows, g)

  expect_identical(names(result), c("g", ".rows", "data"))
  expect_identical(result$g, c("a", "b", "Total"))
  expect_identical(result$.rows, c(1L, 1L, 2L))
  expect_identical(lapply(result$data, as.list), list(
    list(value = 1L), list(value = 2L), list(value = 1:2)
  ))
})

test_that("rowwise .rows keys produce actionable Package conditions", {
  input <- tibble::tibble(g = c("a", "b"), value = 1:2)
  names(input)[1L] <- ".rows"
  calls <- list(
    quote(nest_by_with_margins(input, .by = .rows)),
    quote(nest_by_with_margins(input, .grouping = rollup(.rows))),
    quote(nest_by_with_margins(input["value"], .id = ".rows")),
    quote(nest_by_with_margins(
      input["value"], .grouping = rollup(value), .id = ".rows"
    ))
  )
  for (call in calls) {
    error <- expect_error(eval(call), class = "marginplyr_error")
    expect_match(conditionMessage(error), "`.rows`", fixed = TRUE)
    expect_match(conditionMessage(error), "row-wise identifier", fixed = TRUE)
    expect_match(conditionMessage(error), "Rename", fixed = TRUE)
    expect_match(conditionMessage(error), "nest_with_margins", fixed = TRUE)
    expect_false(grepl("..marginplyr_", conditionMessage(error), fixed = TRUE))
    expect_identical(conditionCall(error), call)
  }
})

# Compare public values independently of the backend's cell class, including
# row counts in cells with no columns.
rows_nesting_values <- function(result, key = "data") {
  list(
    outer = as.list(result[setdiff(names(result), key)]),
    cells = lapply(result[[key]], function(cell) {
      list(rows = nrow(cell), values = as.list(cell))
    })
  )
}

expect_rows_outer_nesting <- function(as_source, lazy) {
  for (role in c("identifier", "fixed", "dimension")) {
    for (keep in c(FALSE, TRUE)) {
      for (size in c(0L, 2L)) {
        for (payload in c(FALSE, TRUE)) {
          control <- tibble::tibble(g = c("a", "b")[seq_len(size)])
          if (payload) control$value <- seq_len(size)
          input <- control
          if (role != "identifier") names(input)[1L] <- ".rows"
          run <- function(data, column, id) {
            nest_with_margins(
              as_source(data),
              .by = if (role == "fixed") dplyr::all_of(column),
              .grouping = if (role != "fixed") rollup(dplyr::all_of(column)),
              .id = id, .keep = keep, .sort = "last"
            )
          }
          actual <- run(
            input, if (role == "identifier") "g" else ".rows",
            if (role == "identifier") ".rows"
          )
          if (lazy) {
            expect_s3_class(actual, "dtplyr_step")
            actual <- dplyr::collect(actual)
          }
          expected <- run(control, "g", if (role == "identifier") "set") |>
            dplyr::collect()
          outer_name <- if (role == "identifier") "set" else "g"
          names(expected)[names(expected) == outer_name] <- ".rows"
          if (keep && role != "identifier") {
            expected$data <- lapply(expected$data, function(cell) {
              names(cell)[names(cell) == "g"] <- ".rows"
              cell
            })
          }
          info <- paste(role, keep, size, payload)
          expect_identical(names(actual), names(expected), info = info)
          expect_identical(dplyr::group_vars(actual), character(), info = info)
          expect_identical(
            rows_nesting_values(actual), rows_nesting_values(expected),
            info = info
          )
        }
      }
    }
  }
}

test_that("local nesting preserves .rows in every outer role", {
  expect_rows_outer_nesting(identity, lazy = FALSE)
  expect_rows_outer_nesting(as.data.frame, lazy = FALSE)
})

test_that("dtplyr nesting preserves .rows in every outer role lazily", {
  skip_if_suggest_absent("dtplyr")
  expect_rows_outer_nesting(function(data) {
    dtplyr::lazy_dt(data)
  }, lazy = TRUE)
})

test_that("a .rows identifier works without a Grouping specification", {
  for (keep in c(FALSE, TRUE)) {
    for (size in c(0L, 2L)) {
      for (payload in c(FALSE, TRUE)) {
        input <- tibble::tibble(.rows = size)
        if (payload) input$value <- seq_len(size)
        result <- nest_with_margins(input, .id = ".rows", .keep = keep)
        expect_identical(names(result), c(".rows", "data"))
        expect_identical(result$.rows, rep(1L, as.integer(size > 0L)))
        expect_identical(
          vapply(result$data, nrow, integer(1)),
          if (size > 0L) size else integer()
        )
        if (size > 0L) {
          expect_identical(as.list(result$data[[1L]]), as.list(input))
        }
      }
    }
  }
})

expect_rows_payload_nesting <- function(as_source) {
  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    for (keep in c(FALSE, TRUE)) {
      input <- tibble::tibble(g = c("a", "b"), value = 1:2)
      names(input)[2L] <- ".rows"
      payload <- verb(
        as_source(input), .grouping = rollup(dplyr::all_of("g")),
        .id = "set", .keep = keep, .sort = "last"
      ) |>
        dplyr::collect()
      groups <- if (identical(verb, nest_by_with_margins)) {
        c("g", "set")
      } else {
        character()
      }
      expect_identical(dplyr::group_vars(payload), groups)
      expect_identical(tail(payload$data, 1L)[[1L]]$.rows, 1:2)
      keyed <- verb(
        as_source(input), .grouping = rollup(dplyr::all_of("g")),
        .key = ".rows", .keep = keep, .sort = "last"
      ) |>
        dplyr::collect()
      expect_identical(names(keyed), c("g", ".rows"))
      expect_identical(tail(keyed$.rows, 1L)[[1L]]$.rows, 1:2)
    }
  }
}

test_that(".rows payload and list-column names remain supported locally", {
  expect_rows_payload_nesting(identity)
})

test_that("dtplyr retains .rows payload and list-column names", {
  skip_if_suggest_absent("dtplyr")
  expect_rows_payload_nesting(function(data) dtplyr::lazy_dt(data))
})

expect_rows_rowwise_boundary <- function(as_source) {
  for (keep in c(FALSE, TRUE)) {
    for (size in c(0L, 2L)) {
      for (payload in c(FALSE, TRUE)) {
        input <- tibble::tibble(g = c("Total", "b")[seq_len(size)])
        if (payload) input$value <- seq_len(size)
        names(input)[1L] <- ".rows"
        # The row-wise boundary precedes even semantic Margin-label checks,
        # and thus precedes expansion and folding as well.
        for (role in c("fixed", "dimension", "identifier")) {
          case_input <- input
          if (role == "identifier") names(case_input)[1L] <- "g"
          source <- as_source(case_input)
          error <- expect_error(
            nest_by_with_margins(
              source,
              .by = if (role == "fixed") dplyr::all_of(".rows"),
              .grouping = if (role == "dimension") {
                rollup(dplyr::all_of(".rows"))
              },
              .id = if (role == "identifier") ".rows" else NULL,
              .keep = keep, .check_margin_label = TRUE
            ),
            class = "marginplyr_error"
          )
          expect_match(
            conditionMessage(error), "row-wise identifier", fixed = TRUE
          )
        }
      }
    }
  }
}

test_that("rowwise .rows keys are refused before local nesting", {
  expect_rows_rowwise_boundary(identity)
})

test_that("rowwise .rows keys are refused before dtplyr nesting", {
  skip_if_suggest_absent("dtplyr")
  expect_rows_rowwise_boundary(function(data) dtplyr::lazy_dt(data))
})

test_that("dtplyr retains a .rows identifier without grouping", {
  skip_if_suggest_absent("dtplyr")
  for (keep in c(FALSE, TRUE)) {
    for (size in c(0L, 2L)) {
      input <- tibble::tibble(value = seq_len(size))
      result <- nest_with_margins(
        dtplyr::lazy_dt(input), .id = ".rows", .keep = keep
      )
      expect_s3_class(result, "dtplyr_step")
      expected <- nest_with_margins(input, .id = "set", .keep = keep)
      names(expected)[1L] <- ".rows"
      expect_identical(
        rows_nesting_values(dplyr::collect(result)),
        rows_nesting_values(expected)
      )
    }
  }
})

test_that(".rows folding avoids occupied private names", {
  input <- tibble::tibble(
    g = c("a", "b"), ..marginplyr_column_1 = 1:2,
    ..marginplyr_column_1_ = 3:4, ..marginplyr_nest_1 = 5:6
  )
  names(input)[1L] <- ".rows"
  result <- nest_with_margins(
    input, .grouping = rollup(.rows), .keep = TRUE, .sort = "last"
  )
  expect_identical(names(result), c(".rows", "data"))
  expect_identical(names(result$data[[3L]]), names(input))
  expect_identical(as.list(result$data[[3L]]), as.list(input))
})
