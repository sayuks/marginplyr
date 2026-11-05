test_that("an expanded summary may share its assigned name with an id", {
  data <- data.frame(g = c("b", "a", "a"), x = 1:3)
  frame <- function(columns) data.frame(out = sum(columns[[1L]]))
  result <- summarize_with_margins(
    data, frame(dplyr::pick(x)), later = out * 2L,
    .grouping = rollup(g), .id = "frame(dplyr::pick(x))"
  )
  expect_identical(names(result),
                   c("g", "frame(dplyr::pick(x))", "out", "later"))
  expect_identical(result$g, c("b", "a", "Total"))
  expect_identical(result[["frame(dplyr::pick(x))"]], c(1L, 1L, 2L))
  expect_identical(result$out, c(1L, 5L, 6L))
  expect_identical(result$later, c(2L, 10L, 12L))
})

check_assigned_public_names <- function(convert) {
  output <- 'frame(dplyr::pick("x"))'
  for (role in c("fixed", "dimension", "id")) {
    for (sort in c("none", "first", "last")) {
      data <- convert(data.frame(g = c("b", "a", "a"), x = 1:3))
      if (role != "id") data[[output]] <- data$g
      counter <- new.env(parent = emptyenv())
      counter$calls <- 0L
      frame <- function(columns) {
        counter$calls <- counter$calls + 1L
        data.frame(out = sum(columns[[1L]]))
      }
      result <- switch(role,
        fixed = summarize_with_margins(
          data, frame(dplyr::pick("x")), later = .data$out * 2L,
          .by = dplyr::all_of(output), .sort = sort
        ),
        dimension = summarize_with_margins(
          data, frame(dplyr::pick("x")), later = .data$out * 2L,
          .grouping = rollup(dplyr::all_of(output)), .sort = sort
        ),
        id = summarize_with_margins(
          data, frame(dplyr::pick("x")), later = .data$out * 2L,
          .grouping = rollup("g"), .id = output, .sort = sort
        )
      )
      expected <- if (role == "fixed") {
        if (sort == "none") c(1L, 5L) else c(5L, 1L)
      } else {
        switch(sort, none = c(1L, 5L, 6L), first = c(6L, 5L, 1L),
               last = c(5L, 1L, 6L))
      }
      expected_names <- if (role == "id") {
        c("g", output, "out", "later")
      } else {
        c(output, "out", "later")
      }
      expect_identical(names(result), expected_names)
      expected_keys <- if (role == "fixed") {
        if (sort == "none") c("b", "a") else c("a", "b")
      } else {
        switch(sort, none = c("b", "a", "Total"),
               first = c("Total", "a", "b"), last = c("a", "b", "Total"))
      }
      key <- if (role == "id") "g" else output
      expect_identical(result[[key]], expected_keys)
      if (role == "id") {
        expect_identical(result[[output]],
                         if (sort == "first") c(2L, 1L, 1L) else c(1L, 1L, 2L))
      }
      expect_identical(result$out, expected)
      expect_identical(result$later, expected * 2L)
      expect_identical(counter$calls, if (role == "fixed") 2L else 3L)
    }
  }
}

test_that("plain frames check public names after unnamed expansion", {
  check_assigned_public_names(identity)
})

test_that("tibbles check public names after unnamed expansion", {
  check_assigned_public_names(tibble::as_tibble)
})

test_that("eager tables check public names after unnamed expansion", {
  skip_if_suggest_absent("data.table")
  check_assigned_public_names(data.table::as.data.table)
})

test_that("a retained Assigned summary name still collides with public names", {
  output <- "frame(dplyr::pick(x))"
  data <- data.frame(g = c("b", "a", "a"), x = 1:3)
  data[[output]] <- data$g
  frame <- function(columns) sum(columns[[1L]])
  expect_error(
    summarize_with_margins(
      data, frame(dplyr::pick(x)), .by = dplyr::all_of(output)
    ),
    "cannot overwrite grouping column", class = "marginplyr_error"
  )
  expect_error(
    summarize_with_margins(
      data, frame(dplyr::pick(x)),
      .grouping = rollup(dplyr::all_of(output))
    ),
    "cannot overwrite grouping column", class = "marginplyr_error"
  )
  data[[output]] <- NULL
  expect_error(
    summarize_with_margins(data, frame(dplyr::pick(x)), .id = output),
    "conflicts with a summary output", class = "marginplyr_error"
  )
})

test_that("actual expanded names still collide with public names", {
  output <- "frame(dplyr::pick(x))"
  data <- data.frame(g = c("b", "a", "a"), x = 1:3)
  data[[output]] <- data$g
  frame <- function(columns) {
    stats::setNames(data.frame(sum(columns[[1L]])), output)
  }
  expect_error(
    summarize_with_margins(
      data, frame(dplyr::pick(x)), .by = dplyr::all_of(output)
    ),
    "cannot overwrite grouping column", class = "marginplyr_error"
  )
  expect_error(
    summarize_with_margins(
      data, frame(dplyr::pick(x)),
      .grouping = rollup(dplyr::all_of(output))
    ),
    "cannot overwrite grouping column", class = "marginplyr_error"
  )
  data[[output]] <- NULL
  expect_error(
    summarize_with_margins(data, frame(dplyr::pick(x)), .id = output),
    "conflicts with a summary output", class = "marginplyr_error"
  )
})
