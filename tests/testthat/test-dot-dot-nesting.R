test_that("dot-dot list-column names survive nesting cleanup", {
  input <- tibble::tibble(g = c("a", "b"), value = 1:2)
  for (key in c("...", "..1", "..10")) {
    for (id in list(NULL, "set")) {
      for (multiple in c(FALSE, TRUE)) {
        result <- nest_with_margins(
          input, .by = if (!multiple) dplyr::all_of("g"),
          .grouping = if (multiple) rollup(g), .key = key, .id = id
        )
        expect_identical(names(result), c("g", id, key))
        expect_identical(result[["g"]], if (multiple) c("a", "b", "Total")
                         else c("a", "b"))
        expect_identical(lapply(result[[key]], as.list), if (multiple) {
          list(list(value = 1L), list(value = 2L), list(value = 1:2))
        } else {
          list(list(value = 1L), list(value = 2L))
        })
      }
    }
  }
})

test_that("dot-dot list-column names survive rowwise conversion", {
  input <- tibble::tibble(g = c("a", "b"), value = 1:2)
  for (key in c("...", "..1", "..10")) {
    result <- nest_by_with_margins(input, .by = g, .id = "set", .key = key)
    expect_identical(names(result), c("g", "set", key))
    expect_s3_class(result, "rowwise_df")
    expect_identical(dplyr::group_vars(result), c("g", "set"))
    expect_identical(lapply(result[[key]], as.list),
                     list(list(value = 1L), list(value = 2L)))
  }
})

# Compare cells and public keys independently of backend-specific cell classes.
dot_dot_nesting_values <- function(result, key) {
  columns <- as.list(dplyr::ungroup(result))
  columns[[key]] <- lapply(columns[[key]], function(cell) {
    list(rows = nrow(cell), values = as.list(cell))
  })
  names(columns)[names(columns) == key] <- "data"
  columns
}

expect_dot_dot_nesting <- function(as_source, lazy = FALSE) {
  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    rowwise <- identical(verb, nest_by_with_margins)
    for (plan in c("none", "fixed", "rollup")) {
      for (size in c(0L, 2L)) {
        input <- as_source(tibble::tibble(
          g = c("b", "a")[seq_len(size)], value = seq_len(size)
        ))
        for (keep in c(FALSE, TRUE)) {
          for (id in list(NULL, "set")) {
            for (sort in c("none", "last", "first")) {
              run <- function(key) {
                verb(
                  input, .by = if (plan == "fixed") dplyr::all_of("g"),
                  .grouping = if (plan == "rollup") rollup(dplyr::all_of("g")),
                  .key = key, .keep = keep, .id = id, .sort = sort
                )
              }
              control <- dplyr::collect(run("data"))
              public_keys <- c(character(), if (plan != "none") "g", id)
              for (key in c("...", "..1", "..10")) {
                result <- run(key)
                info <- paste(rowwise, plan, size, keep, id, sort, key)
                if (lazy && !rowwise) expect_s3_class(result, "dtplyr_step")
                result <- if (lazy && !rowwise) {
                  tibble::as_tibble(result, .name_repair = "minimal")
                } else {
                  dplyr::collect(result)
                }
                expect_identical(
                  names(result), c(public_keys, key), info = info
                )
                expect_identical(
                  dot_dot_nesting_values(result, key),
                  dot_dot_nesting_values(control, "data"), info = info
                )
                expect_identical(
                  dplyr::group_vars(result),
                  if (rowwise) public_keys else character(), info = info
                )
                if (rowwise) {
                  expect_s3_class(result, "rowwise_df")
                  if (plan == "none" && size == 0L) {
                    expect_identical(nrow(result), 1L, info = info)
                    expect_identical(nrow(result[[key]][[1L]]), 0L, info = info)
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

test_that("local nesting preserves dot-dot list columns for Margin options", {
  expect_dot_dot_nesting(identity)
  expect_dot_dot_nesting(as.data.frame)
})

test_that("dtplyr nesting preserves dot-dot list columns lazily", {
  skip_if_suggest_absent("dtplyr")
  expect_dot_dot_nesting(function(data) dtplyr::lazy_dt(data), lazy = TRUE)
})

test_that("dot-dot nesting list columns retain identifier collision checks", {
  input <- tibble::tibble(g = c("a", "b"), value = 1:2)
  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    for (key in c("...", "..1", "..10")) {
      expect_error(
        verb(input, .grouping = rollup(g), .key = key, .id = key),
        "conflicts with nesting", class = "marginplyr_error"
      )
    }
  }
})
