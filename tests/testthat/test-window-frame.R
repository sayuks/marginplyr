# A lazy SQL Margin result holds its input's `dbplyr::window_frame()` exactly
# when it holds the input's usable window order (ADR 0018's `.sort = "none"`
# amendment, #1815). Each backend has its own test; the shapes are shared here.

window_frame_facts <- function() {
  data.frame(
    k = c("p", "p", "q", "q", "p", "q"),
    a = c("x", "y", "x", "y", "x", "y"),
    b = c("u", "u", "w", "w", "u", "w"),
    v = c(1, 2, 3, 4, 5, 6)
  )
}

# Every shape reaches a different executor path: multi-set, single-set and
# `.grouping = NULL` plans, with and without `.by` and `.id`, under each
# duplicate policy, for the expansion and the summary with and without a share.
window_frame_results <- function(input, sort) {
  # The symbols are columns and summary names in the public data mask.
  # nolint start: object_usage_linter.
  summary <- function(share, ...) {
    switch(share,
      none = summarize_with_margins(input, n = sum(v, na.rm = TRUE), ...),
      total = summarize_with_margins(
        input, n = sum(v, na.rm = TRUE), p = share_of_total(n), ...,
        .check_share_source = FALSE
      ),
      parent = summarize_with_margins(
        input, n = sum(v, na.rm = TRUE), p = share_of_parent(n), ...,
        .check_share_source = FALSE
      )
    )
  }
  # nolint end
  plans <- list(
    list(.grouping = rollup("a", "b"), .duplicates = "error",
         shares = c("none", "total", "parent")),
    list(.grouping = rollup("a", "b", "b"), .duplicates = "keep",
         shares = c("none", "total", "parent")),
    list(.grouping = rollup("a", "b", "b"), .duplicates = "drop",
         shares = c("none", "total", "parent")),
    list(.grouping = grouping_sets("a"), .duplicates = "error",
         shares = "none"),
    list(.grouping = NULL, .duplicates = "error", shares = c("none", "total"))
  )
  results <- list()
  for (plan in plans) {
    for (by in list(NULL, "k")) {
      for (id in list(NULL, "set")) {
        arguments <- list(
          .grouping = plan$.grouping, .duplicates = plan$.duplicates,
          .by = by, .id = id, .sort = sort
        )
        label <- paste(
          paste(deparse(plan$.grouping), collapse = ""), plan$.duplicates,
          by %||% "-",
          id %||% "-", sort
        )
        results[[paste(label, "expand")]] <-
          do.call(expand_with_margins, c(list(input), arguments))
        for (share in plan$shares) {
          results[[paste(label, share)]] <- do.call(
            summary, c(list(share), arguments)
          )
        }
      }
    }
  }
  results
}

expect_frame_follows_order <- function(table) {
  frame <- c(-1, 0)
  for (order in c("a", "v")) {
    ordered <- dbplyr::window_order(table, !!rlang::sym(order))
    input <- dbplyr::window_frame(ordered, frame[[1L]], frame[[2L]])
    for (sort in c("none", "first", "last")) {
      results <- window_frame_results(input, sort)
      unframed <- window_frame_results(ordered, sort)
      for (label in names(results)) {
        result <- results[[label]]
        info <- paste("order", order, label)
        if (identical(sort, "none") && length(dbplyr::op_sort(result)) > 0L) {
          expect_identical(dbplyr::op_frame(result), list(range = frame),
                           info = info)
        } else {
          expect_null(dbplyr::op_frame(result), info = info)
        }
        if (!identical(sort, "none")) {
          expect_length(dbplyr::op_sort(result), 0L)
        }
        # A frame is metadata for a later window expression: the result's
        # own statement is the one the unframed input gives.
        expect_identical(
          dbplyr::sql_render(result), dbplyr::sql_render(unframed[[label]]),
          info = info
        )
      }
    }
  }
}

# A later frame-sensitive aggregate over the expansion returns what it returns
# over the `mutate()` + `union_all()` composition `?expand_with_margins` names.
expect_expansion_frame_values <- function(table) {
  input <- dbplyr::window_frame(
    dbplyr::window_order(dplyr::select(table, "a", "v"), .data$v), -1, 0
  )
  native <- dplyr::union_all(
    dplyr::mutate(input, set = 1L),
    dplyr::mutate(input, a = "Total", set = 2L)
  )
  # `set`, `a` and `v` are result columns in the public data mask.
  # nolint start: object_usage_linter.
  later <- function(query) {
    dplyr::collect(dplyr::mutate(
      query, r = sum(v, na.rm = TRUE), .by = c(set, a)
    )) |>
      dplyr::arrange(set, a, v)
  }
  # nolint end
  result <- later(
    expand_with_margins(input, .grouping = rollup("a"), .id = "set")
  )
  expect_equal(
    as.data.frame(result[c("set", "a", "v", "r")]),
    as.data.frame(later(native)[c("set", "a", "v", "r")])
  )
  expect_equal(
    result$r, c(1, 4, 8, 2, 6, 10, 1, 3, 5, 7, 9, 11)
  )
}

# A later frame-sensitive aggregate over a share summary returns what it
# returns over the same summary without a share; the share values are the
# unframed input's.
expect_share_frame_values <- function(table) {
  input <- dbplyr::window_frame(dbplyr::window_order(table, .data$a), -1, 0)
  # `n` and `a` are summary and dimension names in the public data mask.
  # nolint start: object_usage_linter.
  later <- function(query) {
    dplyr::collect(dplyr::mutate(query, r = sum(n, na.rm = TRUE))) |>
      dplyr::arrange(a)
  }
  plain <- later(summarize_with_margins(
    input, n = sum(v, na.rm = TRUE), .grouping = rollup(a)
  ))
  shared <- later(summarize_with_margins(
    input, n = sum(v, na.rm = TRUE), p = share_of_total(n),
    .grouping = rollup(a), .check_share_source = FALSE
  ))
  # nolint end
  expect_equal(shared[c("a", "n", "r")], plain[c("a", "n", "r")])
  expect_equal(shared$p, c(1, 9 / 21, 12 / 21))
}

test_that("DuckDB results carry an input frame exactly with its order", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  table <- dplyr::copy_to(con, window_frame_facts(), "facts")
  expect_frame_follows_order(table)
  expect_expansion_frame_values(table)
  expect_share_frame_values(table)
})

test_that("SQLite results carry an input frame exactly with its order", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  table <- dplyr::copy_to(con, window_frame_facts(), "facts")
  expect_frame_follows_order(table)
  # An arranged input is sealed into one relation before the branches read it.
  expect_frame_follows_order(dplyr::arrange(table, v))
  expect_expansion_frame_values(table)
  expect_share_frame_values(table)
})

test_that("generic SQL results carry an input frame exactly with its order", {
  skip_if_suggest_absent("duckdb", "DBI")
  duck <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(duck, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(duck, "facts", window_frame_facts())
  table <- dplyr::tbl(generic_forwarding_connection(duck), "facts")
  expect_frame_follows_order(table)
  expect_expansion_frame_values(table)
  expect_share_frame_values(table)
})
