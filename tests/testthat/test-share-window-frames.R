test_that("SQLite shares see the denominator beyond an input frame", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), x = c(1, 3)), "facts"
  )
  input <- dbplyr::window_frame(
    dbplyr::window_order(source, dplyr::desc(g)), -Inf, 0
  )
  result <- summarize_with_margins(
    input, value = sum(x, na.rm = TRUE),
    parent = share_of_parent(value), total = share_of_total(value),
    .grouping = rollup(g), .check_share_source = FALSE
  ) |>
    dplyr::collect() |>
    dplyr::arrange(g)
  expect_equal(result$value, c(4, 1, 3))
  expect_identical(result$parent, c(1, 0.25, 0.75))
  expect_identical(result$total, c(1, 0.25, 0.75))
})


framed_share_facts <- function() {
  data.frame(
    fixed = rep(c("east", NA_character_), each = 4L),
    g = rep(c("a", "a", "b", "b"), 2L),
    h = rep(c("x", "y"), 4L), x = c(1, 3, 2, 6, 10, 30, 20, 60)
  )
}

expected_framed_shares <- function() {
  partition <- data.frame(
    fixed = "east",
    g = c(rep(c("a", "a", "b", "b"), 2L), "a", "b", NA_character_),
    h = c(rep(c("x", "y"), 4L), rep(NA_character_, 3L)),
    set = c(rep(1L, 4L), rep(2L, 4L), 3L, 3L, 4L),
    value = c(1, 3, 2, 6, 1, 3, 2, 6, 4, 8, 12),
    parent = c(rep(c(0.25, 0.75), 4L), 1 / 3, 2 / 3, 1),
    total = c(rep(c(1 / 12, 1 / 4, 1 / 6, 1 / 2), 2L), 1 / 3, 2 / 3, 1)
  )
  missing <- partition
  missing$fixed <- NA_character_
  missing$value <- missing$value * 10
  rbind(partition, missing)
}

expect_framed_sql_shares <- function(con) {
  facts <- framed_share_facts()
  source <- dplyr::copy_to(con, facts, "facts")
  frames <- list(c(-Inf, 0), c(0, Inf), c(-1, 1), c(0, 0), c(-Inf, Inf))
  expected <- expected_framed_shares()
  for (frame in frames) {
    input <- dbplyr::window_frame(
      dbplyr::window_order(source, dplyr::desc(.data$g)), frame[[1L]], frame[[2L]]
    )
    input_sql <- dbplyr::sql_render(input)
    for (sort in c("none", "first", "last")) {
      query <- summarize_with_margins(
        input, value = sum(.data$x, na.rm = TRUE),
        parent = share_of_parent(!!rlang::sym("value")),
        total = share_of_total(!!rlang::sym("value")),
        .by = "fixed", .grouping = rollup("g", "h", "h"), .duplicates = "keep",
        .id = "set", .margin_label = NULL, .sort = sort,
        .check_share_source = FALSE
      )
      expect_s3_class(query, "tbl_lazy")
      result <- dplyr::arrange(
        dplyr::collect(query), .data$fixed, .data$set, .data$g, .data$h
      )
      expect_equal(as.data.frame(result), expected,
                   info = paste("frame", paste(frame, collapse = ":"), sort))
      expect_type(result$parent, "double")
      expect_type(result$total, "double")
      expect_identical(dbplyr::sql_render(input), input_sql)
    }
  }
  expect_equal(as.data.frame(dplyr::collect(source)), facts)
}

test_that("SQLite framed shares retain fixed partitions and duplicate sets", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_framed_sql_shares(con)
  source <- dplyr::tbl(con, "facts")
  input <- dbplyr::window_frame(source, -1, 1)
  result <- summarize_with_margins(
    input, value = sum(x, na.rm = TRUE),
    parent = share_of_parent(value), total = share_of_total(value),
    .by = fixed, .grouping = rollup(g, h, h), .duplicates = "keep",
    .id = "set", .margin_label = NULL, .check_share_source = FALSE
  ) |>
    dplyr::collect() |>
    dplyr::arrange(fixed, set, g, h)
  expect_equal(as.data.frame(result), expected_framed_shares())
})


test_that("DuckDB framed shares retain fixed partitions and duplicate sets", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_framed_sql_shares(con)
})

test_that("share denominator frames preserve ordinary input windows", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), x = c(1, 3)), "facts"
  )
  input <- dbplyr::window_frame(
    dbplyr::window_order(source, dplyr::desc(g)), -Inf, 0
  ) |>
    dplyr::mutate(running = sum(x, na.rm = TRUE))
  ordinary <- summarize_with_margins(
    input, value = sum(running, na.rm = TRUE), .grouping = rollup(g)
  ) |>
    dplyr::collect() |>
    dplyr::arrange(g)
  result <- summarize_with_margins(
    input, value = sum(running, na.rm = TRUE),
    parent = share_of_parent(value), total = share_of_total(value),
    .grouping = rollup(g), .check_share_source = FALSE
  ) |>
    dplyr::collect() |>
    dplyr::arrange(g)
  expect_equal(ordinary$value, c(7, 4, 3))
  expect_equal(result[c("g", "value")], ordinary)
  expect_equal(result$parent, c(1, 4 / 7, 3 / 7))
  expect_equal(result$total, c(1, 4 / 7, 3 / 7))
})

test_that("framed share construction leaves source evaluation to collection", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(con, data.frame(g = "a", x = 1), "facts")
  input <- dbplyr::window_frame(
    dbplyr::window_order(source, dplyr::desc(g)), 0, Inf
  ) |>
    dplyr::mutate(x = dbplyr::sql("abs(-9223372036854775808)"))
  query <- summarize_with_margins(
    input, value = sum(x, na.rm = TRUE),
    parent = share_of_parent(value), total = share_of_total(value),
    .grouping = rollup(g), .check_share_source = FALSE
  )
  expect_s3_class(query, "tbl_lazy")
  expect_output(dplyr::show_query(query))
  expect_error(dplyr::collect(query), "integer overflow", fixed = TRUE)
})

test_that("native DuckDB shares use complete framed partitions", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), x = c(1, 3)), "facts"
  )
  input <- dbplyr::window_frame(source, 0, Inf)
  result <- summarize_with_margins(
    input, value = sum(x, na.rm = TRUE),
    parent = share_of_parent(value), total = share_of_total(value),
    .grouping = rollup(g)
  ) |>
    dplyr::collect() |>
    dplyr::arrange(g)
  expect_equal(result$value, c(4, 1, 3))
  expect_identical(result$parent, c(1, 0.25, 0.75))
  expect_identical(result$total, c(1, 0.25, 0.75))
})
