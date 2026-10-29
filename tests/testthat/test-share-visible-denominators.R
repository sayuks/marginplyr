visible_share_requests <- function() {
  source <- rlang::sym("value")
  list(
    parent = rlang::exprs(parent = share_of_parent(!!source)),
    total = rlang::exprs(total = share_of_total(!!source)),
    both = rlang::exprs(
      parent = share_of_parent(!!source), total = share_of_total(!!source)
    )
  )
}

expect_visible_rollup_shares <- function(result, root_id) {
  denominator <- result$value[result$set == root_id]
  expect_length(denominator, 1L)
  for (share in intersect(c("parent", "total"), names(result))) {
    expect_equal(result[[share]][result$set != root_id],
                 result$value[result$set != root_id] / denominator)
    expect_equal(result[[share]][result$set == root_id], 1)
  }
}

test_that("dtplyr shares use their displayed stateful denominator", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  source <- dtplyr::lazy_dt(data)
  for (shares in visible_share_requests()) {
    counter <- 0L
    stamp <- function(x) {
      counter <<- counter + 1L
      as.double(counter)
    }
    query <- summarize_with_margins(
      source, value = stamp(.data$x), !!!shares,
      .grouping = rollup(g), .id = "set", .margin_label = NULL
    )
    expect_identical(counter, 0L)
    expect_s3_class(query, "dtplyr_step")
    expect_output(print(dplyr::show_query(query)))
    expect_identical(counter, 0L)
    expect_visible_rollup_shares(dplyr::collect(query), 2L)
    expect_identical(as.data.frame(dplyr::collect(source)), data)
  }
})

test_that("DuckDB shares use their displayed stateful denominator", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), x = c(1, 3)), "facts"
  )
  for (shares in visible_share_requests()) {
    DBI::dbExecute(con, "CREATE OR REPLACE SEQUENCE share_stamp")
    query <- summarize_with_margins(
      source,
      value = sum(.data$x) + dbplyr::sql("nextval('share_stamp')"),
      !!!shares,
      .grouping = rollup(g), .id = "set", .check_share_source = FALSE
    )
    expect_s3_class(query, "tbl_lazy")
    expect_output(dplyr::show_query(query))
    expect_error(DBI::dbGetQuery(con, "SELECT currval('share_stamp')"),
                 "not yet defined")
    expect_visible_rollup_shares(dplyr::collect(query), 2L)
    expect_equal(dplyr::collect(source)$x, c(1, 3))
  }
})

test_that("SQLite shares use their displayed random denominator", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), x = c(1, 3)), "facts"
  )
  for (shares in visible_share_requests()) {
    query <- summarize_with_margins(
      source,
      value = sum(.data$x) + dbplyr::sql("(ABS(RANDOM() % 1000000000) + 1.0)"),
      !!!shares,
      .grouping = rollup(g), .id = "set", .check_share_source = FALSE
    )
    expect_visible_rollup_shares(dplyr::collect(query), 2L)
    expect_equal(dplyr::collect(source)$x, c(1, 3))
  }
})

visible_partition_data <- function() {
  data.frame(
    fixed = rep(c("east", "west", NA_character_), each = 4L),
    g = rep(c("a", "a", "b", "b"), 3L),
    h = rep(c("x", "y"), 6L), x = seq_len(12L)
  )
}

expect_partition_shares <- function(result) {
  expect_equal(nrow(result), 33L)
  for (fixed in c("east", "west", NA_character_)) {
    rows <- result[vctrs::vec_equal(result$fixed, fixed, na_equal = TRUE), ]
    root <- rows$value[rows$set == 4L]
    expect_length(root, 1L)
    expect_equal(rows$total[rows$set != 4L],
                 rows$value[rows$set != 4L] / root)
    expect_equal(rows$total[rows$set == 4L], 1)
    expect_equal(rows$parent[rows$set == 4L], 1)
    expect_equal(rows$parent[rows$set == 3L],
                 rows$value[rows$set == 3L] / root)
    for (g in c("a", "b")) {
      detail <- rows$set %in% c(1L, 2L) & rows$g == g
      detail[is.na(detail)] <- FALSE
      parent <- rows$value[rows$set == 3L & !is.na(rows$g) & rows$g == g]
      expect_length(parent, 1L)
      expect_equal(rows$parent[detail], rows$value[detail] / parent)
    }
  }
}

test_that("dtplyr stateful shares retain partitions and repeated levels", {
  skip_if_suggest_absent("dtplyr")
  data <- visible_partition_data()
  source <- dtplyr::lazy_dt(data)
  counter <- 0L
  stamp <- function(x) {
    counter <<- counter + 1L
    as.double(counter)
  }
  query <- summarize_with_margins(
    source, value = stamp(.data$x),
    parent = share_of_parent(value), total = share_of_total(value),
    .by = fixed, .grouping = rollup(g, h, h), .duplicates = "keep",
    .id = "set", .margin_label = NULL
  )
  expect_identical(counter, 0L)
  expect_partition_shares(dplyr::collect(query))
  expect_identical(as.data.frame(dplyr::collect(source)), data)
})

test_that("DuckDB stateful shares retain partitions and repeated levels", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  source <- dplyr::copy_to(con, visible_partition_data(), "facts")
  DBI::dbExecute(con, "CREATE SEQUENCE share_stamp")
  query <- summarize_with_margins(
    source,
    value = sum(.data$x) + dbplyr::sql("nextval('share_stamp')"),
    parent = share_of_parent(value), total = share_of_total(value),
    .by = fixed, .grouping = rollup(g, h, h), .duplicates = "keep",
    .id = "set", .margin_label = NULL, .check_share_source = FALSE
  )
  expect_error(DBI::dbGetQuery(con, "SELECT currval('share_stamp')"),
               "not yet defined")
  expect_partition_shares(dplyr::collect(query))
})

test_that("duplicate Grand totals may differ while each self-share is one", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  counter <- 0L
  stamp <- function(x) {
    counter <<- counter + 1L
    as.double(counter)
  }
  result <- summarize_with_margins(
    data, value = stamp(x), fraction = share_of_total(value),
    .grouping = grouping_sets(grouping_set(g), grouping_set(), grouping_set()),
    .duplicates = "keep", .id = "set", .margin_label = NULL
  )
  totals <- result$set %in% c(2L, 3L)
  denominators <- result$value[totals]
  expect_identical(nrow(result), 4L)
  expect_length(unique(denominators), 2L)
  expect_identical(result$fraction[totals], c(1, 1))
  possible_shares <- lapply(denominators, function(denominator) {
    result$value[!totals] / denominator
  })
  expect_true(any(vapply(
    possible_shares, identical, logical(1), y = result$fraction[!totals]
  )))
})
