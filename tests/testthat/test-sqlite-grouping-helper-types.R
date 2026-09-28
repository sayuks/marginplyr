test_that("direct SQLite Grouping helpers stay numeric without rows", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, tibble::tibble(g = "a", v = 1),
    "empty_grouping_helpers", temporary = TRUE
  )
  empty <- dplyr::filter(source, v < 0)

  for (sort in c("none", "last")) {
    query <- summarize_with_margins(
      empty,
      bit = grouping_bit(g), mask = grouping_id(),
      ordinary = dplyr::n(),
      .by = g, .grouping = grouping_set(), .sort = sort
    )
    for (result in list(
      dplyr::collect(query),
      dplyr::collect(query, n = 0),
      dplyr::collect(dplyr::compute(query))
    )) {
      expect_s3_class(result, "tbl_df")
      expect_identical(nrow(result), 0L)
      expect_identical(names(result), c("g", "bit", "mask", "ordinary"))
      expect_identical(result$bit, integer())
      expect_identical(result$mask, integer())
      expect_identical(
        names(dplyr::select(result, dplyr::where(is.numeric))),
        c("bit", "mask")
      )
    }
  }
})

test_that("finite zero-row fetch and computed schema retain helper types", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, tibble::tibble(g = "a", v = 1),
    "finite_grouping_helpers", temporary = TRUE
  )
  query <- summarize_with_margins(
    source,
    bit = grouping_bit(g), mask = grouping_id(),
    .by = g, .grouping = grouping_set(), .sort = "last"
  )
  finite <- dplyr::collect(query, n = 0)
  expect_identical(nrow(finite), 0L)
  expect_identical(finite$bit, integer())
  expect_identical(finite$mask, integer())
  computed <- dplyr::compute(query, name = "finite_helper_output")
  expect_identical(dplyr::collect(computed, n = 0)$mask, integer())
  schema <- DBI::dbGetQuery(
    con, "PRAGMA temp.table_info('finite_helper_output')"
  )
  expect_identical(schema$name, c("g", "bit", "mask"))
  expect_true(all(grepl("INT", schema$type[2:3], fixed = TRUE)))
})

test_that("SQLite uses expanded names without reevaluating across", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, tibble::tibble(g = "a", v = 1),
    "named_grouping_helpers", temporary = TRUE
  )
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  bump <- function() {
    calls$n <- calls$n + 1L
    calls$n
  }
  build <- function(input) {
    summarize_with_margins(
      input,
      dplyr::across(
        v,
        list(
          bit = ~grouping_bit(g),
          mask = function(.x) {
            (grouping_id())
          },
          total = ~sum(.x, na.rm = TRUE),
          shift = ~grouping_id() + 1L,
          text = ~as.character(grouping_id())
        ),
        .names = "{.col}_{.fn}_{bump()}"
      ),
      .by = g, .grouping = grouping_set(), .sort = "last"
    )
  }
  query <- build(dplyr::filter(source, v < 0))
  expect_identical(calls$n, 6L)
  expected_names <- c(
    "g", "v_bit_6", "v_mask_6", "v_total_6", "v_shift_6", "v_text_6"
  )
  for (result in list(
    dplyr::collect(query),
    dplyr::collect(query, n = 0),
    dplyr::collect(dplyr::compute(query))
  )) {
    expect_s3_class(result, "tbl_df")
    expect_identical(names(result), expected_names)
    expect_identical(nrow(result), 0L)
    expect_identical(result$v_bit_6, integer())
    expect_identical(result$v_mask_6, integer())
    expect_identical(result$v_total_6, logical())
    expect_identical(result$v_shift_6, logical())
    expect_identical(result$v_text_6, logical())
  }
  expect_identical(calls$n, 6L)
  calls$n <- 0L
  populated <- dplyr::collect(build(source))
  expect_identical(calls$n, 6L)
  expect_s3_class(populated, "tbl_df")
  expect_identical(names(populated), expected_names)
  expect_identical(populated$v_bit_6, 0L)
  expect_identical(populated$v_mask_6, 0L)
  expect_identical(populated$v_total_6, 1)
  expect_identical(populated$v_shift_6, 1L)
  expect_identical(populated$v_text_6, "0")
})

test_that("single literal across lambdas declare only helper outputs", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, tibble::tibble(g = "a", v = 1),
    "single_lambda_helpers", temporary = TRUE
  )
  query <- summarize_with_margins(
    dplyr::filter(source, v < 0),
    dplyr::across(v, ~grouping_id(), .names = "mask_{.col}"),
    dplyr::across(v, function(.x) grouping_bit(g), .names = "bit_{.col}"),
    .by = g, .grouping = grouping_set()
  )
  result <- dplyr::collect(query)
  expect_identical(names(result), c("g", "mask_v", "bit_v"))
  expect_identical(result$mask_v, integer())
  expect_identical(result$bit_v, integer())
})

test_that("an omitted across function keeps ordinary selection behavior", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, tibble::tibble(g = "a", v = 1),
    "omitted_across_function", temporary = TRUE
  )
  call <- rlang::parse_expr(paste0(
    "summarize_with_margins(source, dplyr::across(v, ), ",
    ".grouping = grouping_set(g))"
  ))
  result <- dplyr::collect(eval(call))
  expect_identical(names(result), c("g", "v"))
  expect_identical(result$v, 1)
})

test_that("SQLite declares direct spellings but not enclosing expressions", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, tibble::tibble(g = "a", v = 1),
    "spelled_grouping_helpers", temporary = TRUE
  )
  query <- summarize_with_margins(
    dplyr::filter(source, v < 0),
    grouping_id(),
    qualified = (marginplyr::grouping_bit)(g),
    block = {
      (grouping_bit(g))
    },
    ordinary = 0L,
    wrapped = as.integer(grouping_id()),
    .by = g, .grouping = grouping_set(), .sort = "last"
  )
  for (result in list(
    dplyr::collect(query), dplyr::collect(dplyr::compute(query))
  )) {
    expect_identical(
      names(result),
      c("g", "grouping_id()", "qualified", "block", "ordinary", "wrapped")
    )
    expect_identical(result[["grouping_id()"]], integer())
    expect_identical(result$qualified, integer())
    expect_identical(result$block, integer())
    expect_identical(result$ordinary, logical())
    expect_identical(result$wrapped, logical())
  }
})

test_that("SQLite retains masks, identifiers, shares, and Margin order", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, tibble::tibble(region = "east", store = "x", v = 2),
    "populated_grouping_helpers", temporary = TRUE
  )
  for (label in list("Total", NULL, NA_character_)) {
    for (sort in c("none", "first", "last")) {
      query <- summarize_with_margins(
        source,
        total = sum(v, na.rm = TRUE),
        bit = grouping_bit(store), mask = grouping_id(),
        share = share_of_total(total),
        .grouping = grouping_sets(
          grouping_set(region, store), grouping_set(region),
          grouping_set(), grouping_set()
        ),
        .duplicates = "keep", .id = "sid", .margin_label = label,
        .sort = sort, .check_share_source = FALSE
      )
      result <- dplyr::collect(query)
      expected_ids <- if (sort == "first") c(3L, 4L, 2L, 1L) else 1:4
      expect_identical(
        names(result),
        c("region", "store", "sid", "total", "bit", "mask", "share")
      )
      expect_identical(result$sid, expected_ids)
      expect_identical(result$bit, c(0L, 1L, 1L, 1L)[expected_ids])
      expect_identical(result$mask, c(0L, 1L, 3L, 3L)[expected_ids])
      expect_identical(result$share, rep(1, 4L))
      expect_identical(result$total, rep(2, 4L))
      expect_identical(
        inherits(result, "tbl_df"), identical(sort, "none")
      )
      computed <- dplyr::collect(dplyr::compute(query))
      expect_identical(computed$sid, expected_ids)
      expect_identical(computed$mask, c(0L, 1L, 3L, 3L)[expected_ids])
    }
  }
})

test_that("an empty Grand total remains a real row with numeric helpers", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, tibble::tibble(g = "a", v = 1),
    "grand_total_grouping_helpers", temporary = TRUE
  )
  query <- summarize_with_margins(
    dplyr::filter(source, v < 0),
    bit = grouping_bit(g), mask = grouping_id(),
    .grouping = rollup(g), .id = "sid", .margin_label = NULL
  )
  result <- dplyr::collect(query)
  expect_identical(nrow(result), 1L)
  expect_identical(result$sid, 2L)
  expect_identical(result$bit, 1L)
  expect_identical(result$mask, 1L)
})

test_that("local and dtplyr Grouping helpers remain integer", {
  skip_if_suggest_absent("dtplyr")
  data <- tibble::tibble(g = "a", v = 1)
  for (source in list(data, dtplyr::lazy_dt(data))) {
    result <- dplyr::collect(summarize_with_margins(
      source,
      bit = grouping_bit(g), mask = grouping_id(),
      .grouping = rollup(g), .id = "sid"
    ))
    expect_identical(result$bit, c(0L, 1L))
    expect_identical(result$mask, c(0L, 1L))
    expect_identical(result$sid, c(1L, 2L))
  }
  inspected <- summarize_with_margins(
    data,
    mask = grouping_id(),
    type = local(typeof(grouping_id())),
    equal = local(identical(grouping_id(), 0L)),
    syntax = deparse1(quote(grouping_id())),
    .grouping = grouping_set(g)
  )
  expect_identical(inspected$mask, 0L)
  expect_identical(inspected$type, "integer")
  expect_identical(inspected$equal, TRUE)
  expect_identical(inspected$syntax, "grouping_id()")
})

test_that("Arrow keeps numeric direct Grouping helpers on empty input", {
  skip_if_suggest_absent("arrow")
  source <- arrow::arrow_table(tibble::tibble(g = "a", v = 1))
  query <- summarize_with_margins(
    dplyr::filter(source, v < 0),
    bit = grouping_bit(g), mask = grouping_id(),
    .by = g, .grouping = grouping_set()
  )
  result <- dplyr::collect(query)
  expect_identical(nrow(result), 0L)
  expect_identical(names(result), c("g", "bit", "mask"))
  expect_true(is.numeric(result$bit))
  expect_true(is.numeric(result$mask))
})

test_that("DuckDB keeps native Grouping helper values numeric", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  source <- dplyr::copy_to(
    con, tibble::tibble(p = 1L, g = "a", v = 1),
    "duckdb_grouping_helpers", temporary = TRUE
  )
  query <- summarize_with_margins(
    dplyr::filter(source, p < 0),
    bit = grouping_bit(g), mask = grouping_id(),
    .by = p, .grouping = rollup(g)
  )
  expect_match(dbplyr::sql_render(query), "GROUPING SETS", fixed = TRUE)
  result <- dplyr::collect(query)
  expect_identical(nrow(result), 0L)
  expect_true(is.numeric(result$bit))
  expect_true(is.numeric(result$mask))
  computed <- dplyr::collect(dplyr::compute(query))
  expect_true(is.numeric(computed$bit))
  expect_true(is.numeric(computed$mask))
})
