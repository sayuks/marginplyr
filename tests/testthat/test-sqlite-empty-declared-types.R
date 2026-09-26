test_that("SQLite empty Margin results retain declared package column types", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(month = 1L, g = "a", v = 3),
    "empty_declared_types", temporary = TRUE
  )
  empty <- dplyr::filter(source, month == 2L)

  for (label in list("Total", NULL, NA_character_)) {
    for (sort in c("none", "last", "first")) {
      query <- summarize_with_margins(
        empty,
        z = sum(v, na.rm = TRUE),
        p = share_of_parent(z),
        t = share_of_total(z),
        dplyr::across(z, share_of_total, .names = "{.col}_across"),
        .by = month,
        .grouping = rollup(g),
        .margin_label = label,
        .id = "sid",
        .sort = sort,
        .check_share_source = FALSE
      )
      expected_names <- c("month", "g", "sid", "z", "p", "t", "z_across")
      computed <- dplyr::collect(dplyr::compute(query))
      expect_identical(names(computed), expected_names)
      expect_identical(computed$sid, integer())
      expect_identical(computed$p, double())
      expect_identical(computed$t, double())
      result <- dplyr::collect(query)
      info <- paste(if (is.null(label)) "NULL" else label, sort)
      expect_identical(nrow(result), 0L, info = info)
      expect_identical(names(result), expected_names, info = info)
      expect_identical(result$month, integer(), info = info)
      expect_identical(result$g, character(), info = info)
      expect_identical(result$sid, integer(), info = info)
      expect_identical(result$p, double(), info = info)
      expect_identical(result$t, double(), info = info)
      expect_identical(result$z_across, double(), info = info)
    }
  }
})

test_that("SQLite empty driver projections do not promise aggregate types", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  raw <- DBI::dbGetQuery(con, "SELECT CAST(NULL AS REAL) AS x WHERE 0")
  expect_identical(raw$x, logical())
})

test_that("SQLite empty summary keeps root shares and standalone identifiers", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(month = 1L, g = 1L, v = 3),
    "empty_declared_root", temporary = TRUE
  )
  empty <- dplyr::filter(source, month == 2L)
  root <- summarize_with_margins(
    empty, z = sum(v), p = share_of_parent(z), t = share_of_total(z),
    .grouping = rollup(g), .id = "sid", .margin_label = NULL,
    .check_share_source = FALSE
  ) |>
    dplyr::collect()
  expect_identical(root$sid, 2L)
  expect_identical(root$p, 1)
  expect_identical(root$t, 1)

  shares_only <- summarize_with_margins(
    empty, z = sum(v), p = share_of_parent(z),
    t = share_of_total(z), .by = month, .grouping = rollup(g),
    .margin_label = NULL, .check_share_source = FALSE
  )
  expect_identical(dplyr::collect(dplyr::compute(shares_only))$p, double())
  result <- dplyr::collect(shares_only)
  expect_identical(result$p, double())
  expect_identical(result$t, double())

  query <- summarize_with_margins(
    empty, z = sum(v), .by = month, .grouping = grouping_set(g),
    .id = "sid"
  )
  computed <- dplyr::collect(dplyr::compute(query))
  expect_identical(computed$g, character())
  expect_identical(computed$sid, integer())
  result <- dplyr::collect(query)
  expect_identical(result$month, integer())
  expect_identical(result$g, character())
  expect_identical(result$sid, integer())
  expect_identical(nrow(result), 0L)
})
